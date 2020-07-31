#' Simulate a survival and longitudinal outcome for a joint model
#' @param data a `data.frame` containing covariate data
#' @param formula a list of model formulas for the survival and longitudinal
#'                outcomes
#' @param reg_coefs a list of regression coefficients with elements "surv" and
#'                  "long". Each of the list elements is a (named) vector.
#'                  If named, the names will be matched with the names of the
#'                  design matrix that is created from `formula` and `data`.
#' @param ranef_vcov a random effects variance-covariance matrix or a named
#'                   list of such matrices when there are more than two levels
#'                   (and names being equal to the grouping variables)
#' @param resid_sd vector of residual standard deviations for longitudinal
#'                 outcomes
#' @param type vector of type specifications for longitudinal outcomes
#' @param timevar character string giving the variable name of the time
#'                variable for the longitudinal outcome. Should be different
#'                from the name of the survival time variable.
#' @param shape_wb shape parameter of the weibull distribution
#' @param beta_Bh0 vector of coefficients for a spline specification of the
#'                 baseline hazard
#' @param mean_cens mean censoring time
#' @param basehaz_type character string specifying the type of baseline hazard:
#'                     `weibull` or `spline`
#' @param knot_range range of the knots for a spline baseline hazard
#' @param .up upper limit for the integration over the hazard
#' @param up_step step with which `.up` is increased in case of failure
#' @param .tries integer; how often is the upper limit increased when looking
#'               for the root
#' @param seed the seed value
#' @param no_obs_after_event logical; should observations of the longitudinal
#'                           outcome with times after the corresponding event/
#'                           censoring time be removed?
#' @param no_subset logical; used for "debugging"
#' @param progress_bar logical: should a progress bar be displayed
#' @param ... arguments passed to other functions
#'
#' @export
#'

sim_outcome_joint_model <- function(data, formula, reg_coefs, resid_sd,
                                    beta_Bh0 = NULL, ranef_vcov, type,
                                    timevar = "time",
                                    basehaz_type = "splines",
                                    shape_wb = 1.2, mean_cens = 30.0,
                                    .tries = 5, .up = 500L, up_step = 500L,
                                    knot_range = NULL,
                                    seed = NULL, no_obs_after_event = TRUE,
                                    no_subset = FALSE, progress_bar = FALSE,
                                    ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # model formulas
  fmla_long <- split_formula_list(check_formula_list(formula$long))
  fmla_fixed <- lapply(fmla_long$fixed, function(x) {
    as.formula(deparse(x[-2], width.cutoff = 500))
  })
  fmla_random <- remove_grouping(fmla_long$random)


  # groups
  idvars <- extract_id(fmla_long$random)
  groups <- get_groups(idvars, data)
  group_lvls <- colSums(!identify_level_relations(groups))

  if (any(group_lvls[duplicated(group_lvls)] %in% c(1, 2))) {
    errormsg("Is there a mistake in the grouping variables? There should only
             be one grouping variable for level-1 and one for level-2.")
  }


  # survival rows and subset
  surv_row <- match(unique(groups[[names(group_lvls)[which(group_lvls == 2)]]]),
                    groups[[names(group_lvls)[which(group_lvls == 2)]]])

  surv_data <- data[surv_row, ]

  fmla_surv <- split_formula_tvar(formula$surv, data = data, groups = groups)

  desgn_mat_surv_tconst <- model_matrix(fmla_surv$tconst, surv_data)

  if (basehaz_type == "spline") {
    desgn_mat_surv_tconst <- desgn_mat_surv_tconst[, -1L]
    kn <- get_knots_h0(nkn = length(beta_Bh0) - 4,
                       Time = knot_range[1]:knot_range[2],
                       gkx = gauss_kronrod()$gkx)
    kn[length(kn)] <- 100 * kn[length(kn)]
  }

  lp_surv_tconst <- desgn_mat_surv_tconst %*%
    select_coefs(reg_coefs$surv, desgn_mat_surv_tconst)



  long_out <- sim_outcome_glmm(data = data,
                               formula = formula$long,
                               reg_coefs = reg_coefs$long,
                               resid_sd = resid_sd,
                               ranef_vcov = ranef_vcov,
                               type = type, return_ranefs = TRUE)

  data = long_out$data
  ranefs = long_out$ranefs

  inv_survival <- function(t, u, i) {
    h <- function(times) {

      rows <- rep(surv_row[i], length(times))
      temp_data <- data[rows, , drop = FALSE]
      temp_data[[timevar]] <- times

      temp_data <- sim_linpred_glmm(data = temp_data,
                                    fmla_fixed = fmla_fixed,
                                    fmla_random = fmla_random,
                                    reg_coefs = reg_coefs$long,
                                    resid_sd = resid_sd,
                                    type = type,
                                    ranefs = lapply(ranefs, function(v) {
                                      lapply(v, function(lvl) {
                                        lvl[rows, , drop = FALSE]
                                      })
                                    })
      )

      desgn_mat_surv_tvar <- model_matrix(fmla_surv$tvar, temp_data)
      lp_surv_tvar <- desgn_mat_surv_tvar %*%
        select_coefs(reg_coefs$surv, desgn_mat_surv_tvar)


      if (basehaz_type == "weibull") {
        exp(log(shape_wb) + (shape_wb - 1L) * log(times) +
              as.vector(lp_surv_tconst) + as.vector(lp_surv_tvar))
      } else if (basehaz_type == "spline") {
        desgn_mat_basehaz <- splines::splineDesign(kn, times, ord = 4L,
                                                   outer.ok = TRUE)

        exp(c(desgn_mat_basehaz %*% beta_Bh0) +
              as.vector(lp_surv_tconst)[i] + as.vector(lp_surv_tvar))
      }
    }
    integrate(h, lower = 0L, upper = t, subdivisions = 500L,
              rel.tol = .Machine$double.eps^0.2)$value + log(u)
  }

  u <- runif(length(surv_row))
  true_times <- nr_tries <- numeric(length(surv_row))

  if (progress_bar) {
    pb <- txtProgressBar(char = "*", width = 60, style = 3)
  }
  for (i in seq_along(surv_row)) {
    if (progress_bar) {
      setTxtProgressBar(pb, i/length(surv_row))
    }
    up <- .up
    tries <- 1
    root <- try(uniroot(inv_survival,
                        interval = c(1.0e-05, up), u = u[i], i = i)$root, TRUE)

    while (inherits(root, "try-error") && tries < .tries) {
      tries <- tries + 1L
      up <- up + up_step
      root <- try(uniroot(inv_survival,
                          interval = c(1.0e-05, up),
                          u = u[i], i = i)$root, TRUE)
    }
    true_times[i] <- if (!inherits(root, "try-error")) {
      root
    } else if (tries == .tries) {
      Inf
    } else {
      NA
    }
    nr_tries[i] <- tries
  }
  if (progress_bar) {
    close(pb)
  }
  # simulate censoring times from an exponential distribution, and calculate the
  # observed event times, i.e., min(true event times, censoring times)
  cens_times <- runif(length(true_times), 0L, 2L * mean_cens)

  surv_data[, all.vars(formula$surv[[2L]])] <-
    cbind(pmin(true_times, cens_times),
          factor(true_times <= cens_times))

  # merge the survival outcome with the longitudinal data
  if (no_subset) {
    jm_data <- merge(subset(surv_data,
                            select = c(idvars, all.vars(formula$surv[[2L]]))),
                     data)
  } else {
    jm_data <- merge(subset(surv_data, !is.na(true_times),
                            select = c(idvars, all.vars(formula$surv[[2L]]))),
                     data)
  }


  if (no_obs_after_event) {
    jm_data <- jm_data[jm_data[[timevar]] <=
                         jm_data[[all.vars(formula$surv[[2L]])[1]]],,
                       drop = FALSE]
  }
  attr(jm_data, "true_times") <- true_times
  attr(jm_data, "cens_times") <- cens_times
  attr(jm_data, "nr_tries") <- nr_tries
  attr(jm_data, "no_obs_after_event") <- no_obs_after_event
  attr(jm_data, "timevar") <- timevar
  attr(jm_data, "size_orig") <- ivapply(groups, function(x) length(unique(x)))
  # attr(jm_data, "lpconst") <- lp_surv_tconst

  jm_data
}
