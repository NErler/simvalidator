#' Simulate a survival outcome
#' @param data a `data.frame` containing covariate data
#' @param formula the model formula
#' @param reg_coefs a vector of regression coefficients. If named, the names
#'                  will be matched with the names of the design matrix that
#'                  is created from `formula` and `data`.
#' @param shape_wb parameter of the Wishart distribution
#' @param mean_cens mean censoring time
#' @param .tries integer; how often is the upper limit increased when looking
#'               for the root
#' @param seed the seed value
#' @param ... arguments passed to other functions
#'
sim_outcome_survival <- function(data, formula, reg_coefs,
                                 basehaz_type = "spline",
                                 beta_Bh0 = NULL, shape_wb = NULL,
                                 knot_range = NULL,
                                 mean_cens = 30.0,
                                 .tries = 5L, .up = 5000L,
                                 step_up = 5000L, seed = NULL,
                                 add_info = NULL, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  desgn_mat <- model_matrix(formula[-2], data = data)
  reg_coefs <- check_coef_mat(reg_coefs, desgn_mat)



  if (basehaz_type == "spline") {
    desgn_mat <- desgn_mat[, -1L]
    kn <- get_knots_h0(nkn = length(beta_Bh0) - 4,
                       Time = knot_range[1]:knot_range[2],
                       gkx = gauss_kronrod()$gkx)
    kn[length(kn)] <- 100 * kn[length(kn)]

  }


  # linear predictor
  lin_pred <- as.vector(desgn_mat %*% reg_coefs[colnames(desgn_mat)])



  inv_survival <- function(t, u, i) {
    h <- function(times) {

      if (basehaz_type == "weibull") {
        exp(log(shape_wb) + (shape_wb - 1L) * log(times) + lin_pred[i])
      } else if (basehaz_type == "spline") {
        desgn_mat_basehaz <- splines::splineDesign(kn, times, ord = 4L,
                                                   outer.ok = TRUE)

        exp(c(desgn_mat_basehaz %*% beta_Bh0) + lin_pred[i])
      }
    }
    integrate(h, lower = 0, upper = t)$value + log(u)
  }


  u <- runif(nrow(data))
  true_times <- nr_tries <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    up <- .up
    tries <- 1
    root <- try(uniroot(inv_survival,
                        interval = c(1.0e-05, up), u = u[i], i = i)$root, TRUE)
    while (inherits(root, "try-error") && tries < .tries) {
      tries <- tries + 1L
      up <- up + step_up
      root <- try(uniroot(inv_survival, interval = c(1.0e-05, up),
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

  na_indicator <- !is.na(true_times)
  true_times <- true_times[na_indicator]
  data <- data[na_indicator, , drop = FALSE]

  # simulate censoring times from an exponential distribution, and calculate the
  # observed event times, i.e., min(true event times, censoring times)
  cens_times <- runif(nrow(data), 0L, 2L * mean_cens)
  data[, all.vars(formula[[2]])] <- cbind(pmin(true_times, cens_times),
                                          as.numeric(true_times <= cens_times))

  if (add_info) {
    data$true_times <- true_times
    data$cens_times <- cens_times
    data$lin_pred <- lin_pred
  }

  attr(data, "true_times") <- true_times
  attr(data, "cens_times") <- cens_times
  attr(data, "nr_tries") <- nr_tries
  attr(data, "size_orig") <- length(lin_pred)

  data
}
