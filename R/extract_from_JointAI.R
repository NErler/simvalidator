#' Extract the posterior mean of the random effects variance-covariance matrices
#' from an object of class 'JointAI'
#' @param object object of class 'JointAI'
#' @export
extract_rd_vcov <- function(object) {
  coefs <- unlist(unname(coef(object)))

  nlapply(object$Mlist$idvar, function(lvl) {
    vcov_list <- nlapply(names(coef(object)), function(var) {
      vec <- coefs[grepl(paste0("^D_", var, "_", lvl, "\\["),
                         names(coefs))]

      if (length(vec) > 0) {
        pos <- do.call(rbind,
                       strsplit(gsub("^[[:print:]]+\\[|]$", "", names(vec)),
                                split = ",")
        )

        pos <- data.frame(apply(pos, 2, as.numeric))
        pos$name <- names(vec)

        mat <- matrix(nrow = max(pos$X1),
                      ncol = max(pos$X1))

        for (i in seq_len(nrow(pos))) {
          mat[pos$X1[i], pos$X2[i]] <- vec[pos$name[i]]
        }

        mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
        structure(
          as.matrix(Matrix::nearPD(mat)$mat),
          "mat_name" = unique(gsub("\\[[[:print:]]+", "", pos$name))
        )
      }
    })

    make_mv_vcov(Filter(Negate(is.null), vcov_list))
  })
}



#' Extract the posterior mean of the regression coefficients from an object of
#' class 'JointAI'
#' @param object object of class 'JointAI'
#' @export
#'
extract_reg_coefs <- function(object) {

  coefs <- coef(object)

  params <- JointAI::parameters(object)

  nlapply(names(coefs), function(vec) {
    coefs[[vec]][
      na.omit(params$varname[params$outcome == JointAI::clean_survname(vec)])
    ]
  })
}

#' Extract the posterior mean of the regression coefficients for a spline
#' specification of the baseline hazard
#'
#' @param object object of class 'JointAI'
#'
#' @export
extract_basehaz <- function(object) {
  coefs <- coef(object)
  basehaz_pars <- lapply(coefs, function(vec) {
    vec[grep("^beta_Bh0_", names(vec))]
  })
  basehaz_pars[ivapply(basehaz_pars, length) > 0]
}

#' Extract the posterior mean of the residual standard deviations from an object
#' of class 'JointAI'
#' @param object object of class 'JointAI'
#' @export
extract_resid_sd <- function(object) {
  coefs <- coef(object)

  resid_sd <- nlapply(names(coefs), function(k) {
    unname(coefs[[k]][paste0("sigma_", k)])
  })

  unlist(resid_sd[!is.na(resid_sd)])
}

#' Extract the model types of longitudinal variables from an object of class
#' 'JointAI'
#' @param object object of class 'JointAI'
#' @export
extract_types <- function(object) {
  modeltypes <- cvapply(object$models, JointAI::get_modeltype)
  cvapply(object$models[modeltypes == "glmm"], JointAI::get_family)
}


#' Extract the knots used for the spline specification of the baseline hazard
#' from a model of class 'JointAI'
#' @param object object of class 'JointAI'
#' @export
extract_basehaz_knots <- function(object) {
  survival_times <- object$data_list[[
    object$info_list[[1]]$resp_mat[1]
  ]][, object$info_list[[1]]$resp_col[1]]

  get_knots_h0(nkn = object$Mlist$df_basehaz - 4L,
               Time = survival_times,
               gkx = gauss_kronrod()$gkx)
}


#' Extract the mean censoring time form an object of class 'JointAI'
#' @param object object of class 'JointAI'
#' @export
extract_mean_cens <- function(object) {
  survival_times <- object$data_list[[
    object$info_list[[1]]$resp_mat[1]
  ]][, object$info_list[[1]]$resp_col[1]]

  censoring <- object$data_list[[
    object$info_list[[1]]$resp_mat[2]
  ]][, object$info_list[[1]]$resp_col[2]]

  mean(survival_times[which(censoring == 0)])
}


#' Extract parameters necessary to simulate an outome from an object of class
#' 'JointAI'
#'
#' This function obtains the `response_type` (from the `analysis_type`),
#' `formula`, `reg_coefs` (`extract_reg_coefs()`), `type`s (`extract_types()`),
#' baseline hazard coefficients and knots (`extract_basehaz()` and
#' `extract_basehaz_knots()`), mean censoring time (`extract_mean_cens()`),
#' upper limit of where to search for the event time (`.up`) and the step
#' size with which to increase that upper limit (`up_step`).
#'
#' @param object object of class 'JointAI'
#' @export
extract_outcome_pars <- function(object) {

  response_type <- switch(object$analysis_type,
                          "JM" = "joint_model",
                          "lme" = "glmm",
                          errormsg("Model type unknown."))

  l <- list(
    response_type = response_type,
    formula = formula(object),
    reg_coefs = extract_reg_coefs(object),
    type = extract_types(object),
    ranef_vcov = extract_rd_vcov(object),
    resid_sd = extract_resid_sd(object),
    N = NA
  )

  if (response_type %in% c("joint_model", "survival")) {
    l$basehaz_type = "spline"
    l$other_pars = list(
      beta_Bh0 = extract_basehaz(object),
      knots = extract_basehaz_knots(object)
    )
    l$mean_cens = extract_mean_cens(object)
    l$.up = max(extract_basehaz_knots(object))
    l$up_step = extract_mean_cens(object)
  }

  l
}


#' Extract parameters necessary to simulate covariate data from an object of
#' class 'JointAI'
#' @param object object of class 'JointAI'
#' @param timevar name of the time variable, is there is one
#' @export
extract_covar_pars <- function(object, timevar = NULL) {

  idvars <- names(object$Mlist$groups)
  data <- object$data

  data_lvls <- cvapply(data, check_varlevel, groups = object$Mlist$groups)

  covars <- setdiff(JointAI::all_vars(JointAI::remove_lhs(object$fixed)),
                    names(object$fixed))

  covars <- nlapply(idvars, function(lvl) {
    intersect(covars, names(data_lvls)[data_lvls == lvl])
  })

  data_list <- nlapply(idvars, function(lvl) {
    data[match(unique(object$Mlist$groups[[lvl]]), object$Mlist$groups[[lvl]]),
         covars[[lvl]], drop = FALSE]
  })

  cor_mat <- lapply(data_list, function(df) {
    if (ncol(df) > 1) {
      cor(as.data.frame(lapply(df, as.numeric)),
          use = 'pair', method = "spearman")
    }
  })

  means <- lapply(data_list, function(df) {
    nvapply(df, function(x) {
      if (inherits(x, "factor")) {
        0
      } else {
        mean(x, na.rm = TRUE)
      }
    })
  })


  variances <- lapply(data_list, function(df) {
    nvapply(df, function(x) {
      if (inherits(x, "factor")) {
        1
      } else {
        var(x, na.rm = TRUE)
      }
    })
  })

  vcov <- Map(make_vcov, variances, cor_mat)

  refs <- object$Mlist$refs[intersect(unlist(covars), names(object$Mlist$refs))]

  probs <- nlapply(names(refs), function(k) {
    if (inherits(unlist(unname(data_list), recursive = FALSE)[[k]], "factor")) {
      prop.table(table(unlist(unname(data_list), recursive = FALSE)[[k]]))
    }
  })

  ordered <- lvapply(refs, attr, "ordered")


  timevar_pars <- if (!is.null(object$Mlist$timevar)) {
    subj_lvl <- names(object$Mlist$group_lvls)[object$Mlist$group_lvls == 2]
    tv <- split(data[, object$Mlist$timevar],
                object$Mlist$groups[[subj_lvl]])

    list(name = object$Mlist$timevar,
         distr = "unif",
         length = quantile(nvapply(tv, length), 0.9, names = FALSE),
         min = quantile(nvapply(tv, min), 0.1, names = FALSE),
         max = quantile(nvapply(tv, max), 0.9, names = FALSE) * 1.5
      )
  } else if (!is.null(timevar)) {
    tv <- object$data_list[[object$Mlist$Mlvls[[timevar]]]][, timevar]
    multiply <- max(tv, na.rm = TRUE)
    tv <- tv/max(tv, na.rm = TRUE)
    tv[tv == 0] <- 1e-10
    tv[tv == 1] <- 1 - 1e-10

    list(name = timevar,
         distr = "beta",
         length = quantile(nvapply(tv, length), 0.9, names = FALSE),
         multiply = multiply,
         params = fitdistrplus::fitdist(tv, "beta", method = "mme")$estimate
    )
  }


  list(means = means, vcov = vcov, probs = probs, ordered = ordered,
       timevar_pars = timevar_pars,
       group_lvls = object$Mlist$group_lvls)
}


#' Create a simulation scenario from a JointAI object
#'
#' Extracts parameters needed to simulate covariates (`extract_covar_pars()`)
#' and the parameters needed to simulate the corresponding outcome(s)
#' (`extract_outcome_pars()`).
#' @param object an object of class "JointAI"
#' @param file path and file name where the extracted information should be
#'   saved to.
#' @param timevar name of the time variable, is there is one
#'
#' @export
create_simulation_scenario <- function(object, file, timevar = NULL) {

  if (inherits(object, "character")) {
    object_path <- object
    object_name <- load(object_path)
    object <- get(object_name)
  }

  if (!inherits(object, "JointAI")) {
    errormsg(
      "%s should be an object of class %s or a path from which such an
    object can be loaded.",
      dQuote("object"),
      sQuote("JointAI")
    )
  }

  outcome_pars <- extract_outcome_pars(object)
  covar_pars <- extract_covar_pars(object, timevar = timevar)

  save(outcome_pars, covar_pars, file = file)
}

