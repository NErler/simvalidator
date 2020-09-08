
rbind_df_list <- function(df_list) {
  list_by_model <- lapply(seq_along(df_list[[1]]), function(k) {
    do.call(rbind, lapply(df_list, "[[", k))
  })

  colnames <- unique(unlist(lapply(list_by_model, colnames)))

  do.call(rbind,
          lapply(list_by_model, function(x) {
            x[, setdiff(colnames, colnames(x))] <- NA
            x[, colnames]
          })
  )
}


#' Convert the simulation results into a data.frame
#' The resulting `data.frame` will contain the simulation results from all
#' models, the true regression coefficients (`true_param`) and simulation
#' evaluation measures (bias, relative bias, coverage of the CI, CI width and
#' MSE) per simulated dataset.
#' @param object object of class simulation_result
#' @export
get_res_df <- function(object) {
  res_df <- rbind_df_list(lapply(object$sim_res, function(x) {
    x[[1]]$res
  }))


  true_param_df <- if (is.list(object$outcome_pars$reg_coefs)) {
    do.call(rbind,
            lapply(names(object$outcome_pars$reg_coefs), function(out) {
              data.frame(outcome = unname(JointAI::clean_survname(out)),
                         variable = names(object$outcome_pars$reg_coefs[[out]]),
                         true_param = object$outcome_pars$reg_coefs[[out]]
              )
            })
    )
  } else {
    data.frame(variable = names(object$outcome_pars$reg_coefs),
               true_param = object$outcome_pars$reg_coefs)
  }

  true_param_df <- rbind(true_param_df,
                         get_otherpars_df(object$outcome_pars$other_pars,
                                          res_df),
                         get_vcov_df(object$outcome_pars$ranef_vcov, res_df),
                         get_resid_sd_df(object$outcome_pars$resid_sd, res_df)
  )

  res_df <- merge(res_df, true_param_df, all = TRUE)

  res_df$bias <- res_df$Mean - res_df$true_param
  res_df$relbias <- res_df$bias/res_df$true_param
  res_df$covrg <- res_df$`2.5%` < res_df$true_param & res_df$`97.5%` > res_df$true_param
  res_df$CIwidth <- res_df$`97.5%` - res_df$`2.5%`

  res_df
}


get_otherpars_df <- function(other_pars, res_df)
  do.call(rbind,
          lapply(other_pars, function(p) {
            while (inherits(p,  "list")) {
              p <- unlist(unname(p), recursive = FALSE)
            }
            if (any(names(p) %in% res_df$variable)) {

              data.frame(outcome = res_df$outcome[match(names(p), res_df$variable)],
                         variable = names(p),
                         true_param = p)
            }
          })
  )


get_vcov_df <- function(ranef_vcov, res_df) {
  do.call(rbind,
          lapply(ranef_vcov, function(vcov) {
            nam <- get_vcov_names(vcov)
            data.frame(outcome = res_df$outcome[match(nam, res_df$variable)],
                       variable = nam,
                       true_param = vcov[lower.tri(vcov, diag = TRUE)]
            )
          })
  )
}

get_vcov_names <- function(vcov) {
  unlist(
    lapply(seq_len(ncol(vcov)), function(i) {
      cvapply(seq(i, ncol(vcov), 1), function(j) {
        paste0(attr(vcov, "mat_name"), "[", i, ",", j, "]")
      })
    })
  )
}


get_resid_sd_df <- function(resid_sd, res_df) {
  nam <- paste0("sigma_", names(resid_sd))
  data.frame(outcome = res_df$outcome[match(nam, res_df$variable)],
             variable = nam,
             true_param = resid_sd)
}
