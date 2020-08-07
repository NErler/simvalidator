
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
#' models, the true regression coefficients (`regcoef`) and simulation
#' evaluation measures (bias, relative bias, coverage of the CI, CI width and
#' MSE) per simulated dataset.
#' @param object object of class simulation_result
#' @export
get_res_df <- function(object) {
  regcoef_df <- data.frame(variable = names(object$outcome_pars$reg_coefs),
                           regcoef = object$outcome_pars$reg_coefs)

  res_df <- rbind_df_list(lapply(object$sim_res, "[[", "res"))

  res_df <- merge(res_df, regcoef_df, all.x = TRUE)

  res_df$bias <- res_df$Mean - res_df$regcoef
  res_df$relbias <- res_df$bias/res_df$regcoef
  res_df$covrg <- res_df$`2.5%` < res_df$regcoef & res_df$`97.5%` > res_df$regcoef
  res_df$CIwidth <- res_df$`97.5%` - res_df$`2.5%`

  res_df
}
