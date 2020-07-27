
#' Simulate data
#' @param covar_def covariate definition
#' @param data_params a list of parameters
#' @param seed a seed value
#' @export
sim_data <- function(covar_def, data_params, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  out_fun_name = paste0("sim_outcome_", data_params$response_type)

  if (!inherits(try(get(out_fun_name)), "function")) {
    errormsg("I cannot find a function %s to simulate an outcome of type %s.",
             dQuote(out_fun_name), dQuote(data_params$response_type))
  }

  input_data <- eval(covar_def, envir = data_params)

  do.call(get(out_fun_name),
          c(list(dat = input_data), data_params))
}

