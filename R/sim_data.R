
#' Simulate data
#' @param covar_def list with covariate definitions
#' @param outcome_pars a list of parameters defining the outcome (model)
#' @param data_seed a seed value
#' @export
sim_data <- function(covar_def, outcome_pars, data_seed = NULL) {

  out_fun_name = paste0("sim_outcome_", outcome_pars$response_type)

  # check if the function to simulate the outcome exists
  if (!inherits(try(get(out_fun_name)), "function")) {
    errormsg("I cannot find a function %s to simulate an outcome of type %s.",
             dQuote(out_fun_name), dQuote(outcome_pars$response_type))
  }

  input_data <- do.call(covar_def, c(outcome_pars, seed = data_seed))

  do.call(get(out_fun_name),
          c(list(data = input_data), seed = NULL,
            outcome_pars, outcome_pars$other_pars)
  )
}

