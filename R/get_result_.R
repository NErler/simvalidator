
#' Extract a summary of the model results from a standard model
#' This function uses `coef()` and `confint()`.
#' @param fitted_model a fitted model object
#' @param type optional character string specifying the type (to identify
#'             what type of model the results belong to later in the visualization
#'             and description of results)
#' @param seed optional (but) suggested seed value. Will be used when called
#'             from within `run_models()`.
#' @param ... optional additional arguments for compatibility with other
#'            `get_result_<...>` functions
#' @export
get_result_default <- function(fitted_model, type = NA, seed = NA, ...) {
  res <- data.frame(seed = seed,
                    type = type,
                    variable = names(coef(fitted_model)),
                    Mean = coef(fitted_model),
                    confint(fitted_model),
                    check.names = FALSE)
  colnames(res) <- gsub(" %", "%", colnames(res))
  res
}


#' Extract a summary of the model results from a JointAI model
#' This function uses `coef()` and `confint()`.
#' @param fitted_model a fitted model object
#' @param seed optional (but) suggested seed value. Will be used when called
#'             from within `run_models()`.
#' @param outcome integer identifying for which outcome the results should be
#'                extracted
#' @param ... optional additional arguments for compatibility with other
#'            `get_result_<...>` functions
#' @param subset subset specification of `JointAI::coef()`,
#'               `JointAI::confint()`, `JointAI::GR_crit` and
#'               `JointAI::MC_error()`
#' @export
get_result_JointAI <- function(fitted_model, seed = NA, outcome = 1L,
                               subset = NULL, ...) {

  res <- data.frame(
    seed = seed,
    time = as.numeric(fitted_model$comp_info$duration, units = "hours"),
    type = 'JointAI',
    n_iter = nrow(fitted_model$MCMC[[1]]),
    n_chain = length(fitted_model$MCMC),
    variable = names(coef(fitted_model, subset = subset)[[outcome]]),
    Mean = coef(fitted_model, subset = subset)[[outcome]],
    confint(fitted_model, subset = subset)[[outcome]],
    "GR-crit" = JointAI::GR_crit(fitted_model, subset = subset,
                                 autoburnin = FALSE,
                                 multivariate = FALSE)[[1]][, 2],
    "MCE/SD" = JointAI::MC_error(fitted_model,
                                 subset = subset)$data_scale[, "MCSE/SD"],
    check.names = FALSE
  )

  attr(res, "pkg_version") <- fitted_model$comp_info$JointAI_version
  attr(res, "future") <- fitted_model$comp_info$future
  attr(res, "R.version") <- fitted_model$comp_info$sessionInfo$R.version$version.string
  attr(res, "platform") <- fitted_model$comp_info$sessionInfo$platform

  res
}
