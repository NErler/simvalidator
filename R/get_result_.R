
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

  gr_crit <- JointAI::GR_crit(fitted_model, subset = subset,
                              autoburnin = FALSE,
                              multivariate = FALSE)[[1]][, 2]
  mce <- JointAI::MC_error(fitted_model,
                           subset = subset)$data_scale[, "MCSE/SD"]

  pars <- JointAI::parameters(fitted_model)
  pars$varname[is.na(pars$varname)] <- pars$coef[is.na(pars$varname)]

  coefs <- coef(fitted_model)
  cis <- confint(fitted_model)

  res <- Reduce(merge,
                list(
                  data.frame(
                    seed = seed,
                    time = sum(as.numeric(fitted_model$comp_info$duration,
                                          units = "hours")),
                    type = 'JointAI',
                    n_iter = nrow(fitted_model$MCMC[[1]]),
                    n_chain = length(fitted_model$MCMC)
                  ),
                  do.call(rbind,
                          lapply(names(coefs), function(out) {
                            data.frame(outcome = JointAI::clean_survname(out),
                                       variable = names(coefs[[out]]),
                                       Mean = coefs[[out]]
                            )
                          })
                  ),
                  do.call(rbind,
                          lapply(names(cis), function(out) {
                            data.frame(outcome = JointAI::clean_survname(out),
                                       variable = rownames(cis[[out]]),
                                       cis[[out]],
                                       check.names = FALSE
                            )
                          })
                  ),
                  match_crit(pars, gr_crit, name = "GR-crit"),
                  match_crit(pars, mce, name = "MCE/SD")
                )
  )


  attr(res, "pkg_version") <- fitted_model$comp_info$JointAI_version
  attr(res, "future") <- fitted_model$comp_info$future
  attr(res, "R.version") <- fitted_model$comp_info$sessionInfo$R.version$version.string
  attr(res, "platform") <- fitted_model$comp_info$sessionInfo$platform

  res
}



match_crit <- function(parameters, crit, name) {
  rows <- if (length(unique(parameters$outcome)) == 1) {
    match(parameters$varname, names(crit))
  } else {
    match1 <- match(paste0(parameters$outcome, ": ",
                           parameters$varname), names(crit))
    match2 <- match(parameters$coef, names(crit))
    match1[is.na(match1)] <- match2[is.na(match1)]
    match1
  }

  df <- data.frame(outcome = parameters$outcome[rows],
                   variable = parameters$varname[rows])
  df[[name]] <- crit

  df
}
