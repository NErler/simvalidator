
#' Extract a summary of the model results from a standard model
#' This function uses `coef()` and `confint()`.
#' @param fitted_model a fitted model object
#' @param type optional character string specifying the type (to identify
#'             what type of model the results belong to later in the visualization
#'             and description of results)
#' @param seed optional (but) suggested seed value. Will be used when called
#'             from within `run_models()`.
#' @param scen optional name of scenario
#' @param ... optional additional arguments for compatibility with other
#'            `get_result_<...>` functions
#' @export
get_result_default <- function(fitted_model, type = NA, seed = NA, scen = NA,
                               ...) {
  outcome <- as.character(formula(fitted_model)[[2]])

  sigma <- if (family(fitted_model)$family %in% c("gaussian", "Gamma")) {
    setNames(sigma(fitted_model), paste0("sigma_", outcome))
  }

  res <- data.frame(seed = seed,
                    scen = scen,
                    outcome = outcome,
                    type = type,
                    variable = names(c(coef(fitted_model), sigma)),
                    Mean = c(coef(fitted_model), sigma),
                    rbind(confint(fitted_model),
                          if (!is.null(sigma)) c(NA, NA)),
                    check.names = FALSE)
  colnames(res) <- gsub(" %", "%", colnames(res))
  res
}





#' Extract a summary of the model results from a lme4 model
#' This function uses `fixef()` and `confint()`.
#' @param fitted_model a fitted model object
#' @param type optional character string specifying the type (to identify
#'             what type of model the results belong to later in the visualization
#'             and description of results)
#' @param ci_method method passed to `confint()`
#' @param seed optional (but) suggested seed value. Will be used when called
#'             from within `run_models()`.
#' @param ... optional additional arguments for compatibility with other
#'            `get_result_<...>` functions
#' @param scen optional name of scenario
#' @export
get_result_lme4 <- function(fitted_model, type = NA, seed = NA, scen = NA,
                            ci_method = "Wald", ...) {

  outcome <- as.character(formula(fitted_model)[[2]])

  sigma <- if (family(fitted_model)$family %in% c("gaussian", "Gamma")) {
    sigma(fitted_model)
  }

  parm <- c(names(lme4::fixef(fitted_model)),
           if (!is.null(sigma)) {
             ".sigma"
           })

  rd_vcov <- lapply(names(lme4::VarCorr(fitted_model)), function(lvl) {
    rd_vcov <- lme4::VarCorr(fitted_model)[[lvl]]

    nam <- lapply(1:nrow(rd_vcov), function(i) {
      paste0("D_", outcome, "_", lvl, "[", i, ",", i:ncol(rd_vcov), "]")
    })

    rd_vcov <- rd_vcov[lower.tri(rd_vcov, diag = TRUE)]
    names(rd_vcov) <- unlist(nam)
    rd_vcov
  })

  nam <- gsub("^.sigma$", paste0("sigma_", outcome),
              c(parm, unlist(lapply(rd_vcov, names)))
  )


  cis <- try(lme4::confint.merMod(fitted_model,
                                  parm = parm,
                                  method = ci_method)[parm, ], silent = TRUE)
  if (inherits(cis, "try-error")) {
    cis <- matrix(data = NA, nrow = length(parm), ncol = 2,
                  dimnames = list(NULL, c("2.5%", "97.5%")))
  }



  res <- data.frame(
    seed = seed,
    scen = scen,
    type = type,
    variable = nam,
    outcome = outcome,
    Mean = c(lme4::fixef(fitted_model),
             if (!is.null(sigma))
               sigma,
             unlist(rd_vcov)),
    cis[match(gsub(paste0("sigma_", outcome), ".sigma", nam),
              rownames(cis)),],
    check.names = FALSE
  )
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
#' @param scen optional name of scenario
#' @export
get_result_JointAI <- function(fitted_model, seed = NA, outcome = 1L,
                               subset = NULL, scen = NA, type = NULL, ...) {

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
                    scen = scen,
                    time = sum(as.numeric(fitted_model$comp_info$duration,
                                          units = "hours")),
                    type = 'JointAI',
                    n_iter = nrow(fitted_model$MCMC[[1]]),
                    n_chain = length(fitted_model$MCMC)
                  ),
                  do.call(rbind,
                          lapply(names(coefs), function(out) {
                            data.frame(outcome = unname(JointAI::clean_survname(out)),
                                       variable = names(coefs[[out]]),
                                       Mean = coefs[[out]]
                            )
                          })
                  ),
                  do.call(rbind,
                          lapply(names(cis), function(out) {
                            data.frame(outcome = unname(JointAI::clean_survname(out)),
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
