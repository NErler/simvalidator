#' Run a joint model with adaptively increasing the number of iterations
#' @inheritParams JointAI::lme_imp
#' @inheritParams run_gr_check
#' @export
lme_imp_adaptive <- function(fixed, data, random,
                             n.chains = 3, n.adapt = 100, n.iter = 0, thin = 1,
                             monitor_params = c(analysis_main = TRUE), auxvars = NULL,
                             refcats = NULL,
                             models = NULL, no_model = NULL, trunc = NULL,
                             shrinkage = FALSE, ppc = TRUE, seed = NULL, inits = NULL,
                             scale_vars = NULL, hyperpars = NULL,
                             modelname = NULL, modeldir = NULL,
                             keep_model = FALSE, overwrite = NULL,
                             quiet = TRUE, progress.bar = "text",
                             warn = TRUE, mess = TRUE,
                             keep_scaled_mcmc = FALSE,
                             extra_iter = NULL,
                             minsize = 500L, step = 200L, subset = NULL,
                             cutoff = 1.2, prop = 0.8,
                             gr_max = 1.5, max_try = 5L,
                             ...) {

  args <- as.list(match.call()[-1L])

  fitted_model <- do.call(JointAI::lme_imp, args)

  check_list <- run_gr_check(fitted_model = fitted_model,
                             extra_iter = extra_iter, minsize = minsize,
                             step = step, subset = subset, cutoff = cutoff,
                             prop = prop,
                             gr_max = gr_max, max_try = max_try)

  chains <- seq_along(check_list$fitted_model$MCMC)
  if (!is.null(check_list$exclude_chains)) {
    chains <- chains[-check_list$exclude_chains]
  }

  check_list$fitted_model$MCMC <- window(check_list$fitted_model$MCMC[chains],
                                         start = check_list$strt)
  check_list$fitted_model
}




#' Return the current state of a 'JointAI' model as list to be used as
#' initial values
#' @param object an object of class 'JointAI'
#' @param what vector of node types
#'
#'
as_inits <- function(object,
                     what = c("RinvD", "invD", "tau", "b")) {

  jags_model_type <- if (inherits(object$model, "list")) {
    "list"
  } else {
    "jags"
  }

  nodes <- unlist(
    lapply(paste0("^", what, "_"),
           grep,
           x = switch(jags_model_type,
                      "list" = names(object$model[[1]]$state()[[1]]),
                      "jags" = names(object$model$state()[[1]])
           ),
           value = TRUE)
  )

  switch(jags_model_type,
         "list" = lapply(object$model, function(mod) {
           mod$state()[[1]][nodes]
         }),
         "jags" = lapply(object$model$state(), function(chain) {
           chain[nodes]
         })
  )
}
