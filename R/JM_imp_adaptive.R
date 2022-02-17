#' Run a joint model with adaptively increasing the number of iterations
#' @inheritParams JointAI::JM_imp
#' @inheritParams as_inits
#' @inheritParams run_gr_check
#' @param inits_iter number of iteration used for the model that generates the
#' initial values
#' @param cc logical: should the model be run as a complete case analysis?
#' @export
JM_imp_adaptive <- function(formula, data, df_basehaz = 6,
                            n.chains = 3, n.adapt = 100, n.iter = 0, thin = 1,
                            monitor_params = c(analysis_main = TRUE),
                            auxvars = NULL,
                            timevar = NULL, refcats = NULL,
                            models = NULL, no_model = NULL,
                            assoc_type = NULL, trunc = NULL,
                            shrinkage = FALSE, ppc = TRUE, seed = NULL,
                            scale_vars = NULL, hyperpars = NULL,
                            modelname = NULL, modeldir = NULL,
                            keep_model = FALSE, overwrite = NULL,
                            quiet = TRUE, progress.bar = "text",
                            warn = TRUE, mess = TRUE,
                            keep_scaled_mcmc = FALSE, inits_iter = 200,
                            what = c("RinvD", "invD", "tau", "b"),
                            extra_iter = NULL,
                            minsize = 500L, step = 200L, subset = NULL,
                            cutoff = 1.2, prop = 0.8,
                            gr_max = 1.5, max_try = 5L, cc = FALSE,
                            ...) {

  args <- as.list(match.call()[-1L])

  if (cc) {
    args$data <- make_cc_subset(args$data, args$formula)
    data <- make_cc_subset(data, args$formula)
  }

  inits <- get_inits_JM(formula = formula[-1L], data = data,
                        inits_iter = inits_iter, n_chains = n.chains)
  args$inits <- inits

  fitted_model <- do.call(JointAI::JM_imp, args)

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






#' Add samples adaptively
#' @inheritParams JointAI::model_imp
#' @inheritParams run_gr_check
#' @export
#'
add_samples_adaptive <- function(fitted_model, extra_iter = NULL,
                                 minsize = 500L, step = 200L, subset = NULL,
                                 cutoff = 1.2, prop = 0.8,
                                 gr_max = 1.5, max_try = 5L) {


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





#' Generate initial values for a JointAI joint model
#' @param formula the model formula
#' @param data the dataset
#' @param inits_iter number of iterations
#' @param n_chains  number of chains (has to be the same as in the joint model)
#' @param seed seed value
#' @param ... additional arguments
#'
#' @export
get_inits_JM <- function(formula, data, inits_iter, n_chains, seed = NULL,
                         ...) {

  thecall <- as.list(match.call()[-1])
  thecall$formula <- str2lang(paste0(deparse(eval(thecall$formula)), collapse = ""))
  thecall$n.iter <- thecall$inits_iter
  thecall$n.chains <- thecall$n_chains

  prep <- do.call(JointAI::lme_imp, thecall)

  as_inits(prep)
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
