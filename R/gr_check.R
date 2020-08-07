
#' Run a JointAI model with flexible number of iterations
#' @param fun JointAI model function
#' @param formula the model formula
#' @param data a `data.frame`
#' @param seed the seed value
#' @param model_args a list of arguments passed to `fun`
#' @param gr_check_args a list of arguments to be passed to the gr check
#' @param ... currently not used
#' @export
run_jointai_flex <- function(fun, formula, data, seed = NULL, model_args,
                             gr_check_args, ...) {

  fitted_model <- do.call(fun,
                          set_args(fun = fun,
                                   args = c(list(formula = formula,
                                                 data = data),
                                            seed = seed,
                                            model_args)
                          )
  )

  fit <- do.call(run_gr_check,
                 set_args(fun = run_gr_check,
                          args = c(list(fitted_model = fitted_model),
                                   gr_check_args))
  )


  chains <- seq_along(fit$fitted_model$MCMC)
  if (!is.null(fit$exclude_chains)) {
    chains <- chains[-fit$exclude_chains]
  }

  fit$fitted_model$MCMC <- window(fit$fitted_model$MCMC[chains],
                                        start = fit$strt)
  fit$fitted_model
}



#' Check the Gelman-Rubin criterion for a 'JointAI' object and add samples if
#' necessary
#' @param fitted_model a object of class 'JointAI'
#' @param extra_iter number of iterations that should be added to the model if
#'   the Gelman-Rubin criterion is too large
#' @param minsize the minimum number of iterations to be considered
#' @param step the step size in which iterations are omitted as burn-in
#' @param subset subset of parameters on which the Gelman-Rubin criterion should
#'   be evaluated. Follows the logic used in **JointAI**
#' @param cutoff the cut-off used for the Gelman Rubin criterion
#' @param prop proportion of parameters that need to be below the `cutoff`
#' @param gr_max maximum allowed value for the Gelman-Rubin criterion
#' @param max_try maximum number of runs of `JointAI::add_samples()`
#'
#' @export
run_gr_check <- function(fitted_model, extra_iter = NULL, minsize = 500L,
                         step = 200L, subset = NULL, cutoff = 1.2, prop = 0.8,
                         gr_max = 1.5, max_try = 5L) {

  if (is.null(extra_iter)) {
    extra_iter <- fitted_model$mcmc_settings$n.iter
  }

  # check convergence
  gr_crit <- check_gr_crit(fitted_model, minsize = minsize, step = step,
                           subset = subset, cutoff = cutoff, prop = prop)

  if (!is.na(gr_crit)) {
    strt <- gr_crit
    exclude_chains <- NULL
  } else {
    # check Gelman-Rubin criterion while excluding one chain at a time
    gr_crit_loo <- check_gr_crit_loo(fitted_model, minsize = minsize,
                                     step = step, subset = subset,
                                     cutoff = cutoff, prop = prop)

    if (any(!is.na(gr_crit_loo))) {
      strt <- min(gr_crit_loo, na.rm = TRUE)
      exclude_chains <- which.min(gr_crit_loo)
    } else {
      # if the chains have not yet converged, add more iterations
      counter <- 0
      while (counter < max_try & (is.na(gr_crit) & all(is.na(gr_crit_loo)))) {
        fitted_model <- JointAI::add_samples(fitted_model, n.iter = extra_iter)

        # check the Gelman-Rubin criterion for all chains
        gr_crit <- check_gr_crit(fitted_model, minsize = minsize, step = step,
                                 subset = subset, cutoff = cutoff, prop = prop)

        if (!is.na(gr_crit)) {
          strt <- gr_crit
          exclude_chains <- NULL
        } else {
          # check the Gelman-Rubin criterion leaving out one chain at a time
          gr_crit_loo <- check_gr_crit_loo(fitted_model, minsize = minsize,
                                           step = step, subset = subset,
                                           cutoff = cutoff, prop = prop)

          if (any(!is.na(gr_crit_loo))) {
            strt <- min(gr_crit_loo, na.rm = TRUE)
            exclude_chains <- which.min(gr_crit_loo)
          } else {
            strt <- start(fitted_model$MCMC)
            exclude_chains <- NULL
          }
        }
        counter <- counter + 1
      }
    }
  }

  list(fitted_model = fitted_model,
       strt = strt,
       exclude_chains = exclude_chains)
}






check_gr_crit <- function(model, minsize = 500L, step = 200L, subset = NULL,
                          cutoff = 1.2, prop = 0.8, gr_max = 1.5) {

  grid <- if (end(model$MCMC) - minsize - step > 0) {
    seq(from = start(model$MCMC),
        to = end(model$MCMC) - minsize,
        by = step)
  } else {
    grid <- start(model$MCMC)
  }

  gr <- lapply(grid, function(k) {
    JointAI::GR_crit(model, subset = subset,
                     start = k, autoburnin = FALSE,
                     multivariate = FALSE)[[1]][, 2]
  })

  gr <- do.call(rbind, gr)

  if (any(rowMeans(gr < cutoff) > prop & apply(gr, 1, max) < gr_max)) {
    grid[min(which(rowMeans(gr < cutoff) > prop & apply(gr, 1, max) < gr_max))]
  } else {
    NA
  }
}

check_gr_crit_loo <- function(model, minsize = 500L, step = 200L,
                              subset = NULL, cutoff = 1.2, prop = 0.8,
                              gr_max = 1.5) {


  grid <- if (end(model$MCMC) - minsize - step > 0){
    seq(from = start(model$MCMC),
        to = end(model$MCMC) - minsize,
        by = step)
  } else {
    start(model$MCMC)
  }

  vapply(seq_along(model$MCMC), function(chain) {
    gr <- lapply(grid, function(k){
      JointAI::GR_crit(model, subset = subset, exclude_chains = chain,
                       start = k, autoburnin = FALSE,
                       multivariate = FALSE)[[1]][, 2]
    })
    gr <- do.call(rbind, gr)

    if (any(rowMeans(gr < cutoff) > prop & apply(gr, 1, max) < gr_max)) {
      grid[min(which(rowMeans(gr < cutoff) > prop & apply(gr, 1, max) < gr_max))]
    } else {
      NA
    }
  }, FUN.VALUE = numeric(1L))
}
