
#' Fit one more multiple models and obtain the model result
#' @param models a list of model information
#' @param formula the model formula
#' @param data a `data.frame`
#' @param seed an optional seed value
#' @param scen optional; name of scenario
#'
#' @export
fit_models <- function(models, formula, data, seed = NULL, scen = NULL) {

  check_resfcts <- vapply(paste0("get_result_", lapply(models, "[[", "result")),
                          function(fun) {
                            inherits(get(fun), "function")
                          }, FUN.VALUE = logical(1L))

  if (!any(check_resfcts)) {
    errormsg("%s is not a function.",
             names(check_resfcts)[!check_resfcts])
  }

  result <- lapply(seq_along(models), function(k) {
    if (!scen %in% models[[k]]$skip_scen) {
      fitted_model <- do.call(models[[k]]$fun,
                              set_args(fun = models[[k]]$fun,
                                       args = c(list(formula = formula,
                                                     seed = seed,
                                                     data = data),
                                                models[[k]]$fun_args)
                              )
      )

      # summarize the result
      result_fct <- paste0("get_result_", models[[k]]$result)

      do.call(get(result_fct),
              set_args(fun = result_fct,
                       args = c(list(fitted_model = fitted_model),
                                seed = seed,
                                scen = scen,
                                models[[k]]$res_args)
              )
      )
    }

  })

  result
}



fit_model <- function(model, data) {

  args <- model[!names(model) %in% c("model", "other_args)")]

  args$data <- if (isTRUE(model$other_args$cc)) {
    make_cc_subset(data, model$formula)
  } else {
    data
  }


  if (!is.null(model$other_args$init_args)) {

    n_chains <- if (is.null(args$n.chains)) {
      args$default_args$n.chains
    } else {
      args$n.chains
    }

    init_args <- model$other_args$init_args %>%
      eval %>%
      `[`(setdiff(names(.), "fun"))
    init_args$formula = args$formula
    init_args$data = args$data
    init_args$n_chains = n_chains
    init_args$seed = next_seed(attr(data, "data_seed"))


    inits <- do.call(eval(model$other_args$init_args$fun), init_args)

    args$inits <- inits
  }


  args$seed <- next_seed(attr(data, "data_seed"))
  fitted_model <- do.call(eval(model$fun), args)

  if (!is.null(model$other_args$add_samples_adaptive)) {
    args_adaptive <- eval(eval(model$other_args$add_samples_adaptive))
    args_adaptive$fitted_model <- fitted_model
    fitted_model <- do.call(add_samples_adaptive, args_adaptive)
  }
  fitted_model
}
