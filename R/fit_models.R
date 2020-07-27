
#' Fit one more multiple models and obtain the model result
#' @param models a list of model information
#' @param formula the model formula
#' @param data a `data.frame`
#' @param seed an optional seed value
#'
#' @export
fit_models <- function(models, formula, data, seed = NULL) {

  check_resfcts <- vapply(paste0("get_result_", lapply(models, "[[", "result")),
                          function(fun) {
                            inherits(get(fun), "function")
                          }, FUN.VALUE = logical(1L))

  if (!any(check_resfcts)) {
    errormsg("%s is not a function.",
             names(check_resfcts)[!check_resfcts])
  }

  result <- lapply(seq_along(models), function(k) {
    fitted_model <- do.call(models[[k]]$fun,
                            set_args(fun = models[[k]]$fun,
                                     args = c(formula = formula,
                                              list(data = data),
                                              models[[k]]$fun_args)
                            )
    )

    # summarize the result
    result_fct <- paste0("get_result_", models[[k]]$result)

    do.call(get(result_fct),
            set_args(fun = result_fct,
                     args = c(list(fitted_model = fitted_model),
                              seed = seed,
                              models[[k]]$res_args)
            )
    )

  })

  result
}
