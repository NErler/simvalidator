errormsg <- function(x, ...) {
  stop(strwrap(gettextf(x, ...), prefix = "\n"), call. = FALSE)
}



check_coef_mat <- function(reg_coefs, desgn_mat) {
  if (!is.null(names(reg_coefs)) &
      any(is.na(match(colnames(desgn_mat), names(reg_coefs))))) {
    errormsg("The names of the coefficients do not match the names of the
             design matrix. The names of the design matrix are %s.",
             colnames(desgn_mat))
  } else if (is.null(names(reg_coefs))) {
    if (length(reg_coefs) != ncol(desgn_mat)) {
      errormsg("You have provided %s regression coefficients, but the design
               matrix has %s columns.", length(reg_coefs), ncol(desgn_mat))
    } else {
      names(reg_coefs) <- colnames(desgn_mat)
    }
  }
  reg_coefs
}



set_args <- function(fun, args) {
  # find default arguments of the result function
  default_args <- formals(fun)
  default_args$`...` <- NULL

  # augment the arguments with the default arguments
  args[setdiff(names(default_args), names(args))] <-
    default_args[setdiff(names(default_args), names(args))]

  args
}


#' Set a model specification
#' @param fun a function
#' @param fun_args optional list of arguments (not including `formula` and
#'                 `data`) to be provided to `fun`
#' @param result type of result summary (to be used to find `get_result_<...>()`)
#' @param res_args optional list or arguments provided to the
#'                    `get_result_<...>()` function
#' @export
set_model <- function(fun, fun_args = NULL, result = "default",
                      res_args = NULL) {
  args <- formals()
  call <- as.list(match.call())[-1L]
  call <- c(call, args[!names(args) %in% names(call)])

  structure(c(lapply(call, eval),
              list(call = call)),
            class = "model_specification")
}

