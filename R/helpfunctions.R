errormsg <- function(x, ...) {
  stop(strwrap(gettextf(x, ...), prefix = "\n"), call. = FALSE)
}


nlapply <- function(x, fun, ...) {
  # a named version of lapply, intended to replace sapply(..., simplify = FALSE)

  l <- lapply(x, fun, ...)
  if (is.null(names(l)))
    if (!is.null(names(x))) {
      names(l) <- names(x)
    } else if (is.character(x)) {
      names(l) <- x
    }
  l
}

lvapply <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = logical(1L), ..., USE.NAMES = TRUE)
}

cvapply <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = character(1L), ..., USE.NAMES = TRUE)
}

nvapply <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = numeric(1L), ..., USE.NAMES = TRUE)
}

ivapply <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = integer(1L), ..., USE.NAMES = TRUE)
}


check_coef_mat <- function(reg_coefs, desgn_mat) {
  if (!is.null(names(reg_coefs)) &
      any(is.na(match(colnames(desgn_mat), names(reg_coefs))))) {
    errormsg("The names of the coefficients do not match the names of the
             design matrix. The names of the design matrix are %s.",
             paste0(colnames(desgn_mat), collapse = ", "))
  } else if (is.null(names(reg_coefs))) {
    if (length(reg_coefs) != ncol(desgn_mat)) {
      errormsg("You have provided %s regression coefficients, but the design
               matrix has %s columns (%s).", length(reg_coefs), ncol(desgn_mat),
               paste0(colnames(desgn_mat), collapse = ", "))
    } else {
      names(reg_coefs) <- colnames(desgn_mat)
    }
  }
  reg_coefs
}

select_coefs <- function(reg_coefs, desgn_mat) {
  if (is.null(names(reg_coefs)) | is.null(colnames(desgn_mat))) {
    errormsg("%s requires a named vector of regression coefficients and a
    design matrix with column names.", dQuote("select_coefs()"))
  }

  reg_coefs[na.omit(match(colnames(desgn_mat), names(reg_coefs)))]
}



set_args <- function(fun, args) {
  # find default arguments of the result function
  default_args <- formals(fun)
  default_args$`...` <- NULL

  # augment the arguments with the default arguments
  # args[setdiff(names(default_args), names(args))] <-
  #   default_args[setdiff(names(default_args), names(args))]

  if (is.null(default_args$formula)) {
    default_args <- c(default_args, setNames(list(NULL), "formula"))
  }
  default_args[intersect(names(args), names(default_args))] <-
    args[intersect(names(args), names(default_args))]


  default_args
}


#' Set a model specification
#' @param fun a function
#' @param fun_args optional list of arguments (not including `formula` and
#'                 `data`) to be provided to `fun`
#' @param result type of result summary (to be used to find `get_result_<...>()`)
#' @param res_args optional list or arguments provided to the
#'                    `get_result_<...>()` function
#' @param skip_scen optional vector of the names of scenarios to be skipped
#' @export
set_model <- function(fun, fun_args = NULL, result = "default",
                      res_args = NULL, skip_scen = NULL) {
  args <- formals()
  call <- as.list(match.call())[-1L]
  call <- c(call, args[!names(args) %in% names(call)])

  structure(c(lapply(call, eval),
              list(call = call)),
            class = "model_specification")
}

