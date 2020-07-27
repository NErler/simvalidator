#' Simulate a conditionally normally distributed outcome
#' @param data a `data.frame` containing covariate data
#' @param formula the model formula
#' @param reg_coefs a vector of regression coefficients. If named, the names
#'                  will be matched with the names of the design matrix that
#'                  is created from `formula` and `data`.
#' @param resid_sd residual standard deviation
#' @param seed optional seed value
#' @param ... other arguments passed to other functions
#' @export
#'
sim_outcome_lm <- function(data, formula, reg_coefs, resid_sd, seed = NULL,
                           ...) {

  if (!is.null(seed)) {
    set.seed
  }

  desgn_mat <- model.matrix(formula[-2], data)
  reg_coefs <- check_coef_mat(reg_coefs, desgn_mat)

  # linear predictor
  lin_pred <- as.vector(desgn_mat %*% reg_coefs[colnames(desgn_mat)])

  data[[formula[[2]]]] <- rnorm(nrow(data), mean = lin_pred, sd = resid_sd)

  data
}
