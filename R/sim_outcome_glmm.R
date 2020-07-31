#' Simulate a (multivariate) longitudinal outcome
#' @param data a `data.frame` containing covariate data
#' @param formula a list of model formulas for the longitudinal outcomes
#' @param reg_coefs a named list of regression coefficients for
#'                  each model, with names equal to the corresponding response
#'                  variable. Each of the list elements is a (named) vector.
#'                  If named, the names will be matched with the names of the
#'                  design matrix that is created from `formula` and `data`.
#' @param ranef_vcov a random effects variance-covariance matrix or a named
#'                   list of such matrices when there are more than two levels
#'                   (and names being equal to the grouping variables)
#' @param resid_sd named vector of residual standard deviations, with names
#'                 equal to the corresponding response variable (for types
#'                 "gaussian" and "Gamma")
#' @param type named vector of model types. Available model types are
#'             "gaussian", "binomial", "Gamma", and "poisson".
#' @param seed the seed value
#' @param ... arguments passed to other functions
#'
#' @export
#'
#' @examples
#' # Bivariate outcome in a multi-level setting
#'
#'
#' covar_data <- function(N, J) {
#'   merge(
#'     # subject level information
#'     data.frame(id = 1:N,
#'                center = rep(1:J, each = N/2),
#'                age = rnorm(N, 60, 10),
#'                sex = factor(rbinom(N, size = 1, prob = 0.5))),
#'     # repeated measurements
#'     data.frame(id = rep(1:N, each = J),
#'                time = runif(J * N, 0, 5)
#'     )
#'   )
#' }
#'
#' ranef_vcov <- list(id = Matrix::bdiag(
#'   matrix(nrow = 2, ncol = 2, data = c(2, 0.5, 0.5, 1),
#'          dimnames = list(c('int', 'time'), c('int', 'time'))),
#'   matrix(nrow = 3, ncol = 3, data = c(  2,  0.5,  0.1,
#'                                         0.5, 1.5, -0.2,
#'                                         0.1, -0.2,  1),
#'          dimnames = list(c('int', 'time1', "time2"), c('int', 'time1', 'time2')))
#' ),
#' center = matrix(nrow = 2, ncol = 2, data = 0.2,
#'                 dimnames = list(c('int', 'int'), c('int', 'int')))
#' )
#'
#'
#' formula <- list(x1 ~ age + sex + time + (time | id) + (1 | center),
#'                 x2 ~ age + x1 + sex + ns(time, df = 2) + (ns(time, df = 2) | id) +
#'                   (1 | center)
#' )
#'
#' reg_coefs = list(x1 = c(1:4),
#'                  x2 = c(-100,  2, -0.01, -3, -2, -1))
#' sim_outcome_glmm(data = covar_data(N = 50, J = 2),
#'                  formula = formula,
#'                  reg_coefs = reg_coefs,
#'                  resid_sd = c(x1 = 0.5),
#'                  ranef_vcov = ranef_vcov,
#'                  type = c(x1 = "gaussian", x2 = "binomial"))



sim_outcome_glmm <- function(data, formula, reg_coefs, resid_sd = NULL,
                             ranef_vcov, type = "gaussian",
                             return_ranefs = FALSE, ranefs = NULL, ...) {

  # model formulas
  formula <- split_formula_list(check_formula_list(formula))
  fmla_fixed <- lapply(formula$fixed, function(x) {
    as.formula(deparse(x[-2], width.cutoff = 500))
  })


  if (length(type) == 1L & length(fmla_fixed) == 1L & is.null(names(type))) {
    names(type) <- names(fmla_fixed)
  } else if (length(type) > 1L & is.null(names(type))) {
    errormsg("Please provide names for the elements in %s.",
             dQuote("type"))
  }


  # random effects linear predictor
  desgn_mat_random <- lapply(formula$random, function(fmla) {
    lapply(remove_grouping(fmla), model_matrix, data = data)
  })


  if (is.null(ranefs)) {
    # random effects
    # groups
    idvars <- extract_id(formula$random)
    groups <- get_groups(idvars, data)

    n_ranefs <- ivapply(groups, function(x)  length(unique(x)))
    ranefs <- replicate_ranefs(sim_ranefs(ranef_vcov, n_ranefs), groups)
    ranefs <- split_ranefs(ranefs, desgn_mat_random, idvars)
  }

  for (k in names(fmla_fixed)) {

    # fixed effects design matrices
    desgn_mat_fixed <- model_matrix(fmla_fixed[[k]], data = data)

    # linear predictor of the longitudinal outcomes
    coefs <- check_coef_mat(reg_coefs[[k]], desgn_mat_fixed)
    linpred_fixed <- as.vector(desgn_mat_fixed %*%
                                 coefs[colnames(desgn_mat_fixed)])

    # linear predictor of the random effects
    linpred_random <- Map(function(r, d) {
      rowSums(r * d)
    }, r = ranefs[[k]], d = desgn_mat_random[[k]])

    # combine fixed and random effects
    linpred <- rowSums(do.call(cbind, c(list(linpred_fixed), linpred_random)))

    data[[k]] <- sample_glm_resp(type[[k]], linpred, resid_sd[[k]])
  }

  if (return_ranefs) {
    list(data = data, ranefs = ranefs)
  } else {
    data
  }
}




sim_outcome_glmm2 <- function(data, fmla_fixed, fmla_random,
                              reg_coefs, resid_sd = NULL,
                              type = "gaussian", ranefs, ...) {

  # random effects linear predictor
  desgn_mat_random <- lapply(fmla_random, function(fmla) {
    lapply(fmla, model_matrix, data = data)
  })


  for (k in names(fmla_fixed)) {

    # fixed effects design matrices
    desgn_mat_fixed <- model_matrix(fmla_fixed[[k]], data = data)

    # linear predictor of the longitudinal outcomes
    coefs <- check_coef_mat(reg_coefs[[k]], desgn_mat_fixed)
    linpred_fixed <- as.vector(desgn_mat_fixed %*%
                                 coefs[colnames(desgn_mat_fixed)])

    # linear predictor of the random effects
    linpred_random <- Map(function(r, d) {
      rowSums(r * d)
    }, r = ranefs[[k]], d = desgn_mat_random[[k]])

    # combine fixed and random effects
    linpred <- rowSums(do.call(cbind, c(list(linpred_fixed), linpred_random)))
    data[[k]] <- linpred

    # data[[k]] <- sample_glm_resp(type[[k]], linpred, resid_sd[[k]])
  }

    data
}

