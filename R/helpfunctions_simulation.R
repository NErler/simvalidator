


next_seed <- function(seed) {
  set.seed(seed)
  sample.int(1e6, size = 1)
}



sim_ranefs <- function(ranef_vcov, n_ranef) {
  # simulate random effects from a multivariate normal distribution
  # with given variance-covariance matrix
  Map(function(nn, dd) {
    MASS::mvrnorm(nn, rep(0, ncol(dd)), dd)
  }, dd = ranef_vcov, nn = as.list(n_ranef)[names(ranef_vcov)])
}

replicate_ranefs <- function(ranefs, groups) {
  nlapply(names(ranefs), function(k) {
    ranefs[[k]][groups[[k]], , drop = FALSE]
  })
}


split_ranefs <- function(ranefs, desgn_mat, idvars) {

  ranef_cols <- nlapply(idvars, function(lvl) {
    l <- lapply(desgn_mat, "[[", lvl)
    unlist(nlapply(names(l), function(v) {
      rep(v, ncol(l[[v]]))
    })
    )
  })

  nlapply(names(desgn_mat), function(v) {
    nlapply(names(ranefs), function(lvl) {
      if (length(ranef_cols[[lvl]]) != ncol(ranefs[[lvl]])) {
        errormsg("The number of columns in the random effects matrix and
               random effects design matrix do not match.")
      }
      ranefs[[lvl]][, ranef_cols[[lvl]] == v, drop = FALSE]
    })
  })
}



sample_glm_resp <- function(type, linpred, resid_sd) {
  if (type %in% c("gaussian", "normal", "lme")) {
    rnorm(length(linpred), mean = linpred, sd = resid_sd)
  } else if (type == "binomial") {
    rbinom(length(linpred), size = 1, prob = plogis(linpred))
  } else if (type %in% c("Gamma", "gamma")) {
    rgamma(length(linpred),
           shape = linpred^2 / resid_sd^2,
           rate = linpred / resid_sd^2)
  } else if (type %in% c("poisson", "Poisson")) {
    rpois(length(linpred), lambda = exp(linpred))
  } else {
    errormsg("Simulation of an outcome of type %s is not implemented.", type)
  }
}




#' Create a variance-covariance matrix from a vector of variances and a
#' vector of correlations
#' @param variances numeric vector
#' @param correlations numeric vector
#' @export
make_vcov <- function(variances, correlations) {
  if (inherits(correlations, "matrix")) {
    cor_mat <- correlations
  } else {
    cor_mat <- diag(length(variances))
    cor_mat[upper.tri(cor_mat)] <- correlations
    cor_mat[lower.tri(cor_mat)] <- t(cor_mat)[lower.tri(cor_mat)]
  }

  sd_mat <- diag(sqrt(variances), ncol = length(variances))
  sd_mat %*% cor_mat %*% sd_mat
}


#' Simulate uniformly distributed correlation elements for a variance-covariance matrix
#' @param dim the dimension of the matrix
#' @param min minimum used in `runif()`
#' @param max maximum used in `runif()`
#' @export
sim_corr_unif <- function(dim, min = -0.5, max = 0.5) {
  runif(dim * (dim - 1)/2, min = min, max = max)
}


#' Combine multiple variance-covariance matrices to one multivariate matrix
#' @param ... separate variance-covariance matrices
#' @param structure character string indicating the correlation structure
#'                  between the matrices (at the moment only "indep")
#' @export
#' @examples
#' make_mv_vcov(
#' make_vcov(rgamma(2, 3, 2), sim_corr_unif(2)),
#' make_vcov(rgamma(4, 3, 2), sim_corr_unif(4))
#' )
#'
make_mv_vcov <- function(..., structure = "indep") {
  if (structure == "indep") {
    mat_name <- try(cvapply(as.list(...), attr, "mat_name"), silent = TRUE)

    structure(as.matrix(Matrix::bdiag(...)),
              "mat_name" = if (!inherits(mat_name, "try-error")) mat_name
    )
  } else {
    errormsg("not yet implemented")
  }
}



gauss_kronrod <- function() {
  # return a list with Gauss-Kronrod quadrature points and weights

  m <- matrix(nrow = 15, ncol = 2, byrow = TRUE,
              data = c(-0.9914553711208126392069,	0.0229353220105292249637,
                       -0.9491079123427585245262,	0.0630920926299785532907,
                       -0.8648644233597690727897,	0.1047900103222501838399,
                       -0.7415311855993944398639,	0.140653259715525918745,
                       -0.5860872354676911302941,	0.1690047266392679028266,
                       -0.4058451513773971669066,	0.1903505780647854099133,
                       -0.2077849550078984676007,	0.2044329400752988924142,
                       0,	0.209482141084727828013,
                       0.2077849550078984676007,	0.2044329400752988924142,
                       0.4058451513773971669066,	0.190350578064785409913,
                       0.5860872354676911302941,	0.1690047266392679028266,
                       0.7415311855993944398639,	0.1406532597155259187452,
                       0.8648644233597690727897,	0.10479001032225018384,
                       0.9491079123427585245262,	0.0630920926299785532907,
                       0.9914553711208126392069,	0.02293532201052922496373))
  return(list(gkx = m[, 1], gkw = m[, 2]))
}


# used in get_data_list() and predict_coxph() (2020-06-11)
get_knots_h0 <- function(nkn, Time, gkx, obs_kn = TRUE) {
  # obtain the knots used in the B-spline specification of the baseline hazard
  # in coxph and JM
  # - nkn: number of (inner) knots
  # - Time: vecor of event times
  # - gkx: Gauss-Kronrod quadrature points
  # - obs_kn: logical; use all observations of the event times
  #           (events and censorings; TRUE) or just actual event times (FALSE)

  pp <- seq(0, 1, length.out = nkn + 2)
  pp <- tail(head(pp, -1), -1) # remove first and last
  kn <- quantile(Time, pp, names = FALSE)

  kn <- kn[kn <= max(Time)]
  sort(c(rep(range(Time, outer(Time/2, gkx + 1)), 4), kn))
}


centering <- function(data) {
  for (k in colnames(data)) {
    if (!inherits(data[, k], "factor")) {
      data[, k] <- data[, k] - mean(data[, k], na.rm = TRUE)
    }
  }
  data
}
