#' Simulate a survival outcome
#' @param data a `data.frame` containing covariate data
#' @param formula the model formula
#' @param reg_coefs a vector of regression coefficients. If named, the names
#'                  will be matched with the names of the design matrix that
#'                  is created from `formula` and `data`.
#' @param phi parameter of the Wishart distribution
#' @param mean_cens mean censoring time
#' @param .tries integer; how often is the upper limit increased when looking
#'               for the root
#' @param seed the seed value
#' @param ... arguments passed to other functions
#'
sim_outcome_survival <- function(data, formula, reg_coefs, phi = 1.2,
                                 mean_cens = 30.0, .tries = 5L,
                                 seed = NULL, ...) {

  desgn_mat <- model.matrix(formula[-2], data)
  reg_coefs <- check_coef_mat(reg_coefs, desgn_mat)

  # linear predictor
  lin_pred <- as.vector(desgn_mat %*% reg_coefs[colnames(desgn_mat)])

  inv_survival <- function(t, u, i) {
    h <- function(s) {
      exp(log(phi) + (phi - 1) * log(s) + lin_pred[i])
    }
    integrate(h, lower = 0, upper = t)$value + log(u)
  }


  u <- runif(nrow(data))
  true_times <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    up <- 5000L
    tries <- .tries
    root <- try(uniroot(inv_survival,
                        interval = c(1.0e-05, up), u = u[i], i = i)$root, TRUE)
    while (inherits(root, "try-error") && tries > 0) {
      tries <- tries - 1L
      up <- up + 5000L
      root <- try(uniroot(inv_survival, interval = c(1.0e-05, up),
                          u = u[i], i = i)$root, TRUE)
    }
    true_times[i] <- if (!inherits(root, "try-error")) {
      root
    } else {
      NA
    }
  }

  na_indicator <- !is.na(true_times)
  true_times <- true_times[na_indicator]
  data <- data[na_indicator, , drop = FALSE]

  n <- length(true_times)

  # simulate censoring times from an exponential distribution, and calculate the
  # observed event times, i.e., min(true event times, censoring times)
  cens_times <- runif(n, 0L, 2L * mean_cens)
  data[, all.vars(formula[[2]])] <- cbind(pmin(true_times, cens_times),
                                          as.numeric(true_times <= cens_times))
  data
}
