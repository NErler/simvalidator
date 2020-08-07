
#' Simulate data from a multivariate normal distribution
#'
#' Data are simulated from a multivariate normal distribution. To create
#' factors, the columns for which `probs` and `ordered`is specified are
#' converted into factors by cutting the simulated continuous variable at the
#' quantiles given by `probs`.
#'
#' @param N the sample size
#' @param covar_pars a list with objects `means`, `vcov`, `probs` and `ordered`
#' @param seed seed value
#' @param ... additional parameters (not used)
#'
#' @export
sim_covars_mvn <- function(N, covar_pars, seed = NULL, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  idvars <- names(covar_pars$means)
  group_lvls <- covar_pars$group_lvls

  df <- lapply(idvars, function(lvl) {

    new_data <- as.data.frame(
      MASS::mvrnorm(N[lvl],
                    mu = covar_pars$means[[lvl]],
                    Sigma = covar_pars$vcov[[lvl]])
    )

    for (k in intersect(names(covar_pars$probs), names(new_data))) {
      probs <- covar_pars$probs[[k]]
      new_data[[k]] <- factor(
        cut(new_data[[k]],
            c(-Inf, qnorm(probs[-length(probs)]), Inf),
            labels = names(probs)),
        ordered = covar_pars$ordered[[k]])
    }

    new_data[[lvl]] <- seq_len(nrow(new_data))

    for (k in names(group_lvls)[group_lvls > group_lvls[[lvl]]]) {
      new_data[[k]] <- sort(c(
        rep(seq_len(N[[k]]), each = floor(N[[lvl]]/N[[k]])),
        sample.int(N[[k]], size = N[[lvl]] %% N[[k]])
      ))
    }

    if (!is.null(covar_pars$timevar_pars$name)) {
      if (covar_pars$timevar_pars$name %in% names(new_data)) {
        df_list <- lapply(
          split(new_data,
                new_data[[names(group_lvls)[group_lvls == group_lvls[[lvl]] + 1]]]),
          function(df) {
            df[[covar_pars$timevar_pars$name]] <-
              c(0,
                sort(
                  runif(nrow(df) - 1,
                        covar_pars$timevar_pars$min,
                        covar_pars$timevar_pars$max)
                )
              )
            df
          })
        new_data <- do.call(rbind, df_list)
      }
    }
    new_data
  })

  Reduce(merge, df)
}
