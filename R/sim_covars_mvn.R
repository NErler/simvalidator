
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

  if (length(group_lvls) > 1) {
    iddat <- lapply(N, seq.int)

    for (k in sort(unique(group_lvls))[-1]) {

      for (lvl in names(group_lvls)[which(group_lvls == k)]) {
        lvl_prev <- names(group_lvls)[which(group_lvls == (k - 1))]

        if (length(lvl_prev) > 1) {
          type <- covar_pars$group_rel[lvl, lvl_prev] +
            covar_pars$group_rel[lvl_prev, lvl]
          lvl_prev <- names(type)[type == 1]
        }

        temp <- rep_lvls(iddat[[lvl]], N[[lvl_prev]])

        if (sum(group_lvls == k) > 1) {
          temp <- sample(temp)
        }
        iddat[[lvl]] <- temp[iddat[[lvl_prev]]]
      }
    }

  df <- nlapply(idvars, function(lvl) {

    new_data <- if (length(covar_pars$means[[lvl]]) > 0L) {
      as.data.frame(
        MASS::mvrnorm(N[lvl],
                      mu = covar_pars$means[[lvl]],
                      Sigma = covar_pars$vcov[[lvl]])
      )
    } else {
      temp <- data.frame(lvl = seq_len(N[lvl]))
      names(temp) <- lvl
      temp
    }

    for (k in intersect(names(covar_pars$probs), names(new_data))) {
      probs <- covar_pars$probs[[k]]
      new_data[[k]] <- factor(
        cut(new_data[[k]],
            c(-Inf, qnorm(probs[-length(probs)]), Inf),
            labels = names(probs)),
        ordered = covar_pars$ordered[[k]])
    }

    new_data[[lvl]] <- seq_len(nrow(new_data))


    if (!is.null(covar_pars$timevar_pars$name)) {
      fun <- get(paste0("r", covar_pars$timevar_pars$distr))

      if (covar_pars$timevar_pars$name %in% names(new_data)) {
        df_list <- lapply(
          split(new_data,
                iddat[[names(group_lvls)[group_lvls == group_lvls[[lvl]] + 1]]]),
          function(df) {
            if (covar_pars$timevar_pars$distr == "unif") {
              df[[covar_pars$timevar_pars$name]] <-
                c(0,
                  sort(
                    do.call(fun, as.list(c(n = nrow(df) - 1,
                                           covar_pars$timevar_pars$params))
                    )
                  )
                )
            } else {
              df[[covar_pars$timevar_pars$name]] <-
                sort(
                  do.call(fun,
                          as.list(c(n = nrow(df),
                                    covar_pars$timevar_pars$params))
                  ) * covar_pars$timevar_pars$multiply
                )
            }
            df
          })
        new_data <- do.call(rbind, df_list)
      }
    }
    new_data
  })





  if (!identical(JointAI:::identify_level_relations(iddat),
                 covar_pars$group_rel[names(iddat), names(iddat)])) {
    stop("The created data does not have the hierarchical structure indicated
         in the input.")
  }

  Reduce(merge, c(list(as.data.frame(iddat)), df))
  } else {
    Reduce(merge, df)
  }
}





rep_lvls <- function(lvls, n) {
  c(rep(lvls, each = floor(n/length(lvls))),
    sample(lvls, size = n %% length(lvls)))
}
