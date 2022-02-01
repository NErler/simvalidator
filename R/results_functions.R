
rbind_df_list <- function(df_list) {

  if (!inherits(df_list[[1]][[1]], "data.frame") &
      inherits(df_list[[1]][[1]], "list")) {

    # df_list
    # |- sim1 = df_list[[1]]
    #     |-scen1 = df_list[[1]][[1]]
    #       |- modeltype1 = data.frame = df_list[[1]][[1]][[1]]
    #       |- modeltype2
    #     |-scen2
    # |- sim2
    # |-...
    types <- unname(unlist(lapply(df_list[[1]], function(z) {
      lapply(z, function(k) unique(k$type))
    })))

    df_list <- lapply(df_list, function(x) {
      # x is the output from one simulation and is a list with one element per
      # scenario
      nlapply(types, function(k) {
        do.call(rbind,
        lapply(x, function(z) {
        # z is the output for one scenario, and is a list with one element per
        # model type
          do.call(rbind,
                  lapply(z, function(zz) {
                    if (isTRUE(unique(zz$type) == k)) zz
                  })
          )
        })
        )
      })
    })
  }

  list_by_model <- lapply(seq_along(df_list[[1]]), function(k) {
    do.call(rbind, lapply(df_list, "[[", k))
  })

  colnames <- unique(unlist(lapply(list_by_model, names)))

  do.call(rbind,
          lapply(list_by_model, function(x) {
            x[, setdiff(colnames, colnames(x))] <- NA
            x[, colnames]
          })
  )
}


#' Convert the simulation results into a data.frame
#' The resulting `data.frame` will contain the simulation results from all
#' models, the true regression coefficients (`true_param`) and simulation
#' evaluation measures (bias, relative bias, coverage of the CI, CI width and
#' MSE) per simulated dataset.
#' @param object object of class simulation_result
#' @export
get_res_df <- function(object) {

  default_out <- as.character(
    check_formula_list(object$outcome_pars$formula)[[1]][[2]])

  if (!is.list(object$outcome_pars$reg_coefs)) {
    object$outcome_pars$reg_coefs <- setNames(
      list(object$outcome_pars$reg_coefs), default_out)
  }


  # res_df <- rbind_df_list(lapply(object$sim_res, function(x) {
  #   lapply(x$scen_res, "[[", "res")
  # }))



  res_df <- object$sim_res %>%
    lapply(., function(x) {
      x$model <- attr(x, "model")
      x$miss_scenario <- attr(x, "miss_scenario")
      x$data_seed <- attr(x, "data_seed")
      x
    }) %>%
    do.call(rbind, .)


  true_param_df <- if (is.list(object$outcome_pars$reg_coefs)) {
    do.call(rbind,
            lapply(names(object$outcome_pars$reg_coefs), function(out) {
              data.frame(outcome = unname(JointAI::clean_survname(out)),
                         variable = names(object$outcome_pars$reg_coefs[[out]]),
                         true_param = object$outcome_pars$reg_coefs[[out]]
              )
            })
    )
  } else {
    data.frame(variable = names(object$outcome_pars$reg_coefs),
               true_param = object$outcome_pars$reg_coefs)
  }

  true_param_df <- rbind(true_param_df,
                         get_otherpars_df(object$outcome_pars$other_pars,
                                          res_df),
                         get_vcov_df(object$outcome_pars$ranef_vcov, res_df),
                         get_resid_sd_df(object$outcome_pars$resid_sd, res_df)
  )

  res_df <- merge(res_df, true_param_df, all = TRUE)

  res_df$bias <- res_df$Mean - res_df$true_param
  res_df$relbias <- res_df$bias/res_df$true_param
  res_df$covrg <- res_df$`2.5%` < res_df$true_param & res_df$`97.5%` > res_df$true_param
  res_df$CIwidth <- res_df$`97.5%` - res_df$`2.5%`

  res_df
}


get_otherpars_df <- function(other_pars, res_df)
  do.call(rbind,
          lapply(other_pars, function(p) {
            while (inherits(p,  "list")) {
              p <- unlist(unname(p), recursive = FALSE)
            }
            if (any(names(p) %in% res_df$variable)) {

              data.frame(outcome = res_df$outcome[match(names(p), res_df$variable)],
                         variable = names(p),
                         true_param = p)
            }
          })
  )


get_vcov_df <- function(ranef_vcov, res_df) {
  if (!is.null(res_df)) {
    do.call(rbind,
            lapply(ranef_vcov, function(vcov) {
              nam <- get_vcov_names(vcov)
              data.frame(outcome = res_df$outcome[match(nam, res_df$variable)],
                         variable = nam,
                         true_param = vcov[lower.tri(vcov, diag = TRUE)]
              )
            })
    )
  }
}

get_vcov_names <- function(vcov) {
  unlist(
    lapply(seq_len(ncol(vcov)), function(i) {
      cvapply(seq(i, ncol(vcov), 1), function(j) {
        paste0(attr(vcov, "mat_name"), "[", i, ",", j, "]")
      })
    })
  )
}


get_resid_sd_df <- function(resid_sd, res_df) {

  if (!is.null(res_df)) {

    nam <- paste0("sigma_", names(resid_sd))
    data.frame(outcome = res_df$outcome[match(nam, res_df$variable)],
               variable = nam,
               true_param = resid_sd)

  }
}



print_summary_tab <- function(smry, name) {

  if (!is.null(smry[[1]][[name]])) {

    dat <- lapply(smry, function(x) {
      as.data.frame(x[[name]])
    }) %>% reshape2::melt(id.vars = colnames(.[[1]]))

    cat("\n\n###", name, "\n\n")

    if (inherits(smry[[1]][[name]], "list")) {
      p <- ggplot(dat, aes(x = x, y = y, group = L1)) +
        geom_line(alpha = 0.1) +
        xlab(name) +
        ylab("density")
    } else if (inherits(smry[[1]][[name]], "table")) {
      # p <- ggplot(dat, aes(x = factor(L1), y = Freq, fill = category)) +
      #   geom_bar(stat = "identity") +
      #   scale_fill_viridis_d(name = name) +
      #   xlab("simulation") +
      #   theme(axis.text.x = element_blank(),
      #         axis.ticks.x = element_blank(),
      #         legend.position = "top")

      p <- ggplot(dat, aes(x = factor(category), y = Freq)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        geom_jitter(width = 0.2, height = 0, alpha = 0.3) +
        xlab("category") +
        ylab("proportion")
    }
    print(p)
  }
}
