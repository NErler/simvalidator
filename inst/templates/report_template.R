#' ---
#' title: "Simulation result JointAI"
#' subtitle: "Simple survival model"
#' author: ""
#' date: "`r Sys.setenv(LANG = 'en_US.UTF-8'); format(Sys.Date(), '%d %B %Y')`"
#' output:
#'   bookdown::html_document2:
#'    number_sections: false
#'    toc: true
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#' params:
#'   object: NA
#' ---
#'

#+ include = FALSE
knitr::opts_chunk$set(echo = FALSE, out.width = "100%")
options(knitr.kable.NA = '')

# ggplot theme
library("ggplot2")
library("kableExtra")


theme_set(theme_gray())
theme_update(panel.grid = element_blank(),
             panel.background = element_rect(fill = grey(0.98)),
             panel.border = element_rect(fill = 'transparent',
                                         color = grey(0.85))
)



object <- params$object
res_df <- get_res_df(object)

res_info <- object$sim_res %>%
  lapply(., function(x) {
    attributes(x) %>%
      `[`(c("miss_scenario", "duration_mins", "model", "data_seed",
            "n_iter", "n_chain")) %>%
      as.data.frame
  })




outvars <- lapply(check_formula_list(object$outcome_pars$formula),
                  function(fmla) {
                    fmla[3] <- 1
                    JointAI::all_vars(fmla)
                  }) %>%
  unlist




#' Simulation report
#'
#' * `r object$file_name`
#' * `r object$sim_pars$nr_sims` simulated datasets
#'
#' Scenarios
#' * `r length(object$mis_scenarios)` scenarios with respect to missing data
#' * `r length(object$models)` models
#'
#' &#8680; `r nrow(object$scenarios)` scenarios:
object$scenarios
#'
#' * total time: `r object$time`
object$platform

#' # Data
#' Target sample size per simulated dataset:
object$outcome_pars$N %>%
  kable(col.names = " ") %>%
  kable_styling(full_width = FALSE)

#+ samplesize, fig.height = 4, fig.cap = figcap, warning = FALSE
figcap <- "Sample size per data level (multi-level data) and simulated dataset.
In models for time-to-event outcomes the size of the simulated data can differ
from the target size since repeated measurements of covariates are simulated
first and deleted after simulation of the time-to-event outcome if they occur
after the event / censoring time."
p <- lapply(object$compl_data_info, "[[", "size") %>%
  do.call(rbind, .) %>%
  reshape2::melt() %>%
  ggplot(aes(x = value)) +
  facet_wrap("Var2") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  xlab("sample size")

if (length(object$compl_data_info) > 50) {
  p + geom_histogram(bins = 100)
} else {
  p + suppressWarnings(geom_dotplot())
}


#' ### Model formula
object$outcome_pars$formula

#'
#' ## Outcome {.tabset}
#+ outvarsplots, results = "asis"

data_summary <- lapply(object$compl_data_info, "[[", "summary")
for (k in outvars) {
  print_summary_tab(data_summary, name = k)
}


#' ## Covariates {.tabset}
#'
#+ covarplots, results = "asis"
for (k in setdiff(names(data_summary[[1]]), outvars)) {
  print_summary_tab(data_summary, name = k)
}


#'
#' ## Missing Data
unlist(object$missing_data_info, recursive = FALSE) %>%
  lapply(., function(x) {
    lapply(x$perc_missing, t) %>%
      reshape2::melt() %>%
      `colnames<-`(gsub("L1", "level", colnames(.)) %>%
                     gsub("Var2", "variable", .)) %>%
      plyr::mutate(scenario = x$scen,
                   data_seed = if (is.null(x$data_seed)) NA else x$data_seed)
  }) %>%
  do.call(rbind, .) %>%
  ggplot(aes(y = variable, x = value,
             group = interaction(variable, scenario),
             fill = scenario, color = scenario)) +
  geom_boxplot(alpha = 0.2) +
  # geom_jitter(alpha = 0.2) +
  scale_x_continuous(name = "missing values (%)",
                     breaks = seq(0, 1, 0.1)) +
  ylab("") +
  facet_wrap("level", scales = "free_y")




#' # Technical Stuff
lapply(object$compl_data_info, "[[", 'nr_tries') %>%
  reshape2::melt() %>%
  ggplot(aes(x = factor(L1),
             fill = factor(value, levels = rev(sort(unique(value)))))) +
  geom_bar() +
  xlab("Simulation Nr.") +
  scale_fill_viridis_d(name = "nr_tries", direction = -1) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top")



if (is.null(res_df)) {
  knitr::knit_exit()
}


do.call(rbind, res_info) %>%
  ggplot(aes(x = n_iter, y = duration_mins, color = model, shape = miss_scenario)) +
  geom_point()

do.call(rbind, res_info) %>%
  ggplot(aes(x = interaction(model, miss_scenario), y = duration_mins)) +
  geom_boxplot()

do.call(rbind, res_info) %>%
  ggplot(aes(x = interaction(model, miss_scenario), y = n_iter)) +
  geom_boxplot() +
  xlab("number of iterations per chain")


do.call(rbind, res_info) %>%
  ggplot(aes(x = interaction(model, miss_scenario), y = n_iter * n_chain)) +
  geom_boxplot() +
  xlab("total number of iterations (all chains)")


do.call(rbind, res_info) %>%
  ggplot(aes(x = interaction(model, miss_scenario), fill = factor(n_chain))) +
  geom_bar()

#
# unique(subset(res_df,
#               select = c("n_iter", "seed", "n_chain"),
#               !is.na(n_iter))) %>%
#   ggplot(aes(x = factor(n_chain), y = n_iter)) +
#   geom_violin() +
#   geom_jitter(width = 0.2, height = 0)





#' # Results
#' ## Bias and CI width {.tabset}

#' ### absolute bias {.tabset}

#+ results = "asis"
plotdf <- subset(res_df, !is.na(bias))

for (out in unique(plotdf$outcome)) {

  cat("\n\n####", out, "\n\n\n")

  p <- subset(plotdf, outcome == out) %>%
  ggplot(., aes(x = variable, y = bias,
      group = interaction(model, miss_scenario, variable),
      color = model,  fill = model)) +
  facet_wrap("miss_scenario", scales = 'free_x') +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'black') +
  xlab('') +
  ylab('absolute bias') +
  geom_boxplot(alpha = 0.2) +
  coord_flip() +
  theme(legend.position = "top") +
  scale_color_viridis_d(end = 0.85, name = "scenario",
                        aesthetics = c("color", "fill"))

  print(p)
}


#' ### relative bias {.tabset}

#+ results = "asis"
plotdf <- subset(res_df, !is.na(relbias))

for (out in unique(plotdf$outcome)) {

  cat("\n\n####", out, "\n\n\n")

  p <- subset(plotdf, outcome == out) %>%
    ggplot(., aes(x = variable, y = relbias,
                  group = interaction(model, miss_scenario, variable),
                  color = model,  fill = model)) +
    facet_wrap("miss_scenario", scales = 'free_x') +
    geom_hline(yintercept = 1, linetype = 2, size = 1, color = 'black') +
    xlab('') +
    ylab('relative bias') +
    geom_boxplot(alpha = 0.2) +
    coord_flip() +
    theme(legend.position = "top") +
    scale_color_viridis_d(end = 0.85, name = "scenario",
                          aesthetics = c("color", "fill"))

  print(p)
}



#' ### parameter estimate {.tabset}
#+ results = "asis"
plotdf <- subset(res_df, !is.na(bias))

for (out in unique(plotdf$outcome)) {

  cat("\n\n####", out, "\n\n\n")

  p <- subset(plotdf, outcome == out) %>%
    ggplot(., aes(x = variable, y = Mean,
                  group = interaction(model, miss_scenario, variable),
                  color = model,  fill = model)) +
    facet_wrap("miss_scenario", scales = 'free_x') +
    geom_point(aes(y = true_param),
               linetype = 2, size = 1, color = 'black') +
    xlab('') +
    ylab('parameter estimate (posterior mean)') +
    geom_boxplot(alpha = 0.2) +
    coord_flip() +
    theme(legend.position = "top") +
    scale_color_viridis_d(end = 0.85, name = "scenario",
                          aesthetics = c("color", "fill"))

  print(p)
}


#' ### CI width {.tabset}

#+ results = "asis"
plotdf <- subset(res_df, !is.na(bias))

for (out in unique(plotdf$outcome)) {

  cat("\n\n####", out, "\n\n\n")

  p <- subset(plotdf, outcome == out) %>%
    ggplot(., aes(x = variable, y = CIwidth,
                  group = interaction(model, miss_scenario, variable),
                  color = model,  fill = model)) +
    facet_wrap("miss_scenario", scales = 'free_x') +
    xlab('') +
    ylab('width of 95% CI') +
    geom_boxplot(alpha = 0.2) +
    coord_flip() +
    theme(legend.position = "top") +
    scale_color_viridis_d(end = 0.85, name = "scenario",
                          aesthetics = c("color", "fill"))

  print(p)
}




#' ## Coverage & MSE {.tabset}
#' ### Coverage
#'
subset(res_df, !is.na(covrg)) %>%
plyr::ddply(c('model', "outcome", 'variable', 'miss_scenario'), plyr::summarize,
            covrg = mean(covrg)
) %>%
  ggplot(aes(x = miss_scenario, y = covrg, color = model, group = model)) +
  geom_point() +
  geom_line() +
  facet_wrap("variable") +
  geom_hline(yintercept = 0.95, lty = 2) +
  scale_color_viridis_d(end = 0.85, name = "scenario") +
  scale_y_continuous(name = "coverage of the 95% CI",
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  theme(legend.position = "top")



subset(res_df, !is.na(covrg)) %>%
  plyr::ddply(c('model', "outcome", 'variable', 'miss_scenario'), plyr::summarize,
              covrg = mean(covrg)
  ) %>%
  ggplot(aes(x = covrg, y = variable, color = model, shape = miss_scenario,
             group = interaction(variable, model, miss_scenario))) +
  geom_point(alpha = 0.5) +
  facet_wrap("outcome", scales = "free_x") +
  geom_vline(xintercept = 0.95, lty = 2) +
  scale_color_viridis_d(end = 0.85) +
  theme(legend.position = "top")




subset(res_df, !is.na(covrg)) %>%
  plyr::ddply(c('model', "outcome", 'variable', 'miss_scenario'), plyr::summarize,
              covrg = mean(covrg)
  ) %>%
  reshape2::dcast(outcome + variable ~ model + miss_scenario,
                  value.var = "covrg") %>%
  kable(digits = 3
        # col.names = gsub(paste0("^", unique(res_df$model), "_", collapse = "|"),
                         # "", colnames(.))
  ) %>%
  kable_styling(full_width = FALSE) %>%
  # add_header_above(c(" " = 2,
  #                    sapply(sort(unique(res_df$model)), function(k) {
  #                      length(unique(res_df$miss_scenario[res_df$model == k]))
  #                    }))) %>%
  collapse_rows(columns = 1, valign = "top")



#' ### MSE
plyr::ddply(res_df, c('model', "outcome", 'variable', 'miss_scenario'),
            plyr::summarize, MSE = mean(bias^2)
) %>%
  ggplot(aes(x = miss_scenario, y = MSE, color = model, group = model)) +
  geom_point() +
  geom_line() +
  facet_wrap("variable", scales = "free_y") +
  scale_color_viridis_d(end = 0.85) +
  theme(legend.position = "top") +
  ylab("MSE")


subset(res_df, !is.na(bias)) %>%
  plyr::ddply(c('model', "outcome", 'variable', 'miss_scenario'), plyr::summarize,
              MSE = mean(bias^2)
  ) %>%
  ggplot(aes(x = MSE, y = variable, color = model, shape = miss_scenario,
             group = interaction(variable, model, miss_scenario))) +
  geom_point(alpha = 0.5) +
  facet_wrap("outcome", scales = "free_x") +
  geom_vline(xintercept = 0.95, lty = 2) +
  scale_color_viridis_d(end = 0.85) +
  theme(legend.position = "top")



plyr::ddply(res_df, c('model', "outcome", 'variable', 'miss_scenario'),
            plyr::summarize,
            MSE = mean(bias^2)
) %>%
  reshape2::dcast(outcome + variable ~ model + miss_scenario, value.var = "MSE") %>%
  kable(digits = 3
        # col.names = gsub(paste0("^", unique(res_df$model), "_", collapse = "|"),
        # "", colnames(.))
  ) %>%
  kable_styling(full_width = FALSE) %>%
  # add_header_above(c(" " = 2,
  #                    sapply(sort(unique(res_df$type)), function(k) {
  #                      length(unique(res_df$scen[res_df$type == k]))
  # }))) %>%
  collapse_rows(columns = 1, valign = "top")






#' ## MCMC criteria
#' ### Gelman-Rubin criterion
#'
#'
#+ fig.width = 8, fig.height = 8
# ggplot(subset(res_df, !is.na(`GR-crit`)),
#        aes(x = model, y = `GR-crit`, group = interaction(model, miss_scenario))) +
#   geom_violin(draw_quantiles = 0.5) +
#   geom_point(position = position_jitterdodge(jitter.width = 0.2,
#                                              dodge.width = 1),
#               aes(color = `GR-crit`)) +
#   geom_hline(yintercept = 1.2, color = grey(0.5), lty = 2) +
#   geom_hline(yintercept = 1.1, color = grey(0.5), lty = 3) +
#   scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
#                         low = "darkgreen", midpoint = 1.2) +
#   scale_y_continuous(trans = "log") +
#   ylab('Gelman-Rubin criterion') +
#   facet_wrap("outcome ~ variable")
#



ggplot(subset(res_df, !is.na(`GR-crit`)),
       aes(x = interaction(variable, outcome), y = `GR-crit`,
           group = interaction(variable, outcome, miss_scenario))) +
  facet_wrap("interaction(model, miss_scenario)") +
  geom_violin(draw_quantiles = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             aes(color = `GR-crit`)) +
  geom_hline(yintercept = 1.2, color = grey(0.5), lty = 2) +
  geom_hline(yintercept = 1.1, color = grey(0.5), lty = 3) +
  scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
                        low = "darkgreen", midpoint = 1.2) +
  scale_y_continuous(trans = "log") +
  ylab('Gelman-Rubin criterion') +
  coord_flip()



#' ### Monte-Carlo error
#+ fig.width = 8, fig.height = 8
# ggplot(subset(res_df, !is.na(`MCE/SD`)),
#        aes(x = model, y = `MCE/SD`,
#            group = miss_scenario)) +
#   # geom_violin(draw_quantiles = 0.5) +
#   geom_point(position = position_jitterdodge(jitter.width = 0.2,
#                                              dodge.width = 1),
#              aes(color = `MCE/SD`)) +
#   geom_hline(yintercept = 0.05, color = grey(0.5), lty = 2) +
#   scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
#                         low = "darkgreen", midpoint = 0.07) +
#   scale_y_continuous(trans = "log",
#                      breaks = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25),
#                      name = 'Monte-Carlo Error / Posterior SD') +
#   facet_wrap("outcome ~ variable", scales = 'free_y')
#
#


ggplot(subset(res_df, !is.na(`MCE/SD`)),
       aes(x = interaction(variable, outcome), y = `MCE/SD`,
           group = interaction(variable, outcome, miss_scenario))) +
  facet_wrap("interaction(model, miss_scenario)") +
  geom_violin(draw_quantiles = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             aes(color = `MCE/SD`)) +
  geom_hline(yintercept = 0.05, color = grey(0.5), lty = 2) +
  scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
                        low = "darkgreen", midpoint = 0.07) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25),
                     name = 'Monte-Carlo Error / Posterior SD') +
  coord_flip()




