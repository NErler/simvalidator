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
library(ggplot2)
library(kableExtra)


theme_set(theme_gray())
theme_update(panel.grid = element_blank(),
             panel.background = element_rect(fill = grey(0.98)),
             panel.border = element_rect(fill = 'transparent',
                                         color = grey(0.85))
)



object <- params$object
res_df <- get_res_df(object)


outvars <- lapply(check_formula_list(object$outcome_pars$formula),
                  function(fmla) {
                    fmla[3] <- 1
                    JointAI::all_vars(fmla)
                  }) %>%
  unlist

data_summary <- lapply(object$compl_data_info, "[[", "summary")


#' # Data
#' Target sample size per simulated dataset:
object$outcome_pars$N %>%
  kable(col.names = " ") %>%
  kable_styling(full_width = FALSE)

#+ samplesize, fig.height = 2, fig.cap = figcap
figcap <- "Sample size per data level (multi-level data) and simulated dataset.
In models for time-to-event outcomes the size of the simulated data can differ
from the target size since repeated measurements of covariates are simulated
first and deleted after simulation of the time-to-event outcome if they occur
after the event / censoring time."
reshape2::melt(
  lapply(object$compl_data_info, function(x) {
    data.frame(t(x$size))
  }), id.vars = NULL) %>%
  ggplot(aes(y = 1, x = value)) +
  geom_violin() +
  geom_jitter(width = 0, height = 0.2, alpha = 0.2) +
  facet_wrap("variable", scales = 'free') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  xlab("sample size")


#' ### Model formula
object$outcome_pars$formula

#'
#' ## Outcome {.tabset}
#+ outvarsplots, results = "asis"
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
lapply(object$sim_res, function(x) {
  cbind(data.frame(t(unlist(unname(x$data_info$perc_missing)))),
        scen = x$data_info$scen)
}) %>% reshape2::melt(id.vars = "scen") %>%
  ggplot(aes(y = variable, x = value,
             group = interaction(variable, factor(scen)),
             fill = scen, color = scen)) +
  geom_boxplot(alpha = 0.2) +
  # geom_jitter(alpha = 0.2) +
  scale_x_continuous(name = "missing values (%)", limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  ylab("")



#' # Results
#' ## Bias and CI width {.tabset}
n_rows <- ceiling(nrow(unique(subset(res_df, !is.na(bias),
                                     select = c("variable", "outcome"))))/4)
#' ### absolute bias {.tabset .tabset-pills}
#+ fig.width = 8, fig.height = n_rows * 2 + 0.5
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = bias, group = interaction(type, scen),
           color = scen, fill = scen)) +
  facet_wrap("outcome ~ variable", scales = 'free', ncol = 4) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'darkgreen') +
  xlab('') +
  ylab('absolute bias') +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.2) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             alpha = 0.4) +
  theme(legend.position = "top") +
  scale_color_viridis_d(end = 0.85, name = "scenario",
                        aesthetics = c("color", "fill"))


#+ results = "asis", fig.width = 10, fig.height = 8
for (out in unique(res_df$outcome)) {

  cat("\n\n####", out, "\n\n")

  p <- ggplot(subset(res_df, !is.na(bias) & outcome == out),
         aes(x = type, y = bias, group = interaction(type, scen),
             color = scen, fill = scen)) +
    facet_wrap("variable", scales = 'free', ncol = 4) +
    geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'darkgreen') +
    xlab('') +
    ylab('absolute bias') +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.2) +
    geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                               dodge.width = 1),
               alpha = 0.4) +
    theme(legend.position = "top") +
    scale_color_viridis_d(end = 0.85, name = "scenario",
                          aesthetics = c("color", "fill"))

    print(p)
}



#' ### relative bias
#+ relbias, fig.width = 8, fig.height = n_rows * 2 + 0.5, eval = nrow(res_df) > 0
ggplot(subset(res_df, !is.na(relbias)),
       aes(x = type, y = relbias,
           group = interaction(type, scen),
           color = scen, fill = scen)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.2) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             alpha = 0.4) +
  theme(legend.position = "top") +
  scale_color_viridis_d(end = 0.85, name = "scenario",
                        aesthetics = c("color", "fill")) +
  facet_wrap("outcome ~ variable", scales = 'free', ncol = 4) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'darkgreen') +
  xlab('') +
  ylab('relative bias')


#' ### parameter estimate
#+ estim, fig.width = 8, fig.height = n_rows * 2 + 0.5, eval = nrow(res_df) > 0
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = Mean, group = interaction(type, scen),
           fill = scen, color = scen)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.2) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             alpha = 0.4) +
  theme(legend.position = "top") +
  scale_color_viridis_d(end = 0.85, name = "scenario",
                        aesthetics = c("color", "fill")) +
  facet_wrap("outcome ~ variable", scales = 'free_y', ncol = 4) +
  geom_hline(aes(yintercept = true_param),
             linetype = 2, size = 1, color = 'darkgreen') +
  xlab('') +
  ylab('parameter estimate')


#' ### CI width
#+ CIwidth, fig.width = 8, fig.height = 8, eval = nrow(res_df) > 0
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = CIwidth, group = interaction(type, scen),
           fill = scen, color = scen)) +
  facet_wrap("outcome ~ variable", scales = 'free') +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.2) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             alpha = 0.4) +
  theme(legend.position = "top") +
  scale_color_viridis_d(end = 0.85, name = "scenario",
                        aesthetics = c("color", "fill")) +
  xlab('') +
  ylab('width of 95% CI')


#' ## Coverage & MSE {.tabset}
#' ### Coverage
#'
#+ covrg, eval = nrow(res_df) > 0
subset(res_df, !is.na(covrg)) %>%
plyr::ddply(c('type', "outcome", 'variable', 'scen'), plyr::summarize,
            covrg = mean(covrg)
) %>%
  ggplot(aes(x = scen, y = covrg, color = type, group = type)) +
  geom_point() +
  geom_line() +
  facet_wrap("variable") +
  geom_hline(yintercept = 0.95, lty = 2) +
  scale_y_continuous(name = "coverage of the 95% CI",
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1))

subset(res_df, !is.na(covrg)) %>%
  plyr::ddply(c('type', "outcome", 'variable', 'scen'), plyr::summarize,
        covrg = mean(covrg)
        ) %>%
  reshape2::dcast(outcome + variable ~ type + scen, value.var = "covrg") %>%
  kable(digits = 3,
        col.names = gsub(paste0("^", unique(res_df$type), "_", collapse = "|"),
                                     "", colnames(.))
        ) %>%
  kable_styling(full_width = FALSE) %>%
    add_header_above(c(" " = 2,
                       sapply(sort(unique(res_df$type)), function(k) {
                         length(unique(res_df$scen[res_df$type == k]))
                       }))) %>%
  collapse_rows(columns = 1, valign = "top")



#' ### MSE
#+ MSE, eval = nrow(res_df) > 0
plyr::ddply(res_df, c('type', "outcome", 'variable', 'scen'), plyr::summarize,
            MSE = mean(bias^2)
) %>%
  ggplot(aes(x = scen, y = MSE, color = type, group = type)) +
  geom_point() +
  geom_line() +
  facet_wrap("variable", scales = "free_y") +
  ylab("MSE")


plyr::ddply(res_df, c('type', "outcome", 'variable', 'scen'), plyr::summarize,
            MSE = mean(bias^2)
) %>%
  reshape2::dcast(outcome + variable ~ type + scen, value.var = "MSE") %>%
  kable(digits = 3, col.names = gsub(paste0("^", unique(res_df$type), "_", collapse = "|"),
                                     "", colnames(.))) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 2,
                     sapply(sort(unique(res_df$type)), function(k) {
                       length(unique(res_df$scen[res_df$type == k]))
                     }))) %>%
  collapse_rows(columns = 1, valign = "top")



#' ## MCMC criteria
#' ### Gelman-Rubin criterion
#+ GRcrit, fig.width = 8, fig.height = 8, eval = nrow(res_df) > 0
ggplot(subset(res_df, !is.na(`GR-crit`)),
       aes(x = type, y = `GR-crit`, group = interaction(type, scen))) +
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
  facet_wrap("outcome ~ variable")



ggplot(subset(res_df, !is.na(`GR-crit`)),
       aes(x = variable, y = `GR-crit`, group = interaction(scen, variable))) +
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
  facet_grid("outcome ~ scen", scales = "free_y") +
  coord_flip()



#' ### Monte-Carlo error
#+ MCE, fig.width = 8, fig.height = 8, eval = nrow(res_df) > 0
ggplot(subset(res_df, !is.na(`MCE/SD`)),
       aes(x = type, y = `MCE/SD`,
           group = scen)) +
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
  facet_wrap("outcome ~ variable", scales = 'free_y')



ggplot(subset(res_df, !is.na(`MCE/SD`)),
       aes(y = `MCE/SD`, x = variable, group = interaction(scen, variable))) +
  geom_violin(draw_quantiles = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 1),
             aes(color = `MCE/SD`), alpha = 0.5) +
  coord_flip() +
  scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
                        low = "darkgreen", midpoint = 0.07) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25),
                     name = 'Monte-Carlo Error / Posterior SD') +
  geom_hline(yintercept = 0.05, color = grey(0.5), lty = 2) +
  facet_grid("outcome ~ scen", scales = "free_y")



#' # Technical Stuff
if (!is.null(object$compl_data_info[[1]]$nr_tries)) {
  reshape2::melt(
    lapply(object$compl_data_info, "[[", "nr_tries")
  ) %>%
    dplyr::mutate(value = factor(value, levels = rev(sort(unique(value))))) %>%
    ggplot(aes(x = factor(L1), fill = value)) +
    geom_bar() +
    xlab("Simulation Nr.") +
    scale_fill_viridis_d(name = "nr_tries") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "top")
}

if (nrow(res_df) > 0) {
  unique(subset(res_df,
                select = c("n_iter", "seed", "n_chain"),
                !is.na(n_iter))) %>%
    ggplot(aes(x = factor(n_chain), y = n_iter)) +
    geom_violin() +
    geom_jitter(width = 0.2, height = 0)
}
