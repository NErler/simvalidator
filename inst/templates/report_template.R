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


#' ## Bias and CI width {.tabset}
n_rows <- ceiling(nrow(unique(subset(res_df, !is.na(bias),
                                     select = c("variable", "outcome"))))/4)
#' ### absolute bias
#+ fig.width = 8, fig.height = n_rows * 2
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = bias)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap("outcome ~ variable", scales = 'free', ncol = 4) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'darkgreen') +
  geom_jitter(height = 0, width = 0.2, alpha = 0.3) +
  xlab('') +
  ylab('absolute bias')


#' ### relative bias
#+ fig.width = 8, fig.height = n_rows * 2
ggplot(subset(res_df, !is.na(relbias)),
       aes(x = type, y = relbias)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap("outcome ~ variable", scales = 'free', ncol = 4) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'darkgreen') +
  geom_jitter(height = 0, width = 0.2, alpha = 0.3) +
  xlab('') +
  ylab('relative bias')


#' ### parameter estimate
#+ fig.width = 8, fig.height = n_rows * 2
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = Mean)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap("outcome ~ variable", scales = 'free', ncol = 4) +
  geom_hline(aes(yintercept = true_param),
             linetype = 2, size = 1, color = 'darkgreen') +
  geom_jitter(height = 0, width = 0.2, alpha = 0.3) +
  xlab('') +
  ylab('parameter estimate')


#' ### CI width
#+ fig.width = 8, fig.height = 8
ggplot(subset(res_df, !is.na(bias)),
       aes(x = type, y = CIwidth)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap("outcome ~ variable", scales = 'free') +
  geom_jitter(height = 0, width = 0.2, alpha = 0.3) +
  xlab('') +
  ylab('width of 95% CI')


#' ## Coverage & MSE
#' ### Coverage and MSE
subset(res_df, !is.na(covrg)) %>%
  plyr::ddply(c('type', "outcome", 'variable'), plyr::summarize,
        covrg = mean(covrg),
        MSE = mean(bias^2)) %>%
  kable(digits = 3) %>%
  kable_styling(full_width = FALSE)


#' ## MCMC criteria
#' ### Gelman-Rubin criterion
#+ fig.width = 8, fig.height = 8
ggplot(subset(res_df, !is.na(`GR-crit`)),
       aes(x = type, y = `GR-crit`)) +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(height = 0, alpha = 0.3, width = 0.3,
              aes(color = `GR-crit`)) +
  geom_hline(yintercept = 1.2, color = grey(0.5), lty = 2) +
  geom_hline(yintercept = 1.1, color = grey(0.5), lty = 3) +
  scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
                        low = "darkgreen", midpoint = 1.2) +
  ylab('Gelman-Rubin criterion') +
  facet_wrap("outcome ~ variable")


#' ### Monte-Carlo error
#+ fig.width = 8, fig.height = 8
ggplot(subset(res_df, !is.na(`MCE/SD`)), aes(x = type, y = `MCE/SD`)) +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(height = 0, alpha = 0.3, width = 0.3,
              aes(color = `MCE/SD`)) +
  geom_hline(yintercept = 0.05, color = grey(0.5), lty = 2) +
  scale_color_gradient2(high = scales::muted("red"), mid = "yellow",
                        low = "darkgreen", midpoint = 0.06) +
  ylab('Monte-Carlo Error / Posterior SD') +
  facet_wrap("outcome ~ variable", scales = 'free_y')
