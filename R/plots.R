#' # object = sim_out
#'
#' plotfun = function(object) {
#' res_df <- get_res_df(object)
#'
#' library(ggplot2)
#'
#' ggplot(res_df, aes(x = type, y = Mean)) +
#'   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#'   facet_wrap("variable", scales = 'free') +
#'   geom_hline(aes(yintercept = regcoef), linetype = 2, size = 1) +
#'   geom_jitter(height = 0, width = 0.2, alpha = 0.3)
#'
#'
#' ggplot(res_df, aes(x = type, y = bias)) +
#'   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#'   facet_wrap("variable", scales = 'free') +
#'   geom_hline(yintercept = 0, linetype = 2, size = 1) +
#'   geom_jitter(height = 0, width = 0.2, alpha = 0.3)
#'
#'
#' ggplot(res_df, aes(x = type, y = relbias)) +
#'   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#'   facet_wrap("variable", scales = 'free') +
#'   geom_hline(yintercept = 0, linetype = 2, size = 1) +
#'   geom_jitter(height = 0, width = 0.2, alpha = 0.3)
#'
#'
#' ggplot(res_df, aes(x = type, y = CIwidth)) +
#'   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#'   facet_wrap("variable", scales = 'free') +
#'   geom_jitter(height = 0, width = 0.2, alpha = 0.3)
#'
#'
#' #' ### Coverage and MSE
#' library(kableExtra)
#' library(plyr)
#' ddply(res_df, c('type', 'variable'), summarize,
#'       covrg = mean(covrg),
#'       MSE = mean(bias^2)) %>%
#'   kable(digits = 3) %>%
#'   kable_styling(full_width = FALSE)
#'
#'
#'
#' ggplot(subset(res_df, !is.na(`GR-crit`)), aes(x = variable, y = `GR-crit`)) +
#'   geom_violin(draw_quantiles = 0.5) +
#'   geom_hline(yintercept = 1.2, color = grey(0.5), lty = 2) +
#'   geom_hline(yintercept = 1.1, color = grey(0.5), lty = 3) +
#'   ylab('Gelman-Rubin criterion') +
#'   facet_wrap("type")
#'
#'
#'
#' ggplot(subset(res_df, !is.na(`MCE/SD`)), aes(x = variable, y = `MCE/SD`)) +
#'   geom_violin(draw_quantiles = 0.5) +
#'   geom_hline(yintercept = 0.05, color = grey(0.5), lty = 2) +
#'   ylab('Gelman-Rubin criterion') +
#'   facet_wrap("type")
#'
#' }
