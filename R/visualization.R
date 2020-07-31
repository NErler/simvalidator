vis_basehaz <- function(knot_range,
                        beta_Bh0 = 0.1 * c(-1.3, -5.1, -3.8, -5.1, -3.3, -2),
                        times = seq(0, 10, by = 0.2), matplot = TRUE, ...) {
  kn <- get_knots_h0(nkn = 2,
                     Time = knot_range[1]:knot_range[2],
                     gkx = gauss_kronrod()$gkx)
  kn[length(kn)] <- 100 * kn[length(kn)]

  desgn_mat_basehaz <- splines::splineDesign(kn, times, ord = 4L,
                                             outer.ok = TRUE)

  basehaz <- c(desgn_mat_basehaz %*% beta_Bh0)


  if (matplot) {
    matplot(times, t(t(desgn_mat_basehaz) * beta_Bh0), type = 'l', ylim = range(1, basehaz),
            col = 1, lty = 2)
    lines(times, basehaz, col = 2)
  } else {
    plot(times, basehaz, col = 2)
  }
  # abline(v = kn, lty = 2)
}



viz_survdat <- function(data) {
  dat_list <- list(true_times = attr(data, "true_times"),
               cens_times = attr(data, "cens_times"),
               obs_times = if (!is.null(attr(data, "timevar"))) {
                 data$etime[!duplicated(data$id)]
               },
               tvar_times = if (!is.null(attr(data, "timevar"))) {
                 data[[attr(data, "timevar")]]
               }
  )
  mdat <- reshape2::melt(Filter(Negate(is.null), dat_list), id.vars = NULL)

  ggplot(mdat, aes(x = value)) +
    geom_histogram(bins = 30, alpha = 0.5, color = "black") +
    facet_wrap("L1", scales = 'free')
}
