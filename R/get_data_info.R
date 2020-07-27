#
# data_info <- data.frame(seed = seed,
#                         Norig = nrow(dat_orig),
#                         N = nrow(dat),
#                         event = sum(dat$event),
#                         censored = sum(1 - dat$event))




#' Summarize information of the simulated data
#' @param data a `data.frame`
#' @param seed the seed value
#' @param ... optional additional arguments
#' @export
get_data_info <- function(data, seed, ...) {
  list(seed = seed,
       nrow = nrow(data),
       perc_missing = colMeans(is.na(data)),
       summary = get_summary(data)
  )
}

get_summary <- function(data) {
  lapply(data, function(x) {
    if (inherits(x, "factor")) {
      as.data.frame(table(x, exclude = NULL))
    } else if (inherits(x, "numeric")) {
      summary(x)
    }
  })
}
