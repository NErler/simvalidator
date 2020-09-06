#
# data_info <- data.frame(seed = seed,
#                         Norig = nrow(dat_orig),
#                         N = nrow(dat),
#                         event = sum(dat$event),
#                         censored = sum(1 - dat$event))




#' Summarize information of the simulated data
#' @param seed the seed value
#' @param data_lvls named vector giving the level of each variable
#' @inheritParams delete_MCAR
#' @param ... optional additional arguments
#' @export
get_data_info <- function(data, seed, idvars, data_lvls, ...) {
  list(seed = seed,
       perc_missing = colMeans(is.na(data)),
       summary = get_summary(data, idvars, data_lvls),
       size = get_size(data, idvars),
       nr_tries = attr(data, "nr_tries")
  )
}

get_summary <- function(data, idvars = NULL, data_lvls = NULL) {

  rows <- nlapply(idvars, function(id) {
    match(unique(cbind(data,
                       lvlone = 1:nrow(data))[[id]]),
          cbind(data, lvlone = 1:nrow(data))[[id]])
  })

  nlapply(names(data), function(k) {
    x <- data[rows[[data_lvls[k]]], k]
    if (inherits(x, "factor") | inherits(x, "logical")) {
      prop.table(table(category = x, exclude = NULL))
    } else if (inherits(x, "numeric")) {
      density(x, n = 50)[c("x", "y")]
    }
  })
}

get_size <- function(data, idvars) {
  ivapply(idvars, function(id) {
    if (id %in% names(data)) {
      length(unique(data[[id]]))
    } else if (id == "lvlone") {
      nrow(data)
    } else {
      errormsg("The variable %s is not part of the data.",
               dQuote(id))
    }
  })
}
