#' Simvalidator: validation of model performance via simulation
#'
#' Functions to run simulations and summarize and visualize the results
#'
#' @import graphics
#' @import utils
#' @import stats
#' @import ggplot2
#' @importFrom magrittr %>%
#' @docType package
#' @name simvalidator
NULL



utils::globalVariables(c("seed", "value", "mis_scen", ".", "x", "y",
                         "L1", "category", "Freq", "scen"))



#' @export
magrittr::`%>%`
