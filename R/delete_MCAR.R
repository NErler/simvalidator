#' Create MCAR missingness
#' @param data a `data.frame`
#' @param missing a named vector giving the names of covariates and proportion
#'   of values to be deleted
#' @param idvars optional vector with names of ID variables that should be used
#'   to identify levels in a multi-level setting
#' @param varlvls named vector giving the level of each variable
#' @param groups list with grouping information
#' @param mis_seed optional seed value
#'
#' @export

delete_MCAR <- function(data, missing, idvars = NULL, varlvls = NULL,
                        groups = NULL, mis_seed = NULL) {

  if (!is.null(mis_seed)) {
    set.seed(mis_seed)
  }

  ## identify groups and variable levels
  if (is.null(groups)) {
    groups <- get_groups(idvars, data)
  }
  if (is.null(varlvls)) {
    varlvls <- cvapply(data, check_varlevel, groups = groups)
  }

  ## randomly select observations and set them to NA
  for (k in names(missing)) {
    p_NA <- rbinom(max(groups[[varlvls[k]]]), size = 1, prob = missing[k])
    data[groups[[varlvls[k]]] %in% which(p_NA == 1), k] <- NA
  }

  data
}
