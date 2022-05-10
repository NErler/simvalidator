#' Create MAR missingness
#' @inheritParams delete_MCAR
#' @export

delete_MAR <- function(data, missing, idvars = NULL, varlvls = NULL,
                       groups = NULL, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
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
    desgn_mat <- model.matrix(data = model.frame(formula = missing[[k]]$formula,
                                          data = data, na.action = na.pass),
                              object = missing[[k]]$formula)

    lp0 <- calc_lp(desgn_mat, missing[[k]]$params)
    lp <- nvapply(split(lp0, groups[[varlvls[k]]]), mean, na.rm = TRUE)

    intercept <- mean(logit(missing[[k]]$prop) - lp0)

    p_NA <- rbinom(max(groups[[varlvls[k]]]), size = 1,
                   prob = plogis(lp + intercept))
    data[groups[[varlvls[k]]] %in% which(p_NA == 1), k] <- NA
  }

  data
}

logit <- function(x) {
  log(x/(1 - x))
}

calc_lp <- function(mat, coef) {
  mat[, intersect(colnames(mat), names(coef))] %*%
    coef[intersect(colnames(mat), names(coef))]
}
