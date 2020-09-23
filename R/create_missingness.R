#' Create missingness in a `data.frame`
#' @inheritParams delete_MCAR
#' @param mis_scen a missingness scenario
create_missingness <- function(data, mis_scen, varlvls = NULL,
                               groups = NULL, idvars = NULL, seed = NULL) {
  if (is.null(mis_scen)) {
    return(data)
  } else {
    args <- c(list(data = data),
              mis_scen$fun_args,
              idvars = idvars,
              if (is.null(mis_scen$fun_args$varlvls)) {
                list(varlvls = varlvls)
              },
              if (is.null(mis_scen$fun_args$groups)) {
                list(groups = groups)
              },
              if (is.null(mis_scen$fun_args$seed)) {
                c(seed = seed)
              })
    do.call(mis_scen$fun, args)
  }
}
