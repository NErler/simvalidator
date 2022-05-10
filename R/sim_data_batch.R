

#' Simulate data for a batch of simulations
#'
#' @param data_seeds vector of seeds to use for generating the data
#' @param covar_def covariate definitions
#' @param outcome_pars list with setting defining the outcome (model)
#' @param packages optional names of packages needed to be passed to
#'                 `future` (for parallel processing)
#' @param mis_scenarios list of missingness scenarios
#'
#' @export

sim_data_batch <- function(data_seeds, covar_def, outcome_pars,
                           mis_scenarios, packages = NULL) {

  datlist <- lapply(data_seeds, function(data_seed) {
    future::future(packages = packages, seed = TRUE, {

      # simulate complete data
      data_orig <- sim_data(covar_def, outcome_pars, data_seed = data_seed)

      # determine grouping structure and levels of each variable
      groups <- simvalidator:::get_groups(
        setdiff(names(outcome_pars$covar_pars$group_lvls), "lvlone"),
        data_orig)

      data_lvls <- cvapply(data_orig, simvalidator:::check_varlevel,
                           groups = groups)

      compl_data_info <- get_compl_data_info(
        data_orig,
        data_seed,
        idvars = names(groups),
        data_lvls = data_lvls
      )

      # create missing values for multiple scenarios
      data_list <- lapply(names(mis_scenarios), function(scen) {

        mis_seed <- simvalidator:::next_seed(data_seed)

        data <- structure(
          create_missings(
            data_orig,
            prop_mis = mis_scenarios[[scen]]$prop_mis,
            prop_cc = mis_scenarios[[scen]]$prop_cc,
            patterns = mis_scenarios[[scen]]$patterns,
            weights = mis_scenarios[[scen]]$weights,
            idvars = setdiff(names(groups), "lvlone"),
            groups = groups, varlvls = data_lvls,
            seed = mis_seed)$data,

          scenario = scen,
          data_seed = data_seed,
          idvars = names(groups),
          data_lvls = data_lvls
        )

        # get info on incomplete data
        data_info <- simvalidator:::get_miss_data_info(
          data = data,
          data_seed = attr(data, "data_seed"),
          scen = attr(data, "scenario"),
          idvars = attr(data, "idvars"),
          data_lvls = attr(data, "data_lvls")
        )

        list(data = data, missing_data_info = data_info)
      })

      list(data_list = lapply(data_list, "[[", "data"),
           missing_data_info = lapply(data_list, "[[", "missing_data_info"),
           complete_data_info = compl_data_info)
    })

  })
  lapply(datlist, future::value)
}
