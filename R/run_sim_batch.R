run_sim_batch <- function(seeds,
                          sim_pars, covar_def, outcome_pars, models,
                          mis_scenarios = NULL, scenarios, skip_fit = FALSE,
                          packages, file_name, folder, path) {

  t0 <- Sys.time()

  # simulate all complete datasets for this batch
  datlist <- sim_data_batch(data_seeds = seeds, covar_def, outcome_pars,
                            packages = packages)

  message("List of datasets generated.")


  # separate the complete data info and actual datasets
  complete_data_info <- lapply(datlist, "[[", "complete_data_info")
  missing_data_info <- lapply(datlist, "[[", "missing_data_info")
  data_list <- unlist(lapply(datlist, "[[", "data_list"), recursive = FALSE)


  # determine all the runs (combination of datasets (incl. missingness
  # scenarios) and model types):
  runs <- data.frame(data_index = seq_along(data_list),
                     data_seed = sapply(data_list, attr, "data_seed"),
                     miss_scenario = sapply(data_list, attr, "scenario"))

  runs <- merge(runs, scenarios)
  runs$runid <- seq.int(nrow(runs))


  # for each scenario,
  batch_res <- lapply(runs$runid, function(runid) {
    future::future(seed = TRUE, packages = "simvalidator", {
      if (!skip_fit) {

        model <- models[[runs$model[runid]]]

        fit <- try(simvalidator:::fit_model(model = model,
                         data = data_list[[runs$data_index[runid]]]))

        if (!inherits(fit, "try-error")) {
          simvalidator:::get_result(fitted_model = fit, model = model,
                                    run = runs[runid, ])
        } else {
          fit
        }
      }
    })
  })

  batch_res <- lapply(batch_res, future::value)

  t1 <- Sys.time()

  cpu <- if ("benchmarkme" %in% installed.packages()[, "Package"]) {
    benchmarkme::get_cpu()$model_name
  }


  file_name <- paste0(file_name, "_",
                      format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")
  out <- structure(
    list(sim_res = batch_res,
         missing_data_info = missing_data_info,
         compl_data_info = complete_data_info,
         sim_pars = sim_pars,
         covar_def = covar_def,
         mis_scenarios = mis_scenarios,
         outcome_pars = outcome_pars,
         models = models,
         time = t1 - t0,
         file_name = file_name,
         path = path,
         platform = list(os = Sys.info()["sysname"],
                         machine = Sys.info()["machine"],
                         r_version = R.version.string,
                         cpu = cpu
                         # future = attr(oplan[[1L]], "call"),
                         # workers = formals(oplan[[1L]])$workers
         )
    ), class = "simulation_result")

  if (exists("path") && !is.null(path)) {
    save(out, file = file.path(path, folder, file_name))
  }

  out
}


# run_sim_batch <- function(seeds,
#                           sim_pars, covar_def, outcome_pars, models,
#                           mis_scenarios = NULL, skip_fit = FALSE,
#                           packages, file_name, folder, path) {
#
#   oplan <- future::plan(future::sequential)
#   future::plan(oplan)
#
#   t0 <- Sys.time()
#
#   sim_res <- foreach::`%dopar%`(
#     foreach::foreach(seed = seeds,
#                      .packages = packages),
#     {
#       # simulate complete data
#       data_orig <- sim_data(covar_def, outcome_pars, seed = seed)
#
#       # determine grouping structure and levels of each variable
#       groups <- get_groups(setdiff(names(outcome_pars$covar_pars$group_lvls),
#                                    "lvlone"),
#                            data_orig)
#       data_lvls <- cvapply(data_orig, check_varlevel, groups = groups)
#
#       compl_data_info <- get_compl_data_info(
#         data_orig,
#         seed,
#         idvars = names(groups),
#         data_lvls = data_lvls
#       )
#
#       scen_res <- foreach::`%do%`(
#         foreach::foreach(scen = names(mis_scenarios),
#                          .packages = packages,
#                          .final = function(x)
#                            setNames(x, names(mis_scenarios))),
#         {
#           data <- create_missingness(data_orig, mis_scenarios[[scen]],
#                                      idvars = setdiff(names(groups), "lvlone"),
#                                      groups = groups, varlvls = data_lvls)
#
#           data_info <- get_miss_data_info(
#             data,
#             seed,
#             scen = scen,
#             idvars = names(groups),
#             data_lvls = data_lvls
#           )
#
#           res <- if (!skip_fit) {
#             fit_models(models, formula = outcome_pars$formula,
#                        data = data, seed = seed, scen = scen)
#           }
#
#           list(res = res,
#                data_info = data_info)
#         }
#       )
#
#       list(compl_data_info = compl_data_info,
#            scen_res = scen_res)
#     })
#
#   t1 <- Sys.time()
#
#   cpu <- if ("benchmarkme" %in% installed.packages()[, "Package"]) {
#     benchmarkme::get_cpu()$model_name
#   }
#
#
#   file_name <- paste0(file_name, "_",
#                       format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")
#   out <- structure(
#     list(sim_res = sim_res,
#          sim_pars = sim_pars,
#          covar_def = covar_def,
#          mis_scenarios = mis_scenarios,
#          outcome_pars = outcome_pars,
#          models = lapply(models, "[[", "call"),
#          time = t1 - t0,
#          file_name = file_name,
#          path = path,
#          platform = list(os = Sys.info()["sysname"],
#                          machine = Sys.info()["machine"],
#                          r_version = R.version.string,
#                          cpu = cpu,
#                          future = attr(oplan[[1L]], "call"),
#                          workers = formals(oplan[[1L]])$workers
#          )
#     ), class = "simulation_result")
#
#   if (exists("path") && !is.null(path)) {
#     save(out, file = file.path(path, folder, file_name))
#   }
#
#   out
# }
