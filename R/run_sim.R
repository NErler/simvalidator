
#' Run simulation
#' @param sim_pars list of simulation parameters
#' @param covar_def expression defining covariates
#' @param outcome_pars list of parameters to create covariates and outcome
#' @param models list of model specifications
#' @param mis_scenarios list of missingness scenarios
#' @param path where to save the results (file name will be generated
#'             automatically)
#' @param packages optional character vector of packages passed to `.packages`
#'                 in `foreach::foreach`
#' @param batch_size optional; number of simulations per batch (will be saved
#'                   in separate file)
#' @param sim_name optional character string used in naming the output folder
#' @param batch_numbers optional index numbers of the batches of simulations to be run
#'                        (in order to only run a subset)
#' @param skip_fit logical; should the step in which the models are fitted be
#'                 skipped (to only get the simulated data)?
#' @export
#'
run_sim <- function(sim_pars, covar_def, outcome_pars, models,
                    mis_scenarios = NULL, sim_name = NULL,
                    batch_size = NULL, batch_numbers = NULL,
                    path = NULL, packages = NULL, skip_fit = FALSE) {

  if (!inherits(models, "list")) {
    if (inherits(models, "model_specification")) {
      models <- list(models)
    } else {
      errormsg("Please provide a list of model specifications to the argument
               %s. A model specification can be obtained using the function
               %s.", dQuote("models"), dQuote("set_model()"))
    }
  }

  if (is.null(mis_scenarios)) {
    mis_scenarios <- list(complete = NULL)
  }

  set.seed(sim_pars$global_seed)
  seeds <- sample(1:1e6, size = sim_pars$nr_sims)

  if (is.null(batch_size)) {
    batch_size <- sim_pars$nr_sims
  }

  batches <- split(seeds, ceiling(seq_along(seeds)/batch_size))

  folder <- paste0("sim_", outcome_pars$response_type,
                   if (!is.null(sim_name)) paste0("_", sim_name),
                   "_", sim_pars$global_seed,
                   "-", sim_pars$nr_sims, "_", format(Sys.Date(), "%Y-%m-%d"))

  if (!dir.exists(folder)) {
    dir.create(file.path(path, folder))
  }

  if (is.null(batch_numbers) ) {
    batch_numbers <- seq_along(batches)
  }

  cat("\nResults are written to\n", file.path(path, folder), "\n")
  cat("Started at", format(Sys.time(), "%H:%M:%S"), "\n")

  for (b in batch_numbers) {
    t0 <- Sys.time()
    batch_nr <- paste0("batch-",
                       sprintf(paste0("%0", nchar(length(batches)) ,"d"), b),
                       "of",
                       sprintf(paste0("%0", nchar(length(batches)) ,"d"),
                               length(batches)))

    file_name <- paste0(c(folder, batch_nr), collapse = "_")

    run_sim_batch(seeds = batches[[b]],
                  sim_pars = sim_pars,
                  covar_def = covar_def,
                  outcome_pars = outcome_pars,
                  models = models,
                  mis_scenarios = mis_scenarios,
                  skip_fit = skip_fit, packages = packages,
                  file_name = file_name, folder = folder,
                  path = path)

    t1 <- Sys.time()
    td <- t1 - t0
    cat(batch_nr, "finished at", format(t1, "%H:%M:%S"),
        paste0("(", round(td, 2), " ", attr(td, "units"), ")"))
  }
}


run_sim_batch <- function(seeds,
                          sim_pars, covar_def, outcome_pars, models,
                          mis_scenarios = NULL, skip_fit = FALSE,
                          packages, file_name, folder, path) {

  oplan <- future::plan(future::sequential)
  future::plan(oplan)

  t0 <- Sys.time()

  datlist <- foreach::`%dopar%`(
    foreach::foreach(seed = seeds, .packages = packages),
    {
      # simulate complete data
      data_orig <- sim_data(covar_def, outcome_pars, seed = seed)

      # determine grouping structure and levels of each variable
      groups <- get_groups(setdiff(names(outcome_pars$covar_pars$group_lvls),
                                   "lvlone"),
                           data_orig)
      data_lvls <- cvapply(data_orig, check_varlevel, groups = groups)

      compl_data_info <- get_compl_data_info(
        data_orig,
        seed,
        idvars = names(groups),
        data_lvls = data_lvls
      )

      data_list <- lapply(names(mis_scenarios), function(scen) {
        list(
          data = create_missingness(data_orig, mis_scenarios[[scen]],
                                    idvars = setdiff(names(groups), "lvlone"),
                                    groups = groups, varlvls = data_lvls),
          scen = scen,
          seed = seed,
          idvars = names(groups),
          data_lvls = data_lvls
        )
      })
      list(data_list = data_list,
           compl_data_info = compl_data_info)
    })

  compl_data_info <- lapply(datlist, "[[", "compl_data_info")
  data_list <- unlist(lapply(datlist, "[[", "data_list"), recursive = FALSE)


  batch_res <- foreach::`%dopar%`(
    foreach::foreach(d_list = data_list,
                     .packages = packages),
    {
      data_info <- get_miss_data_info(
        data = d_list$data,
        seed = d_list$seed,
        scen = d_list$scen,
        idvars = d_list$idvars,
        data_lvls = d_list$data_lvls
      )

      res <- if (!skip_fit) {
        fit_models(models, formula = outcome_pars$formula,
                   data = d_list$data, seed = d_list$seed, scen = d_list$scen)
      }

      list(res = res,
           data_info = data_info)
    })


  t1 <- Sys.time()

  cpu <- if ("benchmarkme" %in% installed.packages()[, "Package"]) {
    benchmarkme::get_cpu()$model_name
  }


  file_name <- paste0(file_name, "_",
                      format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")
  out <- structure(
    list(sim_res = batch_res,
         compl_data_info = compl_data_info,
         sim_pars = sim_pars,
         covar_def = covar_def,
         mis_scenarios = mis_scenarios,
         outcome_pars = outcome_pars,
         models = lapply(models, "[[", "call"),
         time = t1 - t0,
         file_name = file_name,
         path = path,
         platform = list(os = Sys.info()["sysname"],
                         machine = Sys.info()["machine"],
                         r_version = R.version.string,
                         cpu = cpu,
                         future = attr(oplan[[1L]], "call"),
                         workers = formals(oplan[[1L]])$workers
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



#' @export
print.simulation_result <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {

  cat("\n#------------------------------------------------------#\n")
  cat("Simulation of", x$sim_pars$nr_sims, "datasets with a",
      x$outcome_pars$response_type, "outcome.\n")
  cat(" - Computational time:",
      round(x$time, digits), attr(x$time, "units"),
      if (!is.null(x$platform$workers)) {
        paste0("(", x$platform$workers, " workers)")
      },
      "\n")
  cat(" - global seed:", x$sim_pars$global_seed, "\n")

  cat("\nModels fitted:\n")
  models <- vapply(x$models, function(k) {
    paste0("\r - ", deparse(k$fun, width.cutoff = 500L), "\n")
  }, character(1L))
  cat(models)

  cat("\nSystem:\n")
  cat(" -", x$platform$r_version, "\n")
  cat(" - os:", x$platform$os, x$platform$machine, "\n")
  cat(" - cpu:",
      if (is.null(x$platform$cpu)) {
        "unknown"
      } else {
        x$platform$cpu
      }, "\n")

  if (!is.null(x$path)) {
    cat("\nThe simulation was saved to\n",
        file.path(x$path, x$file_name), "\n")
  }
  cat("\n#------------------------------------------------------#\n")
}

