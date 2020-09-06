
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
#' @export
#'
run_sim <- function(sim_pars, covar_def, outcome_pars, models,
                    mis_scenarios = NULL,
                    path = NULL, packages = NULL) {

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

  oplan <- future::plan(future::sequential)
  future::plan(oplan)

  t0 <- Sys.time()
  set.seed(sim_pars$global_seed)
  seeds <- sample(1:1e6, size = sim_pars$nr_sims)
  sim_res <- foreach::`%dopar%`(
    foreach::foreach(seed = seeds,
                     .packages = packages),
    {
      # simulate complete data
      data_orig <- sim_data(covar_def, outcome_pars, seed = seed)

      # determine groupin structure and levels of each variable
      groups <- get_groups(setdiff(names(outcome_pars$covar_pars$group_lvls),
                                   "lvlone"),
                           data_orig)
      data_lvls <- cvapply(data_orig, check_varlevel, groups = groups)

      foreach::`%dopar%`(
                 foreach::foreach(mis_scen = mis_scenarios,
                                  .packages = packages),
                 {
                   data <- create_missingness(data_orig, mis_scen)
                   res <- fit_models(models, formula = outcome_pars$formula,
                                     data = data, seed = seed)
                   data_info <- get_data_info(
                     data,
                     seed,
                     idvars = names(outcome_pars$covar_pars$group_lvls),
                     data_lvls = data_lvls
                   )
                   list(res = res,
                        data_info = data_info)
                 }
               )
    })

  t1 <- Sys.time()

  cpu <- if ("benchmarkme" %in% installed.packages()[, "Package"]) {
    benchmarkme::get_cpu()$model_name
  }


  file_name <- paste0("simres_", outcome_pars$response_type, "_",
                      format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")
  out <- structure(
    list(sim_res = sim_res,
         sim_pars = sim_pars,
         covar_def = covar_def,
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
    save(out, file = file.path(path, file_name))
  }

  out
}


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

