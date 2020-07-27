
#' Run simulation
#' @param sim_params list of simulation parameters
#' @param covar_def expression defining covariates
#' @param data_params list of parameters to create covariates and outcome
#' @param models list of model specifications
#' @param path where to save the results (file name will be generated
#'             automatically)
#' @export
#'
run_sim <- function(sim_params, covar_def, data_params, models,
                    path = NULL) {

  if (!inherits(models, "list")) {
    if (inherits(models, "model_specification")) {
      models <- list(models)
    } else {
      errormsg("Please provide a list of model specifications to the argument
               %s. A model specification can be obtained using the function
               %s.", dQuote("models"), dQuote("set_model()"))
    }
  }

  oplan <- future::plan(future::sequential)
  future::plan(oplan)


  t0 <- Sys.time()
  set.seed(sim_params$global_seed)
  seeds <- sample(1:1e6, size = sim_params$nr_sims)
  sim_res <- foreach::`%dopar%`(foreach::foreach(seed = seeds), {
    data <- sim_data(covar_def, data_params, seed = seed)
    res <- fit_models(models, data_params$formula, data, seed)

    data_info <- get_data_info(data, seed)
    list(res = res, data_info = data_info)
  })

  t1 <- Sys.time()

  cpu <- if ("benchmarkme" %in% installed.packages()[, "Package"]) {
    benchmarkme::get_cpu()$model_name
  }


  file_name <- paste0("simres_", data_params$response_type, "_",
                      format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")
  out <- structure(
    list(sim_res = sim_res,
         sim_params = sim_params,
         covar_def = covar_def,
         data_params = data_params,
         models = lapply(models, "[[", "call"),
         time = t1 - t0,
         file_name = file_name,
         path = path,
         platform = list(os = Sys.info()["sysname"],
                         machine = Sys.info()["machine"],
                         r_version = R.version.string,
                         cpu = cpu,
                         future = attr(oplan[[1L]], "call")
         )
    ), class = "simulation_result")

  if (!is.null(path)) {
    save(out, file = file.path(path, file_name))
  }

  out
}


#' @export
print.simulation_result <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {

  cat("\n#------------------------------------------------------#\n")
  cat("Simulation of", x$sim_params$nr_sims, "datasets with a",
      x$data_params$response_type, "outcome.\n")
  cat(" - Computational time:",
      round(x$time, digits), attr(x$time, "units"), "\n")
  cat(" - global seed:", x$sim_params$global_seed, "\n")

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

