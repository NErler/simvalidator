
#' Create a html report of a simulation result using rmarkdown
#' @param object `simulation_result` object
#' @param title not yet used
#' @param ... additional arguments passed to `rmarkdown::render()`
#' @inheritParams rmarkdown::render
#' @export
create_report <- function(object, title = NULL,
                          output_file = "simulation_report",
                          output_dir = tempdir(), ...) {

  if (inherits(object, "character")) {
    object_path <- object
    if (grepl(".RData$", object_path)) {
      object_name <- load(object_path)
      object <- get(object_name)
    } else if (length(dir(object_path)) > 0) {
      object_list <- lapply(dir(object_path, full.names = TRUE), function(x){
        obj_name <- load(x);
        get(obj_name)
      })
      object <- collapse_sim_batches(object_list)
    } else {
      if (!file.exists(object_path))
      errormsg("The path %s does not exist.", object_path)
    }
  }

  rmarkdown::render(system.file("templates", "report_template.R",
                                package = "simvalidator"),
                    params = list(object = object),
                    output_file = output_file,
                    output_dir = output_dir, ...)

  system2("open", file.path(output_dir, "simulation_report.html"))
}




collapse_sim_batches <- function(object_list) {
  sim_res <- unlist(lapply(object_list, "[[", "sim_res"), recursive = FALSE)
  compl_data_info <- unlist(lapply(object_list, "[[", "compl_data_info"),
                            recursive = FALSE)

  time <- do.call(sum, lapply(object_list, "[[", "time"))
  file_name <- cvapply(object_list, "[[", "file_name")


  l <- nlapply(setdiff(names(object_list[[1]]),
                       c("sim_res", "time", "file_name", "compl_data_info")),
               function(k) {
                 x <- unique(lapply(object_list, "[[", k))
                 if (length(x) > 1) {
                   if (length(unique(lapply(x, deparse))) > 1) {
                     errormsg("There are different versions of %s.", dQuote(k))
                   }
                 }
                 x[[1]]
               })
  c(
    list(sim_res = sim_res,
         compl_data_info = compl_data_info,
         time = time,
         file_name = file_name),
    l)
}

