
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
    object_name <- load(object_path)
    object <- get(object_name)
  }

  rmarkdown::render(system.file("templates", "report_template.R",
                                package = "simvalidator"),
                    params = list(object = object),
                    output_file = output_file,
                    output_dir = output_dir, ...)

  system2("open", file.path(output_dir, "simulation_report.html"))
}
