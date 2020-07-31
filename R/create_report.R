
#' Create a html report of a simulation result using rmarkdown
#' @param object `simulation_result` object
#' @param title not yet used
#' @inheritParams rmarkdown::render
#' @export
create_report <- function(object, title = NULL, output_file = "simulation_report",
                        output_dir = tempdir(), ...) {

  rmarkdown::render("inst/report_template.R",
                    params = list(object = object),
                    output_file = output_file,
                    output_dir = output_dir, ...)

  system2("open", file.path(output_dir, "report_template.html"))
}