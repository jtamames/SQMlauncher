
#' Run squeezeMetaR Shiny app
#' @export
run_app <- function() {

  if (is.null(getOption("browser")) || getOption("browser") == "") {
    options(browser = "xdg-open")
  }

  shiny::runApp(
    system.file("shiny", package = "squeezeMetaR"),
    launch.browser = TRUE
  )
}
