#' Run squeezeMetaR Shiny app
#' @export
run_app <- function() {

  app_dir <- system.file("shiny", package = "squeezeMetaR")

  if (app_dir == "") {
    stop("Shiny app directory not found.")
  }

  # Forzar navegador sin depender de getOption("browser")
  shiny::runApp(
    app_dir,
    launch.browser = function(url) {
      system2("xdg-open", url)
    }
  )
}
