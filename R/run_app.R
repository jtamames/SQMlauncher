#' Run SQMLauncher Shiny App
#' @export
run_app <- function(host = "127.0.0.1", port = NULL) {

  app_dir <- system.file("shiny", package = "squeezeMetaR")

  if (app_dir == "") {
    stop("Could not find the Shiny app directory.")
  }

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = FALSE
  )
}
