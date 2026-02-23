#' Run SQMLauncher Shiny App
#'
#' Launches the SQMLauncher graphical interface.
#'
#' @export
run_app <- function() {

  app_dir <- system.file("shiny", package = "squeezeMetaR")

  if (app_dir == "") {
    stop("Could not find the Shiny app directory.")
  }

  # Prevent browseURL errors
  shiny::runApp(
    appDir = app_dir,
    launch.browser = interactive()
  )
}
