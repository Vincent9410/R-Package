#' Launch the ausbushfire Shiny app
#'
#' Starts the Shiny application bundled with the package (located under
#' \code{inst/shiny}). This is the recommended way for users to run the app after
#' installing the package.
#'
#' @return Invisibly returns the app object (called for side effects).
#' @export
run_ausbushfire_app <- function() {
  appDir <- system.file("shiny", package = "ausbushfire")

  shiny::runApp(appDir)
}
