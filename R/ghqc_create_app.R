#' @import shiny
NULL

# #' #' @export
#' ghqc_create_app <- function() {
#'   runGadget(
#'     app = ghqc_create_ui(
#'       id = "ghqc_create_app"),
#'     server = function(input, output, session) {
#'       ghqc_create_server(
#'         id = "ghqc_create_app"
#'       )
#'     },
#'     viewer = paneViewer()
#'   )
#' }

#' @export
ghqc_create_app <- function() {
  app <- shinyApp(
    ui = ghqc_create_ui(
      id = "ghqc_create_app"),
    server = function(input, output, session) {
      ghqc_create_server(
        id = "ghqc_create_app"
      )
    }
  )
  runApp(app, port = as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454)))
}
