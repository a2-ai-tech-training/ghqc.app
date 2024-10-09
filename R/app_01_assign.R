#' @import shiny
NULL

#' @export
ghqc_assign_app <- function() {
  # check for errors
  app <- shinyApp(
    ui = ghqc_assign_ui(
      id = "ghqc_assign_app"
    ),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
