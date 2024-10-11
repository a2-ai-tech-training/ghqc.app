#' @import shiny
NULL

#' @export
ghqc_resolve_app <- function() {
  app <- shinyApp(
    ui = ghqc_resolve_ui(
      id = "ghqc_resolve_app"
    ),
    server = function(input, output, session) {
      ghqc_resolve_server(
        id = "ghqc_resolve_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
