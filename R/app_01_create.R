#' @import shiny
NULL

#' @export
ghqc_create_app <- function() {
  info(.le$logger, do.call(getExportedValue(.lci$client_pkg_name, "load_logo_png_shiny")))
  app <- shinyApp(
    ui = ghqc_create_ui(
      id = "ghqc_create_app"
    ),
    server = function(input, output, session) {
      ghqc_create_server(
        id = "ghqc_create_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
