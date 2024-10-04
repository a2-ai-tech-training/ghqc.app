#' @import shiny
NULL

#' @export
ghqc_create_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()
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
