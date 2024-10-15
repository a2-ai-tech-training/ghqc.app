#' @import shiny
NULL

#' @export
ghqc_resolve_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  remote <- check_github_credentials()

  app <- shinyApp(
    ui = ghqc_resolve_ui(
      id = "ghqc_resolve_app"
    ),
    server = function(input, output, session) {
      ghqc_resolve_server(
        id = "ghqc_resolve_app",
        remote = remote
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
