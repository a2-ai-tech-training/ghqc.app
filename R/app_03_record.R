#' @import shiny
NULL

#' @export
ghqc_record_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  remote <- check_github_credentials()

  app <- shinyApp(
    ui = ghqc_record_ui(
      id = "ghqc_record_app"
    ),
    server = function(input, output, session) {
      ghqc_record_server(
        id = "ghqc_record_app",
        remote = remote
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5256))
  runApp(app, port = port)
}
