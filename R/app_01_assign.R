#' @import shiny
NULL

#' @export
ghqc_assign_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  root_dir <- rproj_root_dir()
  remote <- check_github_credentials()

  app <- shinyApp(
    ui = ghqc_assign_ui(
      id = "ghqc_assign_app"
    ),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app",
        remote = remote,
        root_dir = root_dir
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
