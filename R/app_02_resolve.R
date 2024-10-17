#' @import shiny
NULL

#' @export
#' @import log4r
ghqc_resolve_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  remote <- check_github_credentials()
  org <- get_org_errors()
  repo <- get_repo_errors(remote)
  milestone_list <- get_open_milestone_list_errors(org = org, repo = repo)

  if (length(milestone_list) == 0) {
    error(.le$logger, glue::glue("There were no open Milestones found in {org}/{repo}"))
    rlang::abort("No open Milestones found")
  }

  # error if no open milestones
  if (length(milestone_list) == 0) {
    error(.le$logger, glue::glue("There were no open milestones found in {org}/{repo}. Please use the Assign app before using the Resolve app."))
    rlang::abort("There were no open milestones found.")
  }

  app <- shinyApp(
    ui = ghqc_resolve_ui(
      id = "ghqc_resolve_app"
    ),
    server = function(input, output, session) {
      ghqc_resolve_server(
        id = "ghqc_resolve_app",
        remote = remote,
        org = org,
        repo = repo,
        milestone_list = milestone_list
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
