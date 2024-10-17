#' @import shiny
NULL

#' @export
ghqc_assign_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  root_dir <- rproj_root_dir()
  remote <- check_github_credentials()
  checklists <- get_valid_checklists()
  org <- get_org_errors()
  repo <- get_repo_errors(remote)
  members <- get_members_errors(org = org,
                                repo = repo)

  milestone_list <- get_open_milestone_list_errors(org = org,
                                              repo = repo)


  app <- shinyApp(
    ui = ghqc_assign_ui(
      id = "ghqc_assign_app"
    ),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app",
        remote = remote,
        root_dir = root_dir,
        checklists = checklists,
        org = org,
        repo = repo,
        members = members,
        milestone_list = milestone_list
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
