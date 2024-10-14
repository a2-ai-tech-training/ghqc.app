#' @import shiny
NULL

#' @export
<<<<<<< HEAD:R/app_01_create.R
ghqc_create_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()
=======
ghqc_assign_app <- function() {
>>>>>>> main:R/app_01_assign.R
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
