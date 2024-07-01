#' @import shiny
NULL

#' @export
ghqc_app <- function() {
  runGadget(
    app = ghqc_ui(
      id = "ghqc_app"),
    server = function(input, output, session) {
      ghqc_server(
        id = "ghqc_app"
      )
    },
    viewer = paneViewer()
  )
}

# ghqc_app <- function() {
#   ui <- ghqc_ui(
#     id = "ghqc_app")
#
#   server <- function(input, output, session) {
#     ghqc_server(
#       id = "ghqc_app"
#     )
#   }
#   shiny::runApp(list(ui = ui, server = server), port = as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454)))
# }

# ghqc_app <- function() {
#   #.libPaths("~/ghqc-rpkgs/")
#
#   runApp(list(
#     ui = ghqc_ui(
#       id = "ghqc_app"),
#     server = function(input, output, session) {
#       ghqc_server(
#         id = "ghqc_app"
#       )
#     },
#     #viewer = rstudioapi::viewer(sprintf("https://127.0.0.1:%s", port)),
#     port = as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
#     #shiny::runApp(list(ui = ui, server = server), port = as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454)))
#   )
#   )
# }
