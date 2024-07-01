#' @import shiny
NULL

#' @export
ghqc_create_app <- function() {
  runGadget(
    app = ghqc_create_ui(
      id = "ghqc_create_app"),
    server = function(input, output, session) {
      ghqc_create_server(
        id = "ghqc_create_app"
      )
    },
    viewer = paneViewer()
  )
}
