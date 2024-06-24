#' @import shiny
NULL

#' @export
ghqc_update_app <- function() {
  runGadget(
    app = ghqc_update_ui(
      id = "ghqc_update_app"),
    server = function(input, output, session) {
      ghqc_update_server(
        id = "ghqc_update_app"
      )
    },
    viewer = paneViewer()
  )
}
