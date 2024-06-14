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
