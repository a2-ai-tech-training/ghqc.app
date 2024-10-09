#' @import shiny
NULL

#' @export
ghqc_comment_app <- function() {
  app <- shinyApp(
    ui = ghqc_comment_ui(
      id = "ghqc_comment_app"
    ),
    server = function(input, output, session) {
      ghqc_comment_server(
        id = "ghqc_comment_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
