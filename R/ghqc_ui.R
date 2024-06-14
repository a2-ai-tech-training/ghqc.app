#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_preloader
NULL

#' @export
ghqc_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    #tags$head(tags$style(HTML(brio::read_file(system.file("css/styles.css", package = "ghqc"))))),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc/css/styles.css")),
    waiter_preloader(
      html = tagList(
        spin_1(),
        h4("Loading in ...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      div(id=ns("title_bar"),
           gadgetTitleBar(title = span(img(src = "ghqc/assets/logo.png", height = 50), "QC Shiny Tool"))
      ),
      div(
        id = ns("content"),
        uiOutput(ns("sidebar")),
        uiOutput(ns("main_panel"))
      ),
      div(
        id=ns("button_block"),
        miniButtonBlock(
          actionButton(ns("create_qc_items"), "Create QC items"),
          actionButton(ns("reset"), "Reset")
        )
      )
    )
  )
  return(ui)
}

