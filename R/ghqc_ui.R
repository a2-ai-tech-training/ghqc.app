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
    # tags$head(tags$style(HTML(brio::read_file(system.file("css/styles.css", package = "ghqc"))))),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc/css/styles.css"),
      tags$script(type = "module", src = "ghqc/js/adjust_grid.js"),
      tags$script(type = "module", src = "ghqc/js/toggle_sidebar.js"),
      tags$script(type = "module", src = "ghqc/js/tree_paths.js")
      ),
    waiter_preloader(
      html = tagList(
        spin_1(),
        h4("Loading in ...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(
        title = span(tags$img(src = "ghqc/assets/logo.png", height = 50, class = "logo-img"), "QC Shiny Tool"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      div(
        id = ns("content"),
        uiOutput(ns("sidebar")),
        div(
          id = ns("divider"),
          actionButton(ns("toggle_sidebar"), "", icon = icon("angle-double-left"), class = "toggle-sidebar-btn")
        ),
        miniContentPanel(uiOutput(ns("main_panel")))
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("create_qc_items"), "Create QC items")
        )
      )
    )
  )

  return(ui)
}
