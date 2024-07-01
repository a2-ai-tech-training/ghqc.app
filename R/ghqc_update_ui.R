#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_preloader
NULL

ghqc_update_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc/css/styles.css"),
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
          title = span(tags$img(src = "ghqc/assets/logo.png", height = 50, class = "logo-img"), "Update QC Shiny Tool"),
          right = actionButton(ns("reset"), "Reset", class = "btn-sm")
        ),
        miniContentPanel(
          div(id = ns("center_content"),
          selectInput(ns("select_milestone"), "Select QC Item List (github milestone)", choices = "", multiple = FALSE),
          selectInput(ns("select_issue"), "Select QC Item (github issue)", choices = "", multiple = FALSE),
          textAreaInput(ns("message"), "Message", ""),
          checkboxInput(ns("show_diff"), "Show file difference?", FALSE),
          conditionalPanel(
            condition = "input.show_diff == true", ns = ns,
            radioButtons(ns("compare"), "Compare to:", inline = TRUE, choices = c(
              "Initial QC Request" = "init",
              "Previous QC Update" = "prev"
            ))
          )
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("preview"), "Preview Comment"),
          actionButton(ns("post"), "Post Comment")
        )
      )
    )
  )
  return(ui)
}
