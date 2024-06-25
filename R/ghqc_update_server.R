#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr case_when
NULL

ghqc_update_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      issues <- get_issues_info()

      choices <- setNames(issues$number, paste0("Issue ", issues$number, ": ", issues$title))

      updateSelectizeInput(
        session,
        "select_issue",
        choices = c("", choices)
      )
    })

    observe({
      print(input$select_issue)
    })

    observeEvent(input$preview, {
      req(input$select_issue)
      html_file_path <- create_gfm_file(create_comment_body(get_organization(),
                                                            get_current_repo(),
                                                            issue_number = input$select_issue,
                                                            diff = input$show_diff, #TODO: check outputs
                                                            force = TRUE))
      custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")

      # modal_content <- case_when(
      #     !input$show_diff ~ custom_html,
      #     input$show_diff && input$compare == "init" ~ '<h1>Initial QC Request</h1><p>Comparing to the initial quality control request.</p>',
      #     input$show_diff && input$compare == "prev" ~ '<h1>Previous QC Update</h1><p>Comparing to the previous quality control update.</p>',
      #     TRUE ~ '<h1>Unexpected Option</h1><p>An unexpected option was selected.</p>'
      #   )

      showModal(modalDialog(
        title = "Comment Preview",
        renderUI(HTML(custom_html)),
        footer = modalButton("Close")
      ))
    })

    observe({
      removeClass("preview", "enabled-btn")
      addClass("preview", "disabled-btn")

      removeClass("post", "enabled-btn")
      addClass("post", "disabled-btn")


      if (isTruthy(input$select_issue)) {
        removeClass("preview", "disabled-btn")
        addClass("preview", "enabled-btn")

        removeClass("post", "disabled-btn")
        addClass("post", "enabled-btn")
      }
    })

    observeEvent(input$reset, {
      session$reload()
    })

    return(input)
  })
}
