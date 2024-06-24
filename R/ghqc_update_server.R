#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr case_when
NULL

ghqc_update_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      issues <- get_issues2()

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

      html_file_path <- "example.html"
        custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")

      #---

      #create_comment_body(owner = get_organization(), repo = get_current_repo(), issue_number = 1, message = "TEST")
      # Error in gert::git_branch_create(temp_branch, ref = commit_sha) :
      # Failed to find git reference 'd9e7eff4746ac0768beea63957dd09b54b0a482e'

      # Error in if (stringr::str_detect(line, "^\\*")) { :
      #     missing value where TRUE/FALSE needed
      #---
     # html_file_path <- "example.html"
     # custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")

      modal_content <- case_when(
          !input$show_diff ~ custom_html,
          input$show_diff && input$compare == "init" ~ '<h1>Initial QC Request</h1><p>Comparing to the initial quality control request.</p>',
          input$show_diff && input$compare == "prev" ~ '<h1>Previous QC Update</h1><p>Comparing to the previous quality control update.</p>',
          TRUE ~ '<h1>Unexpected Option</h1><p>An unexpected option was selected.</p>'
        )

      showModal(modalDialog(
        title = "Comment Preview",
        renderUI(HTML(modal_content)),
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
