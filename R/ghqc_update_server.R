#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr case_when
NULL

ghqc_update_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
    #  issues <- get_issues_info()
      issues <- get_all_issues(owner = get_organization(), repo = get_current_repo())

      issues_df <- purrr::map_df(issues, ~{
        tibble::tibble(
          number = .x$number,
          title = .x$title,
          milestone = if (!is.null(.x$milestone)) .x$milestone$title else NA,
          milestone_created = if (!is.null(.x$milestone) && !is.null(.x$milestone$created_at)) .x$milestone$created_at else NA
        )
      })

    #  Filter out issues without a milestone
      issues_df <- issues_df %>% dplyr::filter(!is.na(milestone))


      # nest issues under milestone name group. need to set single elements as list or else won't nest
      # TODO: need to finish re-sort list after split so that most recent milestones are on top rather than alphabetical
      sorted_issues <- issues_df %>%
        dplyr::mutate(milestone_order = as.numeric(factor(milestone_created, levels = unique(milestone_created)))) %>%
        split(.$milestone) %>%
        lapply(function(x) {
          setNames(object = as.list(x$number),
                   nm = paste0("Issue ", x$number, ": ", x$title))
      })

      # Updating the selectInput with grouped choices
      updateSelectizeInput(
        session,
        "select_issue",
        choices = sorted_issues,
        server = TRUE
      )
    })

    #checks:
    # git sync status - same as before
    # file status - pull issue title name and compare to see if in git panel
    # also keep the git file checkk

    observe({
      req(input$select_issue)
      issue_parts <- strsplit(sub("Issue ", "", input$select_issue), ": ")[[1]]
      print(issue_parts)
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])
      print(issue_number)
      print(issue_title)
    })

    observeEvent(input$preview, {
      req(input$select_issue)

      issue_parts <- strsplit(sub("Issue ", "", input$select_issue), ": ")[[1]]
      print(issue_parts)
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])


      # git_files <- git_status()$file
      # git_sync_status <- git_ahead_behind()
      # message <- determine_modal_message(selected_files = file_names, git_files = git_files, git_sync_status = git_sync_status)
      #
      # if (!is.null(message)) {
      #   showModal(modalDialog(
      #     HTML(message),
      #     footer = tagList(
      #       if (length(git_files) > 0 &&
      #           !any(file_names %in% git_files) &&
      #           git_sync_status$ahead == 0 &&
      #           git_sync_status$behind == 0) {
      #         actionButton(ns("proceed"), "Proceed Anyway")
      #       },
      #       actionButton(ns("return"), "Return")
      #     )
      #   ))
      # } else {
      #   qc_trigger(TRUE)
      # }

      html_file_path <- create_gfm_file(create_comment_body(get_organization(),
                                                            get_current_repo(),
                                                            issue_number = issue_number,
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
