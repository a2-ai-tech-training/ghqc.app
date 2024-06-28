#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr case_when
NULL

ghqc_update_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_trigger <- reactiveVal(FALSE)
    post_trigger <- reactiveVal(FALSE)

    issues <- reactive(get_issues_info())

    observe({

      # req(issues())
      # milestone_list <- unique(issues()$milestone)
      milestone_list <- get_open_milestones(org = get_organization(), repo = get_current_repo())
      milestone_list <- rev(milestone_list)

      updateSelectizeInput(
        session,
        "select_milestone",
        choices =  milestone_list,
        server = TRUE
      )


    })


    observe({
      req(input$select_milestone)

     # issues_by_milestone <- split(issues(), issues()$milestone)
      issues_by_milestone <- get_all_issues_in_milestone(owner = get_organization(), repo = get_current_repo(), milestone_name = input$select_milestone)

      # issues_choices <- issues_by_milestone[[input$select_milestone]] %>%
      #   dplyr::mutate(label = paste0("Item ", number, ": ", title)) %>%
      #   dplyr::pull()

      issues_choices <- issues_by_milestone %>%
          lapply(function(x) {
            setNames(nm = paste0("Item ", x$number, ": ", x$title))
        })

      updateSelectizeInput(
        session,
        "select_issue",
        choices = issues_choices,
        server = TRUE
      )
    })

    issue_parts <- reactive({
      req(input$select_issue)
      issue_parts <- strsplit(sub("Item ", "", input$select_issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])
      list(issue_number = issue_number, issue_title = issue_title)
    })

    observeEvent(input$preview, {
      req(issue_parts())

      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
    #  gh_issue_status <- check_if_there_are_update_comments(owner = get_organization(), repo = get_current_repo(), issue_number = issue_parts()$issue_number)
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)
      message <- determine_update_modal_message(selected_issue = issue_parts()$issue_title,
                                                uncommitted_git_files = uncommitted_git_files,
                                                untracked_selected_files = untracked_selected_files,
                                                git_sync_status = git_sync_status)

      if (!is.null(message)) {
        showModal(modalDialog(
          HTML(message),
          footer = tagList(
            if (length(uncommitted_git_files) > 0 &&
                !(issue_parts()$issue_title %in% untracked_selected_files) &&
                !(issue_parts()$issue_title %in% uncommitted_git_files) &&
                git_sync_status$ahead == 0 &&
                git_sync_status$behind == 0) {
              actionButton(ns("proceed_preview"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          )
        ))
      } else {
        preview_trigger(TRUE)
      }
    })

    observeEvent(input$post, {
      req(issue_parts())

      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      #  gh_issue_status <- check_if_there_are_update_comments(owner = get_organization(), repo = get_current_repo(), issue_number = issue_parts()$issue_number)
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)
      message <- determine_update_modal_message(selected_issue = issue_parts()$issue_title,
                                                uncommitted_git_files = uncommitted_git_files,
                                                untracked_selected_files = untracked_selected_files,
                                                git_sync_status = git_sync_status)

      if (!is.null(message)) {
        showModal(modalDialog(
          HTML(message),
          footer = tagList(
            if (length(uncommitted_git_files) > 0 &&
                !(issue_parts()$issue_title %in% untracked_selected_files) &&
                !(issue_parts()$issue_title %in% uncommitted_git_files) &&
                git_sync_status$ahead == 0 &&
                git_sync_status$behind == 0) {
              actionButton(ns("proceed_post"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          )
        ))
      } else {
        post_trigger(TRUE)
      }
    })

    observe({
      req(issue_parts())
      req(preview_trigger())
      preview_trigger(FALSE)

      compare_to_first <- case_when(
        input$compare == "init" ~ TRUE,
        input$compare == "prev" ~ FALSE
      )

      html_file_path <- create_gfm_file(create_comment_body(get_organization(),
                                                            get_current_repo(),
                                                            message = input$message,
                                                            issue_number = issue_parts()$issue_number,
                                                            diff = input$show_diff,
                                                            compare_to_first = compare_to_first,
                                                            force = TRUE))
      custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")


      showModal(modalDialog(
        title = "Comment Preview",
        HTML(custom_html)
      ))
    })


    observe({
      req(issue_parts())
      req(post_trigger())
      post_trigger(FALSE)

      compare_to_first <- case_when(
        input$compare == "init" ~ TRUE,
        input$compare == "prev" ~ FALSE
      )

      add_fix_comment(get_organization(),
                                 get_current_repo(),
                                 message = input$message,
                                 issue_number = issue_parts()$issue_number,
                                 diff = input$show_diff,
                                 compare_to_first = compare_to_first,
                                 force = TRUE)

      showModal(modalDialog(
        "Commented posted."
      ))
    })

    observeEvent(input$proceed_preview, {
      removeModal()
      preview_trigger(TRUE)
    })

    observeEvent(input$proceed_post, {
      removeModal()
      post_trigger(TRUE)
    })

    observeEvent(input$return, {
      removeModal()
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
