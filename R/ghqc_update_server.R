#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom gert git_status git_ahead_behind
NULL

ghqc_update_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_trigger <- reactiveVal(FALSE)
    post_trigger <- reactiveVal(FALSE)

    observe({
      milestone_list <- get_open_milestones(org = get_organization(), repo = get_current_repo())
      milestone_list <- rev(milestone_list)

      updateSelectInput(
        session,
        "select_milestone",
        choices =  c("All QC Items", milestone_list),
      )
    })


    observe({
      req(input$select_milestone)

      if(input$select_milestone == "All QC Items") {
        all_issues <- get_all_issues_in_repo(owner = get_organization(), repo = get_current_repo())
        issues_df <- map_df(all_issues, ~{
          tibble(
            number = .x$number,
            title = .x$title,
            state = .x$state
          )
        })
        issues_choices <- issues_df %>%
          mutate(state = case_when(
            state == "open" ~ "Open Items",
            state == "closed" ~ "Closed Items"
          )) %>%
          split(.$state) %>%
          rev() %>%
            lapply(function(x) {
              setNames(nm = paste0("Item ", x$number, ": ", x$title))
          })
      } else{
        issues_by_milestone <- get_all_issues_in_milestone(owner = get_organization(), repo = get_current_repo(), milestone_name = input$select_milestone)
        issues_df <- map_df(issues_by_milestone, ~{
          tibble(
            number = .x$number,
            title = .x$title,
          )
        })
        issues_choices <- issues_df %>%
          mutate(label = paste0("Item ", number, ": ", title)) %>%
          pull()
      }

      updateSelectInput(
        session,
        "select_issue",
        choices = issues_choices,
      )
    })

    issue_parts <- reactive({
      req(input$select_issue)
      issue_parts <- strsplit(sub("Item ", "", input$select_issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])
      list(issue_number = issue_number, issue_title = issue_title)
    })

    #https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
    modal_check <- eventReactive(c(input$preview, input$post), {
      req(issue_parts())
      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)

      gh_issue_status <- if (input$compare == "prev") {
        check_if_there_are_update_comments(
          owner = get_organization(),
          repo = get_current_repo(),
          issue_number = issue_parts()$issue_number
        )
      } else {
        TRUE
      }

      determine_modal_message(
        selected_files = issue_parts()$issue_title,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        gh_issue_status = gh_issue_status
      )
    })

    observeEvent(input$preview, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          HTML(modal_check()$message),
          footer = tagList(
            if (modal_check()$state == "warning") {
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
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          HTML(modal_check()$message),
          footer = tagList(
            if (modal_check()$state == "warning") {
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
        "Update comment posted successfully."
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
