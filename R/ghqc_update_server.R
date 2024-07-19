#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom gert git_status git_ahead_behind
NULL

ghqc_update_server <- function(id) {
  error_if_git_not_initialized()

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_trigger <- reactiveVal(FALSE)
    post_trigger <- reactiveVal(FALSE)

    waiter_hide()

    start_time <- Sys.time()

    log_message <- function(message) {
      cat(round(difftime(Sys.time(), start_time, units = "secs"), 2), "-", message, "\n")
    }

    org <- reactive({
      get_organization()
    })

    repo <- reactive({
      get_current_repo()
    })

    observe({
      w_gh <- create_waiter(ns, sprintf("Fetching organization and milestone data for %s ...", org()))

      log_message(paste("Connecting to organization:", org()))
      log_message(paste("Retrieving open milestones from repo:", repo()))

      milestone_list <- get_open_milestone_names(org = org(), repo = repo())
      milestone_list <- rev(milestone_list)

      updateSelectInput(
        session,
        "select_milestone",
        choices = c("All QC Items", milestone_list),
      )
      log_message(paste("Connected to organization and retrieved", length(milestone_list), "open milestones from repo:", repo()))
    }, priority = -1)

    observe({
      w_gh <- create_waiter(ns, sprintf("Fetching issue data for %s ...", input$select_milestone))
      w_gh$show()
      on.exit(w_gh$hide())

      req(input$select_milestone)

      if (input$select_milestone == "All QC Items") {
        log_message(paste("Retrieving all issues from repo:", repo()))

        all_issues <- get_all_issues_in_repo(owner = org(), repo = repo())
        issues_df <- map_df(all_issues, ~ {
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
        log_message(paste("Retrieved", length(all_issues), "issues from repo:", repo()))
      } else {
        log_message(paste("Retrieving all issues from milestone:", input$select_milestone))

        issues_by_milestone <- get_all_issues_in_milestone(owner = org(), repo = repo(), milestone_name = input$select_milestone)
        issues_df <- map_df(issues_by_milestone, ~ {
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
        log_message(paste("Retrieved", length(issues_by_milestone), "issues from milestone:", input$select_milestone))
      }

      updateSelectInput(
        session,
        "select_issue",
        choices = issues_choices,
      )
    }, priority = -1)

    ref_commits <- reactive({
      req(issue_parts()$issue_number)
      ref_commits <- get_reference_df(issue_number = issue_parts()$issue_number)
    })

    observe({
      if (nrow(ref_commits()) == 0) {
        return(updateSelectizeInput(session, "ref_commits", choices = "",
                                    options = list(
                                      placeholder = "No commits since QC initialization."
                                    )))
      }

      ref_commits <- ref_commits() %>%
        split(.$date) %>%
        rev() %>%
        lapply(function(x) {
          setNames(
            object = x$commit,
            nm = x$display
          )
        })

      # to prevent empty inputs (which errors comment fxn) on selection focus lost, autoset to first choice if otherwise empty
      updateSelectizeInput(session, "ref_commits", choices = ref_commits,
                           options = list(
                             onBlur = I("function() {
                             var selectize = this;
                             if (selectize.getValue() === '') {
                                 selectize.setValue(selectize.options[Object.keys(selectize.options)[0]].value);
                             }
                         }")
                           ))
    })

    comp_commits <- reactive({
      req(issue_parts()$issue_number)
      req(input$ref_commits)

      comp_commits <- get_comparator_df(
        issue_number = issue_parts()$issue_number,
        selected_reference_commit = input$ref_commits
      )
    })

    observe({
      if (!isTruthy(comp_commits())) {
        return(updateSelectizeInput(session, "comp_commits", choices = "",
                                    options = list(
                                      placeholder = "No commits since reference commit."
                                    )))
      }

      comp_commits <- comp_commits() %>%
        split(.$date) %>%
        rev() %>%
        lapply(function(x) {
          setNames(
            object = x$commit,
            nm = x$display
          )
        })

      # to prevent empty inputs (which errors comment fxn) on selection focus lost, autoset to first choice if otherwise empty
      updateSelectizeInput(session, "comp_commits", choices = comp_commits,
                           options = list(
                             onBlur = I("function() {
                             var selectize = this;
                             if (selectize.getValue() === '') {
                                 selectize.setValue(selectize.options[Object.keys(selectize.options)[0]].value);
                             }
                         }")
                           ))
    })

    issue_parts <- reactive({
      req(input$select_issue)
      issue_parts <- strsplit(sub("Item ", "", input$select_issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])
      list(issue_number = issue_number, issue_title = issue_title)
    })

    # https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
    modal_check <- eventReactive(c(input$preview, input$post), {
      req(issue_parts()$issue_title)
      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)

      commit_update_status <- check_if_updates_since_init(get_reference_df(issue_number = issue_parts()$issue_number))

      determine_modal_message(
        selected_files = issue_parts()$issue_title,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        commit_update_status = commit_update_status
      )
    })

    observeEvent(input$preview, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed_preview"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        preview_trigger(TRUE)
      }
    })

    observeEvent(input$post, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed_post"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        post_trigger(TRUE)
      }
    })

    observe({
      req(issue_parts()$issue_number)
      req(preview_trigger())
      preview_trigger(FALSE)

      commits_for_compare <- case_when(
        input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
        input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
      )

      html_file_path <- create_gfm_file(create_comment_body(org(),
        repo(),
        message = input$message,
        issue_number = issue_parts()$issue_number,
        diff = input$show_diff,
        comparator_commit = commits_for_compare$comparator_commit,
        reference_commit = commits_for_compare$reference_commit
      ))
      custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")


      showModal(modalDialog(
        title = tags$div(
          style = "display: flex;
          justify-content: space-between;
          align-items: center;",
          "Comment Preview",
          modalButton("Dismiss")
        ),
        footer = NULL,
        easyClose = TRUE,
        HTML(custom_html)
      ))
    })

    observe({
      req(issue_parts()$issue_number)
      req(post_trigger())
      post_trigger(FALSE)

      commits_for_compare <- case_when(
        input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
        input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
      )

      add_fix_comment(
        org(),
        repo(),
        message = input$message,
        issue_number = issue_parts()$issue_number,
        diff = input$show_diff,
        reference_commit = commits_for_compare$reference_commit,
        comparator_commit = commits_for_compare$comparator_commit
      )

      issue <- get_issue(org(), repo(), issue_parts()$issue_number)
      issue_url <- issue$html_url

      showModal(modalDialog(
        title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
        footer = NULL,
        easyClose = TRUE,
        tags$p("Update comment posted successfully."),
        tags$a(href = issue_url, "Click here to visit the QC Checklist on Github", target = "_blank")
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

      # TODO block buttons if show file diff is checked + commit checks if they exist
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
