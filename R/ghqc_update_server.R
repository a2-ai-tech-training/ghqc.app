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

      # nest issues under milestone name group. need to set single elements as list or else won't nest https://stackoverflow.com/questions/57248427/grouped-select-input-with-only-one-item
      # TODO: need to finish re-sort list after split so that most recent milestones are on top rather than alphabetical
      # sorted_issues <- issues %>%
      #   dplyr::mutate(milestone_order = as.numeric(factor(milestone_created, levels = unique(milestone_created)))) %>%
      #   split(.$milestone) %>%
      #   lapply(function(x) {
      #     setNames(object = as.list(paste(x$number, x$title)),
      #              nm = paste0("Item ", x$number, ": ", x$title))
      # })

      req(issues())
      milestone_list <- unique(issues()$milestone)

      updateSelectizeInput(
        session,
        "select_milestone",
        choices =  milestone_list,
        server = TRUE
      )


    })


    observe({
      req(issues(), input$select_milestone)

      issues_by_milestone <- split(issues(), issues()$milestone)

      issues_choices <- issues_by_milestone[[input$select_milestone]] %>%
        dplyr::mutate(label = paste0("Item ", number, ": ", title)) %>%
        dplyr::pull()

      updateSelectizeInput(
        session,
        "select_issue",
        choices = issues_choices,
        server = TRUE
      )
    })
    #checks:
    # git sync status - same as before
    # file status - pull issue title name and compare to see if in git panel #TODO: check if this works
    # also keep the git file check
    #TODO: ask jenna to have functions to check for 1. if there are no commits or comments to return false instead of error
    # > check_if_there_are_update_comments(owner = get_organization(), repo = get_current_repo(), issue_number = 3) # this has no comments
    # Error in UseMethod("arrange") :
    #   no applicable method for 'arrange' applied to an object of class "NULL"
    # > check_if_there_are_update_comments(owner = get_organization(), repo = get_current_repo(), issue_number = 5)
    # [1] TRUE
    # Only way this button should proceed is if
    # 1. there are no unstaged changes in local of the file
    # 2. git status is current
    # 3. There is a new commit different from init commit sha
    #  preview and post should have same proceed checks

    # init/prev differences should be the same if there is no prev update comment. init should look at first comment for sha;
    # prev should loop from latest comment until it founds most recent sha

    # workflow should be (?) 1. create qc item list thru create qc app 2. assignee/qcer comments issues on gh 3. author fixes issues and pushes new commit
    # 4. author uses update qc app to add fix comment on prev commit 5-7. repeat 2-4 as needed 8. after author addresses everything, uses init compare for final comment
    # 9. author repeats for all qc items in list 10. author scrapes the milestone/qc item list for archive items thru a fxn

    issue_parts <- reactive({
      req(input$select_issue)
      issue_parts <- strsplit(sub("Item ", "", input$select_issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])
      list(issue_number = issue_number, issue_title = issue_title)
    })

    observeEvent(input$preview, {
      req(issue_parts())
      print(issue_parts())


      git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      message <- determine_update_modal_message(selected_issue = issue_parts()$issue_title, git_files = git_files, git_sync_status = git_sync_status)

      if (!is.null(message)) {
        showModal(modalDialog(
          HTML(message),
          footer = tagList(
            if (length(git_files) > 0 &&
                !(issue_parts()$issue_title %in% git_files) &&
                git_sync_status$ahead == 0 &&
                git_sync_status$behind == 0) {
              actionButton(ns("proceed"), "Proceed Anyway")
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
      print(issue_parts())


      git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      message <- determine_update_modal_message(selected_issue = issue_parts()$issue_title, git_files = git_files, git_sync_status = git_sync_status)

      if (!is.null(message)) {
        showModal(modalDialog(
          HTML(message),
          footer = tagList(
            if (length(git_files) > 0 &&
                !(issue_parts()$issue_title %in% git_files) &&
                git_sync_status$ahead == 0 &&
                git_sync_status$behind == 0) {
              actionButton(ns("proceed"), "Proceed Anyway")
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

      html_file_path <- create_gfm_file(create_comment_body(get_organization(),
                                                            get_current_repo(),
                                                            message = input$message,
                                                            issue_number = issue_parts()$issue_number,
                                                            diff = input$show_diff,
                                                            compare_to_first = TRUE,
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

      add_fix_comment(get_organization(),
                                 get_current_repo(),
                                 message = input$message,
                                 issue_number = issue_parts()$issue_number,
                                 diff = input$show_diff,
                                 compare_to_first = TRUE,
                                 force = TRUE)

      showModal(modalDialog(
        "Commented posted."
      ))
    })

    observeEvent(input$proceed, {
      removeModal()
      preview_trigger(TRUE)
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
