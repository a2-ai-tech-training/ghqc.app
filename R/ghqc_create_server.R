#' @import shiny
#' @import log4r
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom shinyWidgets treeInput create_tree
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_ahead_behind git_status
NULL

ghqc_create_server <- function(id) {
  error_if_git_not_initialized()

  rproj_root_dir <- rprojroot::find_rstudio_root_file()
  if (getwd() != rproj_root_dir) {
    setwd(rproj_root_dir)
    message("Directory changed to project root:", rproj_root_dir, "\n")
  }

  selected_paths <- treeNavigatorServer(
    "explorer",
    rootFolder = rproj_root_dir,
    search = FALSE,
    pattern = exclude_patterns(),
    all.files = FALSE
  )

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    qc_trigger <- reactiveVal(FALSE)

    waiter_hide()

    org <- reactive({
      get_organization()
    })

    repo <- reactive({
      get_current_repo()
    })

    members <- reactive({
      get_members_df(org())
    })

    w_load_items <- Waiter$new(
      id = ns("content"),
      html = tagList(
        spin_2(),
      ),
      color = "white"
    )

    start_time <- Sys.time()

    log_message <- function(message) {
      cat(round(difftime(Sys.time(), start_time, units = "secs"), 2), "-", message, "\n")
    }


    output$sidebar <- renderUI({
      w_tree <- create_waiter(ns, sprintf("Creating file tree for %s ...", basename(getwd())))
      w_tree$show()
     # Sys.sleep(2)
      on.exit(w_tree$hide())
      tagList(
        textInput(ns("milestone"),
                  "Name QC Item List (github milestone)",
                  placeholder = "(required)",
                  width = "100%"),
        textAreaInput(
          ns("milestone_description"),
          "Create a description for the QC Item List",
          placeholder = "(optional)",
          width = "100%"
        ),
        selectizeInput(
          ns("assignees"),
          "Select assignees for QC",
          choices = "No Assignee",
          multiple = TRUE,
          width = "100%",
          options = list(
            closeAfterSelect = TRUE
          )
        ),
        div(
          style = "display: flex; align-items: center; column-gap: 5px;",
          h5("Select files for QC"),
          actionButton(ns("file_info"), "checklist info", class = "preview-button")
        ),
        treeNavigatorUI("explorer")
      )
    })

    observe({
      debug(.le$logger, paste("Connecting to organization..."))
      w_gh <- create_waiter(ns, sprintf("Fetching organization and member data for %s ...", org()))
      w_gh$show()
      on.exit(w_gh$hide())

      # Organization logging
      info(.le$logger, paste("Connected to organization:", org()))

      # Assignees logging
      debug(.le$logger, paste("Retrieving assignees..."))
      info(.le$logger, glue::glue("Retrieved {nrow(members())} assignees from {org()}"))

      members_string <- glue::glue_collapse(apply(members(), 1, function(row) {
        glue::glue("username: {row['username']}, name: {row['name']}")
      }), sep = "\n")

      debug(.le$logger, paste("Retrived assignees:\n", members_string))
      if (nrow(members()) == 0) {
        warn(.le$logger, glue::glue("No assignees retrived from {org()}"))
      }

      updateSelectizeInput(
        session,
        "assignees",
        server = TRUE,
        choices = members(),
        options = list(
          placeholder = "(optional)",
          valueField = "username",
          labelField = "username",
          searchField = c("username", paste0("name")),
          render = I(
            '{ option: function(item, escape) {
if (item.name !== null) {
return "<div><strong>" + escape(item.username) + "</strong> (" + escape(item.name) +") </div>" } else {
return "<div><strong>" + escape(item.username) + "</div>"
}
}
}'
          )
        )
      )
      debug(.le$logger, paste(
        "Connected to organization and retrieved",
        nrow(members()),
        "assignees from:",
        org()
      ))
    })

    selected_items <- reactive({
      req(selected_paths())
      selected_paths()
    })

    qc_items <- reactive({
      req(selected_items())
      extract_file_data(input, selected_items())
    })

    output$main_panel <- renderUI({
      validate(need(length(selected_items()) > 0, "No files selected"))
      w_load_items$show()
      list <- render_selected_list(input, ns, items = selected_items(), checklist_choices = get_checklists())
      isolate_rendered_list(input, session, selected_items())

      session$sendCustomMessage("adjust_grid", list) # finds the width of the files and adjusts grid column spacing based on values
      return(list)
    })

    observeEvent(c(selected_items(), input$assignees),
      {
        delay(300, {
          w_load_items$hide()
        })
      },
      ignoreInit = TRUE
    )

    # button behavior
    observe({
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")

      if (length(selected_items()) > 0 && isTruthy(input$milestone)) {
        removeClass("create_qc_items", "disabled-btn")
        addClass("create_qc_items", "enabled-btn")
      }
    })

    observeEvent(input$file_info, {
      checklists <- get_checklists()
      showModal(
        modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          "Each file input will require a checklist type. Each checklist type will have its own items associated with it.",
          "See below for a reference of all types and their items.",
          br(),
          br(),
          selectInput(ns("checklist_info"), NULL, choices = names(checklists)),
          renderUI({
            info <- checklists[[input$checklist_info]]
            list <- convert_list_to_ui(info) # checklists needs additional formatting for list of named elements
            tags$ul(list)
          })
        )
      )
    })

    observeEvent(selected_items(), {
      items <- selected_items()
      for (name in items) {
        create_button_preview_event(input, name = name)
      }
    })

    modal_check <- eventReactive(input$create_qc_items, {
      req(qc_items())
      file_names <- sapply(qc_items(), function(x) x$name)
      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), file_names)

      issues_in_milestone <- get_all_issues_in_milestone(owner = org(), repo = repo(), milestone_name = input$milestone)

      determine_modal_message(
        selected_files = file_names,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        issues_in_milestone = issues_in_milestone
      )
    })

    observeEvent(input$create_qc_items, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        qc_trigger(TRUE)
      }
    })


    observe({
      req(qc_trigger())
      qc_trigger(FALSE)

      w_create_qc_items <- create_waiter(ns, "Creating QC items ...")
      w_create_qc_items$show()

      create_yaml("test",
        repo = repo(),
        milestone = input$milestone,
        description = input$milestone_description,
        files = qc_items()
      )
      create_checklists("test.yaml") # added logging to fxn
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")
      milestone_url <- get_milestone_url(org(), repo(), input$milestone)

      w_create_qc_items$hide()

      showModal(
        modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          tags$p("QC items created successfully."),
          tags$a(href = milestone_url, "Click here to visit the QC items on Github", target = "_blank")
        )
      )
    })

    observeEvent(input$proceed, {
      removeModal()
      qc_trigger(TRUE)
    })

    observeEvent(input$return, {
      removeModal()
    })

    observeEvent(input$reset, {
      session$reload()
    })

    return(input)
  })
}
