#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom shinyWidgets treeInput create_tree
#' @importFrom waiter Waiter spin_1 spin_2
#' @importFrom gert git_ahead_behind git_status
NULL

ghqc_create_server <- function(id) {
  error_if_git_not_initialized()

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    qc_trigger <- reactiveVal(FALSE)

    org <- reactive({
      get_organization()
    })

    repo <- reactive({
      get_current_repo()
    })

    members <- reactive({
      get_members_df(org())
    })

    w_create_qc_items <- Waiter$new(
      id = ns("main_container"),
      html = tagList(
        spin_1(),
        h4("Creating QC items ...", style = "color: white;")
      ),
      color = "darkgrey"
    )

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
      tagList(
        textInput(ns("milestone"), "Name QC Item List (github milestone)", width = "100%"),
        textAreaInput(
          ns("milestone_description"),
          "Create a description for the QC Item List",
          placeholder = "(optional)",
          width = "100%"
        ),
        selectizeInput(
          ns("assignees"),
          "Select assignees for QC",
          choices = "",
          multiple = TRUE,
          width = "100%",
          options = list(
            closeAfterSelect = TRUE
          )
        ),
        uiOutput(ns("tree_list_ui")),
      )
    })


    observeEvent(input$file_info, {
      showModal(
        modalDialog(
          "Each file input will require a checklist type. Each checklist type will have its own items associated with it.",
          "See below for a reference of all types and their items.",
          br(),
          br(),
          selectInput(ns("checklist_info"), NULL, choices = names(get_checklists())),
          renderUI({
            info <- get_checklists()[[input$checklist_info]]
            tags$ul(
              lapply(info, function(item) {
                tags$li(item)
              })
            )
          }),
          easyClose = TRUE
        )
      )
    })

    observe({
      log_message(paste("Connecting to organization:", org()))
      log_message(paste("Retrieving assignees:", members()))
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
      log_message(paste(
        "Connected to organization and retrieved",
        nrow(members()),
        "assignees from:",
        org()
      ))
    })


    output$tree_list_ui <- renderUI({
      root_dir <- find_root_directory()
      files_tree_df <- convert_dir_to_df(dir_path = root_dir)
      log_message(paste("Creating file tree for:", root_dir))

      tree <- treeInput(
        inputId = ns("tree_list"),
        label = div(
          "Select files for QC",
          actionButton(ns("file_info"), NULL, icon = icon("question"), class = "question-action-button")
        ),
        choices = create_tree(files_tree_df),
        returnValue = "text", # neither id or all gives pathing
        closeDepth = 0
      )

      log_message(paste("Created file tree for", nrow(files_tree_df), "files"))

      return(tree)
    })


    selected_items <- reactive({
      validate(need(input$tree_list, "No files selected"))
      session$sendCustomMessage("process_tree_list", message = list(ns = id)) # pass in ns id for new input

      input$paths # input needs to be set through js because treeInput not built to give pathing info
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

    qc_items <- reactive({
      req(selected_items())
      extract_file_data(input, selected_items())
    })

    modal_check <- eventReactive(input$create_qc_items, {
      req(qc_items())
      file_names <- sapply(qc_items(), function(x) x$name)
      uncommitted_git_files <- git_status()$file
      git_sync_status <- git_ahead_behind()
      untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), file_names)

      determine_modal_message(
        selected_files = file_names,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status
      )
    })

    observeEvent(input$create_qc_items, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          HTML(modal_check()$message),
          footer = tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          )
        ))
      } else {
        qc_trigger(TRUE)
      }
    })


    observe({
      req(qc_trigger())
      qc_trigger(FALSE)

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
          tags$p("QC items created successfully."),
          tags$a(href = milestone_url, "Click here to visit the QC items on Github", target = "_blank")
          # "QC items created successfully."
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
