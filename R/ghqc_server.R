#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom shinyWidgets treeInput create_tree
#' @importFrom waiter Waiter spin_1 spin_2
#' @importFrom gert git_ahead_behind
NULL

#' @export
ghqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    output$sidebar <- renderUI({
      tagList(
        textInput(ns("milestone"), "Create a QC identifier (github milestone)"),
        textAreaInput(
          ns("milestone_description"),
          "Create a description for the QC",
          placeholder = "(optional)"
        ),
        selectizeInput(
          ns("assignees"),
          "Select assignees for QC",
          choices = "",
          multiple = TRUE
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
      updateSelectizeInput(
        session,
        "assignees",
        server = TRUE,
        choices = get_members_df(get_organization()),
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
    })


    output$tree_list_ui <- renderUI({
      files_tree_df <- convert_dir_to_df(dir_path = find_root_directory())

      treeInput(
        inputId = ns("tree_list"),
        label = div(
          "Select files for QC",
          actionButton(ns("file_info"), NULL, icon = icon("question"), class = "question-action-button")
        ),
        choices = create_tree(files_tree_df),
        returnValue = "text", # neither id or all gives pathing
        closeDepth = 0
      )
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

    observeEvent(c(selected_items(), input$assignees), {
      delay(300, {
        w_load_items$hide()
      })
    }, ignoreInit = TRUE)

    # button behavior
    observe({
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")

      if (length(selected_items()) > 0 && isTruthy(input$milestone)) {
        removeClass("create_qc_items", "disabled-btn")
        addClass("create_qc_items", "enabled-btn")
      }
    })

    observeEvent(input$create_qc_items, {
      req(selected_items())
      file_data <- extract_file_data(input, selected_items())

      if (git_ahead_behind()$ahead == git_ahead_behind()$behind) {
        w_create_qc_items$show()
        create_yaml("test",
          repo = get_current_repo(),
          milestone = input$milestone,
          description = input$milestone_description,
          files = file_data
        )
        create_checklists("test.yaml")

        removeClass("create_qc_items", "enabled-btn")
        addClass("create_qc_items", "disabled-btn")

        w_create_qc_items$update(html = tagList(
          h4("QC items created!", style = "color: white;")
        ))

        Sys.sleep(1) # Delay to allow the user to see the success message

        w_create_qc_items$hide()
      } else {
        showModal(
          modalDialog(determine_modal_message())
        )
      }
    })

    observeEvent(input$reset, {
      session$reload()
    })

    return(input)
  })
}
