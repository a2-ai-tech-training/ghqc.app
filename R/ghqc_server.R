#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom shinyTree renderTree shinyTree
#' @importFrom waiter Waiter spin_1
#' @importFrom gert git_ahead_behind
NULL

#' @export
ghqc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    w <- Waiter$new(
      id = ns("main_container"),
      html = tagList(
        spin_1(),
        h4("Creating QC items ...", style = "color: white;")
      ),
      color = "darkgrey"
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
        tags$body(
          div(
            class = "flex-container",
            h5("Select files for QC", class = "header-padding"),
            actionButton(ns("file_info"), NULL,
                         icon = icon("question"),
                         class = "small-action-button"
            )
          )
        ),
        shinyTree(ns("tree_list"), checkbox = TRUE)
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


    output$tree_list <- renderTree({
      get_files_tree_list(path = ".")
    })

    selected_items <- reactive({
      validate(need(input$tree_list, "No files selected"))
      get_selected_items(tree = input$tree_list)
    })


    output$main_panel <- renderUI({
      validate(need(length(selected_items()) > 0, "No files selected"))

      list <- render_selected_list(input, ns, items = selected_items(), checklist_choices = get_checklists())

      isolate_rendered_list(input, session, selected_items())

      return(list)
    })

    # button actions

    # button state
    observe({
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")

      req(selected_items())
      file_data <- extract_file_data(input, selected_items())

      if (length(file_data) > 0 && isTruthy(input$milestone)) {
        removeClass("create_qc_items", "disabled-btn")
        addClass("create_qc_items", "enabled-btn")
      }
    })

    observeEvent(input$create_qc_items, {
      req(selected_items())
      file_data <- extract_file_data(input, selected_items())

      if (git_ahead_behind()$ahead == git_ahead_behind()$behind) {
        w$show()
        create_yaml("test",
          repo = get_current_repo(), # TODO: set to current repo for final: get_current_repo()
          milestone = input$milestone,
          description = input$milestone_description,
          files = file_data
        )
        create_checklists("test.yaml") # commented to test context behavior outside of gh port

        removeClass("create_qc_items", "enabled-btn")
        addClass("create_qc_items", "disabled-btn")

        w$update(html = tagList(
          h4("QC items created!", style = "color: white;")
        ))

        Sys.sleep(1) # Delay to allow the user to see the success message

        w$hide()
      } else {
        showModal(
          modalDialog(determine_modal_message())
        )
      }
    })

    observeEvent(input$reset, {
      session$reload()
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    return(input)
  })
}
