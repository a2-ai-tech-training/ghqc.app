#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @import dplyr
#' @import log4r
#' @importFrom purrr map_df
#' @importFrom gert git_status git_ahead_behind
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom shinyWidgets treeInput create_tree
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_ahead_behind git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_report_server <- function(id) {
  error_if_git_not_initialized()

  # check gitcreds
  check_github_credentials()

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report_trigger <- reactiveVal(FALSE)
    waiter_hide()

    org <- reactive({
      tryCatch(
        {
          get_organization()
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving organization: {e$message}"))
          showModal(modalDialog("Error in getting organization: ", e$message, footer = NULL))
        }
      )
    })

    repo <- reactive({
      tryCatch(
        {
          get_current_repo()
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving repo: {e$message}"))
          showModal(modalDialog("Error in getting repository: ", e$message, footer = NULL))
        }
      )
    })

    closed_milestones <- reactive({
      req(org(), repo())
      w_gh <- create_waiter(ns, sprintf("Fetching milestone data for %s in %s...", repo(), org()))

      tryCatch(
        {
          closed_milestones <- get_closed_milestone_names(org = org(), repo = repo())
          rev(closed_milestones)
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving milestones: {e$message}"))
          showModal(modalDialog("Error in getting milestones: ", e$message, footer = NULL))
        }
      )
    })

    all_milestones <- reactive({
      req(org(), repo())

      tryCatch(
        {
          all_milestones <- list_milestones(org = org(), repo = repo())
          rev(all_milestones)
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving milestones: {e$message}"))
          showModal(modalDialog("Error in getting milestones: ", e$message, footer = NULL))
        }
      )
    })

    observeEvent(input$closed_only, {
      # if closed
      if (input$closed_only) {
        placeholder <- ifelse(length(closed_milestones()) == 0, "No closed milestones", "Select closed milestones")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = closed_milestones(),
          options = list(placeholder = placeholder)
        )
      }

      # if not closed
      else {
        placeholder <- ifelse(length(all_milestones()) == 0, "No milestones", "Select milestone")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = c(all_milestones()),
          options = list(placeholder = placeholder)
        )
      }
    })

    observe({
      debug(.le$logger, glue::glue("generate_report buttons are inactivated."))
      removeClass("generate_report", "enabled-btn")
      addClass("generate_report", "disabled-btn")

      num_milestones_selected <- length(input$select_milestone)

      if (num_milestones_selected > 0) {
        debug(.le$logger, glue::glue("generate_report buttons are activated because there are {num_milestones_selected} selected QC Item Lists"))
        removeClass("generate_report", "disabled-btn")
        addClass("generate_report", "enabled-btn")
      }
    })

    observe({
      req(report_trigger())
      report_trigger(FALSE)

      milestone_num_str <- ifelse(length(input$select_milestone) == 1, "milestone", "milestones")

      milestones <- glue::glue_collapse(input$select_milestone, sep = ", ", last = ", and ")
      w_generate_report <- create_waiter(ns, glue::glue("Generating report for {milestone_num_str}: {milestones}..."))
      w_generate_report$show()
      on.exit(w_generate_report$hide())

      tryCatch({
        pdf_path <- ghqc_report(
          milestone_names = input$select_milestone,
          input_name = input$pdf_name,
          just_tables = input$just_tables,
          location = input$pdf_location
        )

        showModal(
          modalDialog(
            title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
            footer = NULL,
            easyClose = TRUE,
            tags$p(glue::glue("QC report generated successfully: {pdf_path}"))
          )
        ) #showModal
      },
      error = function(e) {
        error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"
        showModal(modalDialog(
          title = tags$div(
            tags$span("Error", style = "float: left; font-weight: bold; font-size: 20px;"),
            modalButton("Dismiss"),
            style = "overflow: hidden; text-align: right;"
          ),
          HTML(error_icon_html, e$message, "<br>"),
          easyClose = TRUE,
          footer = NULL
        ))
        #rlang::abort(e$message)
      }) # tryCatch
      report_trigger(FALSE)
    })


    observeEvent(input$generate_report, {
      report_trigger(TRUE)
    })


    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Comment button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      session$reload()
    })

    return(input)
  })
}
