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
  # check gitcreds
  check_github_credentials()

  error_if_git_not_initialized()

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #preview_trigger <- reactiveVal(FALSE)
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
      # w_gh$show()

      tryCatch(
        {
          closed_milestones <- get_closed_milestone_names(org = org(), repo = repo())

          if(length(closed_milestones) == 0){
            w_gh$hide()
            showModal(modalDialog(glue::glue("There were no closed milestones found in {org()}/{repo()}. Please use the Create QC app before using the Update QC app."), footer = NULL))
            return()
          }

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

          if(length(all_milestones) == 0) {
            return()
          }

          rev(all_milestones)
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving milestones: {e$message}"))
          showModal(modalDialog("Error in getting milestones: ", e$message, footer = NULL))
        }
      )
    })

    observe({
      req(closed_milestones())
      updateSelectizeInput(
        session,
        "select_milestone",
        choices = c(closed_milestones())
      )
    })

    observe({
      debug(.le$logger, glue::glue("generate_report buttons are inactivated."))
      removeClass("generate_report", "enabled-btn")
      addClass("generate_report", "disabled-btn")
print(input$select_milestone)
      num_milestones_selected <- length(input$select_milestone)
      print(num_milestones_selected)
      if (num_milestones_selected > 0) {
        debug(.le$logger, glue::glue("generate_report buttons are activated because there are {num_milestones_selected} selected QC Item Lists"))

        removeClass("generate_report", "disabled-btn")
        addClass("generate_report", "enabled-btn")
      }

    })


    observeEvent(input$closed_only, {
      if (input$closed_only) {
        updateSelectizeInput(
          session,
          "select_milestone",
          choices = c(closed_milestones())
        )
      }
      else {
        updateSelectizeInput(
          session,
          "select_milestone",
          choices = c(all_milestones())
        )
      }
    })

    observeEvent(input$generate_report, {
       report_trigger(TRUE)
    })

    observe({
      req(report_trigger())
      report_trigger(FALSE)

      w_generate_report <- create_waiter(ns, "Generating report ...")
      w_generate_report$show()
      on.exit(w_generate_report$hide())

      tryCatch({
        pdf_path <- ghqc_report(
          milestone_names = input$select_milestone,
          input_name = input$pdf_name,
          just_tables = input$just_tables,
          location = input$pdf_location
        )
      },
      error = function(e) {
        rlang::abort(e$message)
      }
      )

      showModal(
        modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          tags$p(glue::glue("QC report generated successfully: {pdf_path}")),
          #tags$a(href = milestone_url, "Click here to visit the QC items on Github", target = "_blank")
        )
      )

      report_trigger(FALSE)
    })

    # observe({
    #   #once path is received => do something
    # })
#     observe({
#       # req post_comment causes modal not to show
#       showModal(modalDialog(
#         title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
#         footer = NULL,
#         easyClose = TRUE,
#         tags$p("Update comment posted successfully."),
#         tags$a(href = post_comment(), "Click here to visit the QC Checklist on Github", target = "_blank")
#       ))
#     })
#
#     observe({
#       debug(.le$logger, glue::glue("comment buttons are inactivated."))
#       removeClass("preview", "enabled-btn")
#       addClass("preview", "disabled-btn")
#
#       removeClass("post", "enabled-btn")
#       addClass("post", "disabled-btn")
#
#       if (isTruthy(input$select_issue)) {
#         debug(.le$logger, glue::glue("comment buttons are activated because there is an issue selected: {input$select_issue}"))
#
#         removeClass("preview", "disabled-btn")
#         addClass("preview", "enabled-btn")
#
#         removeClass("post", "disabled-btn")
#         addClass("post", "enabled-btn")
#       }
#     })

    # observeEvent(input$proceed_preview, {
    #   debug(.le$logger, glue::glue("preview comment button proceeded and modal removed."))
    #   removeModal()
    #   preview_trigger(TRUE)
    # })

    # observeEvent(input$proceed_post, {
    #   debug(.le$logger, glue::glue("post comment button proceeded and modal removed."))
    #   removeModal()
    #   report_trigger(TRUE)
    # })

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
