#' @import shiny
#' @importFrom fs dir_ls
NULL

generate_input_id <- function(prefix = NULL, name) {
  clean_name <- gsub("[^a-zA-Z0-9/_.-]", "", name)
  if (is.null(prefix)) {
    return(clean_name)
  } else {
    return(paste0(prefix, "_", clean_name))
  }
}

#' Render a Selected List with Inputs
#'
#' This function generates a nested list of selected items with associated input fields
#' for checklists and assignees. It creates a hierarchical representation of items with
#' file extensions, providing selectize input fields for each file.
#'
#' @param input A named list from Shiny server function containing inputs from the Shiny application.
#' @param ns A namespace function used for handling Shiny modules.
#' @param items A list representing the selected items.
#' @param checklist_choices A vector of checklist choices for the selectize input fields.
#'
#' @noRd
render_selected_list <- function(input, ns, items = NULL, checklist_choices = NULL, depth = 0) {
  checklist_choices <- setNames(names(checklist_choices), names(checklist_choices))
  ul <- div(class = paste("grid-container", "depth", depth, sep = "-"))

  for (name in items) {
    checklist_input_id <- generate_input_id("checklist", name)
    assignee_input_id <- generate_input_id("assignee", name)
    button_input_id <- generate_input_id("button", name)

    checklist_input <- selectizeInput(
      ns(checklist_input_id),
      label = NULL,
      choices = checklist_choices,
      width = "100%",
      options = list(placeholder = "select checklist")
    )
    assignee_input <- selectizeInput(
      ns(assignee_input_id),
      label = NULL,
      choices = c("No Assignee", input$assignees),
      width = "100%",
      options = list(placeholder = "No Assignee")
    )
    button_input <- actionButton(
      ns(button_input_id),
      label = "preview",
      class = "preview-button"
    )

    # no css only way to set line breaks on certain chr; used <wbr> to designate non-alphanumeric values as wbr (https://stackoverflow.com/a/24489931)
    modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", generate_input_id(name = name))

    ul <- tagAppendChild(ul, div(
      class = "grid-items",
      div(class = "item-a", HTML(modified_name), button_input),
      div(class = "item-b", checklist_input),
      div(class = "item-c", assignee_input)
    ))
  }

  ul
}

#' Update Selectize Inputs for Checklists and Assignees
#'
#' This function updates selectize inputs for both checklist and assignee
#' selections within a Shiny application, handling conditions where assignee
#' options may vary in number. It specifically preserves the current user
#' selections for checklists, and automatically selects the sole assignee if
#' only one is available, or maintains the current selection when more are added.
#'
#' @param input A reactive list of inputs from a Shiny session.
#' @param session A server-side representation of a Shiny session.
#' @param items A list representing the selected items.
#'
#' @return None. The function performs operations on UI elements and does not return
#'   any value.
#' @noRd
isolate_rendered_list <- function(input, session, items) {
  for (name in items) {
    checklist_input_id <- generate_input_id("checklist", name)
    assignee_input_id <- generate_input_id("assignee", name)

    updateSelectizeInput(
      session,
      checklist_input_id,
      selected = isolate(input[[checklist_input_id]])
    )
      updateSelectizeInput(
        session,
        assignee_input_id,
        choices = c("No Assignee", input$assignees),
        selected = isolate(input[[assignee_input_id]])
      )
  }
}

#' Extract File Data from Selected Items
#'
#' This function extracts file data from a hierarchical structure of selected items.
#' It collects the checklist and assignee information for each file and returns a structured list.
#'
#' @param input A list containing input parameters, specifically the values of checklist and assignee inputs.
#' @param items A list representing the selected items, typically structured hierarchically.
#' @return A list of structured data for each file, including the file name, assignees, and checklist type.
#'
#' @noRd
extract_file_data <- function(input, items) {
  file_data <- list()
  for (name in items) {
    checklist_input_id <- generate_input_id("checklist", name)
    assignee_input_id <- generate_input_id("assignee", name)

    checklist_input_value <- input[[checklist_input_id]]
    assignee_input_value <- input[[assignee_input_id]]

    if (!isTruthy(assignee_input_value) || assignee_input_value == "No Assignee") {
      assignee_input_value <- NULL
    }
    # requires the widget and input to be available before proceeding
    if (!isTruthy(checklist_input_value)) {
      return(NULL)
    }

    file_data <- append(file_data, list(create_file_data_structure(file_name = generate_input_id(name = name), assignees = assignee_input_value, checklist_type = checklist_input_value)))
  }

  return(file_data)
}

#' Convert Directory File Paths to a Data Frame
#'
#' This function lists all files in the specified directory recursively,
#' filters out paths without extensions, sorts them so that files with directory
#' names are pushed to the top, and then converts the sorted paths to a data frame
#' with levels representing the directory structure.
#'
#' @param dir_path A character string specifying the path to the directory. Default should be the root directory.
#' @return A data frame where each row represents a file. The columns represent
#' different levels of the directory structure.
#' @noRd
convert_dir_to_df <- function(dir_path = find_root_directory()) {
  if (getwd() != dir_path) {
    setwd(dir_path)
    message("Directory changed to project root:", dir_path, "\n")
  }

  all_paths <- dir_ls(recurse = TRUE, regexp = "renv", invert = TRUE, type = "file")

  has_dirname <- dirname(all_paths) != "."

  sorted_indices <- order(!has_dirname, all_paths)
  sorted_paths <- all_paths[sorted_indices]

  max_path_length <- max(sapply(sorted_paths, function(x) length(unlist(strsplit(x, "/")))))

  # Convert to data frame with padding for shorter paths for treeInput format
  df <- do.call(rbind, lapply(sorted_paths, function(x) {
    file_name <- basename(x)
    path_parts <- unlist(strsplit(x, "/"))

    if (length(path_parts) > 1) {
      path_parts <- c(paste0("📁 ", path_parts[-length(path_parts)]), paste0("📄 ", path_parts[length(path_parts)]))
    } else {
      path_parts <- paste0("📄 ", path_parts)
    }

    padded_paths <- c(path_parts, rep(NA, max_path_length - length(path_parts)))

    data.frame(t(data.frame(padded_paths)), stringsAsFactors = FALSE)
  }))

  colnames(df) <- c(paste0("level", 1:(ncol(df))))
  return(df)
}

#' Find Root Directory
#'
#' This function searches for the root directory of a Git repository by looking for .Rproj file.
#' @return The path of the project root.
#' @noRd
find_root_directory <- function() {
  current_dir <- normalizePath(getwd(), winslash = "/")

  findProjectRoot <- function(path) {
    files <- list.files(path, full.names = TRUE)

    if (any(grepl("\\.Rproj$", files))) {
      return(path)
    } else {
      parent_dir <- dirname(path)

      if (parent_dir == path) {
        stop("No .Rproj file found in any parent directories.")
      }

      return(findProjectRoot(parent_dir))
    }
  }

  project_root <- findProjectRoot(current_dir)

  return(project_root)
}

#' Flatten a List to UI Elements
#'
#' This function takes a list with named elements and flattens it into a series of UI elements
#' suitable for rendering in a Shiny application. It converts each item in the list
#' to a bullet point (`<li>` tag) and adds a bold title (`<b>` tag) for each named
#' sub-list. It also adds a line break (`<br>` tag) before all but the first named
#' sub-list.
#'
#' @param checklists A list containing the items to be converted to UI elements.
#' @param parent_name A character string representing the name of the current element.
#' @param is_first A logical value indicating whether the current list is the first
#'   in the hierarchy.
#'
#' @return A list of UI elements created from the checklists.
#' @noRd
convert_list_to_ui <- function(checklists, parent_name = NULL, is_first = TRUE) {
  ui_elements <- list()

  if (!is.null(parent_name)) {
    if (!is_first) {
      ui_elements <- list(ui_elements, tags$br(), tags$b(parent_name))
    } else {
      ui_elements <- list(ui_elements, tags$b(parent_name))
    }
  }

  if (is.character(checklists)) {
    ui_elements <- c(ui_elements, lapply(checklists, tags$li))
  } else if (is.list(checklists)) {
    first_child <- TRUE
    for (name in names(checklists)) {
      ui_elements <- c(ui_elements, convert_list_to_ui(checklists[[name]], name, is_first && first_child))
      first_child <- FALSE
    }
  } else {
    stop("Unsupported type")
  }

  return(ui_elements)
}

create_button_preview_event <- function(input, name) {
  button_input_id <- generate_input_id("button", name)
  clean_name <- generate_input_id(name = name)

  observeEvent(input[[button_input_id]], {
    showModal(
      modalDialog(
        title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
        footer = NULL,
        easyClose = TRUE,
        renderUI({
          renderPrint(cat(readLines(clean_name), sep = "\n"))
        })
      )
    )
  }, ignoreInit = TRUE)
}
