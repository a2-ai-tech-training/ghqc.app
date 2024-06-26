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
      choices = input$assignees,
      width = "100%",
      options = list(placeholder = "no assignees")
    )

    # no css only way to set line breaks on certain chr; used <wbr> to designate non-alphanumeric values as wbr (https://stackoverflow.com/a/24489931)
    modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", generate_input_id(name=name))

    ul <- tagAppendChild(ul, div(
      class = "grid-items",
      div(class = "item-a", HTML(modified_name)),
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
    if (length(input$assignees) == 1) {
      updateSelectizeInput(
        session,
        assignee_input_id,
        choices = input$assignees,
        selected = input$assignees
      )
    } else {
      updateSelectizeInput(
        session,
        assignee_input_id,
        choices = input$assignees,
        selected = isolate(input[[assignee_input_id]])
      )
    }
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

    if (!isTruthy(assignee_input_value)) {
      assignee_input_value <- NULL
    }
    # requires the widget and input to be available before proceeding
    if (!isTruthy(checklist_input_value)) {
      return(NULL)
    }

    file_data <- append(file_data, list(create_file_data_structure(file_name = generate_input_id(name=name), assignees = assignee_input_value, checklist_type = checklist_input_value)))
  }

  return(file_data)
}

#' Determine the Modal Message for Git Synchronization
#'
#' This function determines the appropriate modal message based on the
#' current Git status regarding how many commits the local repository is
#' ahead or behind the remote repository. It provides instructions for
#' the user to either push local changes, pull remote updates, or both.
#'
#' @param ahead Integer indicating how many commits the local repository
#'        is ahead of the remote repository.
#' @param behind Integer indicating how many commits the local repository
#'        is behind the remote repository.
#'
#' @return A character string with the message to be displayed in the modal.
#' @noRd
determine_modal_message <- function(selected_files, git_files, git_sync_status) {
  messages <- c()
  uncommitted_selected_files <- selected_files %in% git_files

  generate_html_list <- function(files) {
    paste("<li>", files, "</li>", collapse = "")
  }

  if (git_sync_status$ahead > 0 || git_sync_status$behind > 0) {
    sync_messages <- c()
    if (git_sync_status$ahead > 0) sync_messages <- c(sync_messages, "push changes to the remote repository.")
    if (git_sync_status$behind > 0) sync_messages <- c(sync_messages, "pull updates from the remote.")
    messages <- c(messages, "There are local changes that need to be synchronized. Please", paste(sync_messages, collapse = " and "))
  }

  if (any(uncommitted_selected_files)) {
    messages <- c(messages, sprintf("The following selected local files have uncommitted changes:<ul>%s</ul>",
                                    generate_html_list(selected_files[uncommitted_selected_files])))
  }

  if (length(git_files) > 0 && !any(uncommitted_selected_files)) {
    messages <- c(messages, sprintf("There are local files that have uncommitted changes:<ul>%s</ul>",
                                    generate_html_list(git_files)))
  }

  if (length(messages) == 0) {
    return(NULL)
  } else {
    return(paste(messages, collapse = " "))
  }
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
  # Find the maximum path length
  max_path_length <- max(sapply(sorted_paths, function(x) length(unlist(strsplit(x, "/")))))

  # Convert to data frame with padding for shorter paths
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
#' @return Sets wd to the path of the project root.
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

