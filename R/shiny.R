#' @import shiny
NULL

#' Get a Tree List of Files and Directories
#'
#' This function recursively retrieves a tree list of files and directories from a given path.
#'
#' @param path A character string specifying the path to retrieve the file tree from.
#' @return A list representing the tree structure of files and directories. Directories are
#' represented as lists with the class "folder" and files are represented as empty strings
#' with the class "file".
#'
#' @export
get_files_tree_list <- function(path) {
  # Get all files and directories at the current path
  items <- list.files(path, full.names = TRUE)

  # items_with_ext <- items[tools::file_ext(items) %in% c("R", "mod", "cpp")]
  items_with_ext <- items[tools::file_ext(items) != ""] # no filtering for extensions for now

  # Filter directories
  directories <- items[dir.exists(items)]

  # Initialize an empty list to store the structure
  tree_list <- list()

  for (dir in directories) {
    subdir_list <- get_files_tree_list(dir)
    if (length(subdir_list) > 0) {
      tree_list[[basename(dir)]] <- structure(subdir_list, sttype = "folder", sticon = "fa fa-folder")
    }
  }

  # Add files to the tree list
  for (file in items_with_ext) {
    tree_list[[basename(file)]] <- structure("", sttype = "file", sticon = "fa fa-file")
  }

  return(tree_list)
}

#' Get Selected Items from a Tree Structure
#'
#' This function recursively retrieves selected items from a given tree structure.
#' Each node in the tree can have an attribute "stselected" which, if TRUE, indicates
#' that the node is selected.
#'
#' @param tree A list representing the tree structure, where each node can have the
#' attribute "stselected" to indicate selection.
#' @param parent A character string specifying the parent path. Defaults to an empty string.
#' @return A list of selected items from the tree structure. The structure of the returned
#' list mirrors the input tree, with selected items indicated.
#' @export
get_selected_items <- function(tree, parent = "") {
  results <- list()
  for (name in names(tree)) {
    node <- tree[[name]]

    path <- if (parent == "") name else paste(parent, name, sep = "/")

    if (!is.null(attr(node, "stselected")) && attr(node, "stselected")) {
      if (is.list(node) && any(sapply(node, function(x) !is.null(attr(x, "stselected")) && attr(x, "stselected")))) {
        results[[name]] <- get_selected_items(node, path)
      } else {
        results[[path]] <- attr(node, "id")
      }
    } else if (is.list(node)) {
      sub_results <- get_selected_items(node, path)
      if (length(sub_results) > 0) {
        results[[name]] <- sub_results
      }
    }
  }
  return(results)
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
#' @export
render_selected_list <- function(input, ns, items = NULL, checklist_choices = NULL, depth = 0) {
  checklist_choices <- setNames(names(checklist_choices), names(checklist_choices))
  ul <- div(class = paste("grid-container", "depth", depth, sep="-"))

  for (name in names(items)) {
    current_item <- items[[name]]
    is_file <- !is.list(current_item)

    if (is_file) {
      checklist_input <- selectizeInput(
        ns(paste0("checklist_", gsub("/", "_", name))),
        label = NULL,
        choices = checklist_choices,
        width = "100%",
        options = list(placeholder = "select checklist"),
      )
      assignee_input <- selectizeInput(
        ns(paste0("assignee_", gsub("/", "_", name))),
        label = NULL,
        choices = input$assignees,
        width = "100%",
        options = list(placeholder = "no assignees"),
      )

      # no css only way to set line breaks on certain chr; used <wbr> to designate non-alphanumeric values as wbr (https://stackoverflow.com/a/24489931)
      modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", name)

      ul <- tagAppendChild(ul, div(
        class = "grid-items",
        div(class = "item-a", HTML(modified_name)),
        div(class = "item-b", checklist_input),
        div(class = "item-c", assignee_input)
      ))
    } else {
      ul <- tagAppendChild(ul, render_selected_list(input, ns, items[[name]], checklist_choices, depth + 1))
    }
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
#' @return None The function performs operations on UI elements and does not return
#'   any value.
#' @export
isolate_rendered_list <- function(input, session, items){
  for (name in names(items)) {
    current_item <- items[[name]]
    is_file <- !is.list(current_item)

    if (is_file) {
      checklist_input_id <- paste0("checklist_", gsub("/", "_", name))
      assignee_input_id <- paste0("assignee_", gsub("/", "_", name))

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
#' @export
extract_file_data <- function(input, items) {
  file_data <- list()
  for (name in names(items)) {
    # Check if name has a file extension and only add to file_names if so
    is_file <- grepl("\\.", name)
    if (is_file) {
      checklist_input_id <- paste0("checklist_", gsub("/", "_", name))
      assignee_input_id <- paste0("assignee_", gsub("/", "_", name))

      checklist_input_value <- input[[checklist_input_id]]
      assignee_input_value <- input[[assignee_input_id]]

      if(!isTruthy(assignee_input_value)) {
        assignee_input_value <- NULL
      }
      # requires the widget and input to be available before proceeding
      if (!isTruthy(checklist_input_value)) {
        return(NULL)
      }

      file_data <- append(file_data, list(create_file_data_structure(file_name = name, assignees = assignee_input_value, checklist_type = checklist_input_value)))
    }
    # Recursively call extract_file_names for subdirectories
    if (!is.null(items[[name]])) {
      subdir_data <- extract_file_data(input, items[[name]])
      file_data <- c(file_data, subdir_data)
    }
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
#' @export
determine_modal_message <- function(ahead = gert::git_ahead_behind()$ahead, behind = gert::git_ahead_behind()$behind) {
  if (ahead > 0 && behind > 0) {
    return("The local repository has changes that need to be pushed and there are updates on the remote that need to be pulled. Please sync these changes.")
  } else if (ahead > 0) {
    return("There are local changes that need to be pushed to the remote repository. Please push these changes.")
  } else if (behind > 0) {
    return("There are updates on the remote repository that need to be pulled. Please pull these updates.")
  } else {
    return("Current repository status is unclear. Please check the Git status and ensure everything is synchronized.")
  }
}
