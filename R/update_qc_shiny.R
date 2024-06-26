#' Determine Modal Message Based on Git Status and File Commit Status
#'
#' This function generates HTML-formatted messages indicating the synchronization status
#' of the local git repository and the commit status of selected files. It highlights
#' files that need to be pushed or pulled to synchronize with the remote repository, as
#' well as local files with uncommitted changes.
#'
#' @param gh_file fdafda
#' @param git_files A character vector of file paths representing files with uncommitted changes in the git repository.
#' @param git_sync_status A list containing elements `ahead` and `behind` which indicate the number
#' of commits by which the local repository is ahead or behind the remote repository, respectively.
#'
#' @return A character string with HTML content detailing the status messages. If no issues are
#' detected, the function returns `NULL`.
#' @noRd
determine_modal_message <- function(gh_file, git_files, git_sync_status) {
  messages <- c()
  uncommitted_selected_files <- gh_file %in% git_files

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
                                    generate_html_list(gh_file[uncommitted_selected_files])))
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
