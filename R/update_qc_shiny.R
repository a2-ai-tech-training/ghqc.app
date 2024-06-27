determine_update_modal_message <- function(selected_issue, git_files, git_sync_status) {
  messages <- c()
  uncommitted_selected_files <- selected_issue %in% git_files

  generate_html_list <- function(files) {
    paste("<li>", files, "</li>", collapse = "")
  }

  warning_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"
  error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"

  if (git_sync_status$ahead > 0 || git_sync_status$behind > 0) {
    sync_messages <- c()
    if (git_sync_status$ahead > 0) sync_messages <- c(sync_messages, "push changes to the remote repository.")
    if (git_sync_status$behind > 0) sync_messages <- c(sync_messages, "pull updates from the remote.")
    messages <- c(messages, paste(error_icon_html, "There are local changes that need to be synchronized. Please", paste(sync_messages, collapse = " and "), "<br>"))
  }

  if (any(uncommitted_selected_files)) {
    messages <- c(messages, sprintf("%s The following selected local files have uncommitted changes:<ul>%s</ul><br>",
                                    error_icon_html, generate_html_list(selected_issue[uncommitted_selected_files])))
  }

  if (length(git_files) > 0 && !any(uncommitted_selected_files)) {
    messages <- c(messages, sprintf("%s There are local files that have uncommitted changes:<ul>%s</ul><br>",
                                    warning_icon_html, generate_html_list(git_files)))
  }

  if (length(messages) == 0) {
    return(NULL)
  } else {
    return(paste(messages, collapse = " "))
  }
}
