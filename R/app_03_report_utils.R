generate_html_list_with_hyperlink <- function(items) {
  browser()
  #paste("<li>", files, "</li>", collapse = "")
  paste("<li><a href='", items$url, "'>", items$title, "</a></li>", collapse = "")
}

generate_open_milestone_message <- function(open_milestones, warning_icon_html) {
  messages <- c()
  if (length(open_milestones) > 0) {
    messages <- c(messages, sprintf(
      "%s The following selected milestones are open:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list_with_hyperlink(open_milestones)
    ))
  }
  return(messages)
}

generate_open_issue_message <- function(open_issues, warning_icon_html) {
  messages <- c()
  if (length(open_issues) > 0) {
    messages <- c(messages, sprintf(
      "%s The selected milestones contain the following open issues:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list_with_hyperlink(open_issues)
    ))
  }
  return(messages)
}

generate_open_checklist_message <- function(issues_with_open_checklists, warning_icon_html) {
  messages <- c()
  if (length(issues_with_open_checklists) > 0) {
    messages <- c(messages, sprintf(
      "%s The selected milestones contain the following issues with open checklist items:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list_with_hyperlink(issues_with_open_checklists)
    ))
  }
  return(messages)
}

determine_modal_message_report <- function(owner, repo, milestone_names) {
  warning_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"

  open_milestones <- check_for_open_milestones(owner, repo, milestone_names)
  open_issues <- check_for_open_issues(owner, repo, milestone_names)
  open_checklists <- check_for_open_checklists(owner, repo, milestone_names)

  # uncommitted_selected_files <- selected_files[selected_files %in% uncommitted_git_files | selected_files %in% untracked_selected_files]
  # uncommitted_files <- list(selected = uncommitted_selected_files, general = uncommitted_git_files)
  # issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)
  # existing_issues <- selected_files[selected_files %in% issue_titles]

  messages <- c()
  messages <- c(messages, generate_open_milestone_message(open_milestones, warning_icon_html))
  messages <- c(messages, generate_open_issue_message(open_issues, warning_icon_html))
  messages <- c(messages, generate_open_checklist_message(open_checklists, warning_icon_html))

  # log_string <- glue::glue("Modal Check Inputs:
  #   - Selected Files: {glue::glue_collapse(selected_files, sep = ', ')}
  #   - Uncommitted Git Files: {glue::glue_collapse(uncommitted_git_files, sep = ', ')}
  #   - Untracked Selected Files: {glue::glue_collapse(untracked_selected_files, sep = ', ')}
  #   - Git Sync Status: Ahead: {git_sync_status$ahead}, Behind: {git_sync_status$behind}
  #   - Commit Update Status: {commit_update_status}
  #   - Issues in Milestone: {glue::glue_collapse(existing_issues, sep = ', ')}
  # ")
  #
  # log4r::debug(.le$logger, log_string)

  if (length(messages) == 0) {
    return(list(message = NULL, state = NULL))
  } else {
    state <- if (any(grepl(warning_icon_html, messages))) "warning" else "error"
    return(list(message = paste(messages, collapse = "\n"), state = state))
  }
}



check_for_open_milestones <- function(owner, repo, milestone_names) {
  milestones <- purrr::map(milestone_names, function(milestone_name) {
    get_milestone_from_name(owner, repo, milestone_name)
  })

  open_milestones <- purrr::map_dfr(milestones, function(milestone) {
    if (milestone$state == "open") {
      data.frame(title = milestone$title, url = milestone$html_url)
    }
    else NULL
  })
}

check_for_open_issues <- function(owner, repo, milestone_names) {
  open_issues <- purrr::map_dfr(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

    purrr::map_dfr(issues, function(issue) {
      if (issue$state == "open") {
        data.frame(title = issue$title, url = issue$html_url)
      }
      else NULL
    })
  })
}

check_for_open_checklists <- function(owner, repo, milestone_names) {
  issues_with_open_checklists <- purrr::map_dfr(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

    purrr::map_dfr(issues, function(issue) {
      if (unchecked_items_in_issue(issue)) {
        data.frame(title = issue$title, url = issue$html_url)
      } else NULL
    })
  })
}

check_for_open_issues_and_checklists <- function(owner, repo, milestone_names) {

  sapply(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(owner, repo, milestone_name)
    issue_names <- sapply(issues, function(issue) {

      if (issue$state == "open") {
        new_row <- data.frame(title = issue$title, url = issue$html_url)
        open_issues <- rbind(open_issues, new_row)
      }
      if (unchecked_items_in_issue(issue)) {
        new_row <- data.frame(title = issue$title, url = issue$html_url)
        issues_with_open_checklists <- rbind(issues_with_open_checklists, new_row)
      }
    })
  })

  return(list(open_issues = open_issues,
              issues_with_open_checklists = issues_with_open_checklists
              )
         )
}


