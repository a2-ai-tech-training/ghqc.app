check_if_qc_file_untracked <- function(qc_file_path) {
  status <- processx::run("git", c("status", "-u"))$stdout
  lines <- strsplit(status, "\n")[[1]]
  untracked_start <- grep("Untracked files:", lines)

  if (length(untracked_start) > 0) {
    # lines in "Untracked files" section
    untracked_lines <- lines[(untracked_start + 1):length(lines)]

    # check if the qc_file_path is in untracked_lines
    return(any(grepl(qc_file_path, untracked_lines)))
  }

  # if "Untracked files" not found, return false
  return(FALSE)
}

check_if_issue_name_already_in_milestone <- function(owner, repo, issue_title_in, milestone_name) {
  # get issues in milestone
  issues_in_milestone <- get_all_issues_in_milestone(owner, repo, milestone_name)
  # get issue titles
  issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)
  # see if issue_title_in is in issue_titles
  if (issue_title_in %in% issue_titles) TRUE
  else FALSE
}

check_if_git_initialized <- function(path = getwd()) {
  while (TRUE) {
    if (file.exists(file.path(path, ".git"))) {
      return(TRUE)
    } else {
      parent_path <- dirname(path)
      if (identical(parent_path, path)) {
        return(FALSE)
      }
      path <- parent_path
    }
  }
}


# use this for the update app
# this has been added to get_open_milestone_objects
check_that_milestone_is_non_empty <- function(milestone) {
  if (milestone$open_issues == 0 && milestone$closed_issues == 0) {
    FALSE
  }
  else TRUE
}
