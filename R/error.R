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
