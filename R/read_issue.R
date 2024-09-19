#' pull_issue
#'
#' @param owner
#' @param repo
#' @param milestone_name
#'
#' @return pulled issue from github as list
pull_issue <- function(owner, repo, milestone_name) {
  milestone_number <- look_up_existing_milestone_number(list(owner = owner,
                                                             repo = repo,
                                                             title = milestone_name))
  issue <- list(gh::gh("/repos/{owner}/{repo}/issues",
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number))
  names(issue[[1]]) <- lapply(issue[[1]], function(mI) mI$title)
}

#' check_completeness
#'
#' @param milestone_name input$select_milestone
#'
#' @return
#'
#' @examples
check_completeness <- function(milestone_names){
  owner <- get_organization()
  repo <- get_current_repo()

  issues <- list()
  for (mN in milestone_names) {
    milestone_issues <- list(pull_issue(owner, repo, mN))
    issue_names <- lapply(milestone_issues[[1]], function(mI) mI$title)
    names(milestone_issues[[1]]) <- issue_names
    issues <- c(issues, milestone_issues)
  }
  names(issues) <- milestone_names

  return(issues)
}
