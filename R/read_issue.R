parse_unchecked_elem <- function(check_line) {
  check_line <- stringr::str_remove_all(check_line, "- \\[x\\]") # including x for testing since most are not checked
n
}

find_unchecked_header <- function(subsubheader_loc, unchecked_loc) {
  diffs <- subsubheader_loc - unchecked_loc
  loc <- unchecked_loc - min(abs(diffs[diffs < 0]))
}

name_unchecked_elem <- function(body, head_loc, uc_loc) {
  unchecked_header_loc <- find_unchecked_header(head_loc, uc_loc)
  unchecked_header_name <- stringr::str_remove_all(body[unchecked_header_loc], "### ")
  unchecked_content <- stringr::str_remove_all(body[uc_loc], "- \\[x\\] ") # including x for testing since most are not checked
  paste0(unchecked_header_name, ": ", unchecked_content)
}

parse_checklist <- function(body) {
  subsubheader_loc <- which(grepl("###", body))
  unchecked_loc <- which(grepl("\\[x\\]", body)) # including x for testing since most are not checked
  unchecked_elem <- unlist(lapply(unchecked_loc, function(uL) name_unchecked_elem(body, subsubheader_loc, uL)))
}

#' check_checklists
#'
#' @param issue
#'
#' @return
#'
#' @importFrom stringr str_split
#'
#' @examples
check_checklists <- function(issue) {
  unchecked_issues <- list()
  for (i in issue) {
    body <- unlist(stringr::str_split(i$body, "\n"))
    body <- body[body != ""]
    unchecked_issues <- c(unchecked_issues, list(parse_checklist(body)))
  }
  names(unchecked_issues) <- names(issue)
  unchecked_issues <- unchecked_issues[unlist(lapply(unchecked_issues, function(uI) class(uI) == "character"))]
}

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
  issue <- list()
  for (state in c("open", "closed")) {
    state_issue <- gh::gh("/repos/{owner}/{repo}/issues",
           owner = owner,
           repo = repo,
           milestone = milestone_number,
           state = state)
    names(state_issue) <- lapply(state_issue, function(sI) sI$title)
    issue <- c(issue, list(state_issue))
  }
  names(issue) <- c("open", "closed")
  issue <- issue[unlist(lapply(issue, function(x) length(x) != 0))]
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

  issues <- lapply(milestone_names, function(mN) pull_issue(owner, repo, mN))

  issue_state <- unlist(lapply(issues, function(x) x$state))
  issue_body <- unlist(lapply(issues, function(x) x$body))

  unchecked_closed_items <- list()
  unchecked_open_items <- list()

  for (mN in milestone_names) {
    milestone_issues <- pull_issue(owner, repo, mN)
    # unchecked_closed_issues <- c(unchecked_closed_items, check_checklists(milestone_issues$closed))
    unchecked_open_items <- check_checklists(milestone_issues$open)
    unchecked_items <- write_unchecked_items(unchecked_closed_issues, unchecked_open_issues)
  }
  # names(unchecked_closed_items) <- milestone_names
  # unchecked_closed_items <- unchecked_closed_items[unlist(lapply(unchecked_closed_items, function(x) length(x) != 0))]
  unchecked_open_items <- unchecked_open_items[unlist(lapply(unchecked_open_items, function(x) length(x) != 0))]
  # list(closed_issues = unchecked_closed_items, open_issues = unchecked_open_items)
}
