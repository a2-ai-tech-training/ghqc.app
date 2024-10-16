# get the most chronologically recent comparator in milestone
get_final_milestone_qc_commit <- function(owner, repo, milestone_name) {

  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

  list_of_most_recent_comps_in_each_issue <- lapply(issues, function(issue) {
    get_final_issue_qc_commit(owner, repo, issue)
  })

  # get most recent comp commit in list
}

get_issue_commits <- function(owner, repo, issue_number) {
  updates <- check_if_there_are_update_comments(owner, repo, issue_number)
  if (!updates) {
    # return init qc commit
    get_init_qc_commit(owner, repo, issue_number)
  }
  else {
    # get comments
    comments <- get_comments(owner, repo, issue_number)
    #if (nrow(comments) == 1) comments <- as.list(comments[1, ])
    # get comp in each update comment

    comps <- as.list(
      apply(comments, 1, function(comment) {
        comment_body <- comment[["body"]]
        get_comparator_commit_from_comment(comment_body)
      })
    )
    comps
  }
}

# get the most chronologically recent comparator in issue
get_final_issue_qc_commit <- function(owner, repo, issue) {
  # get list of commits
  issue_commits <- get_issue_commits(owner, repo, issue$number)
  #if (length(issue_commits) == 1) return(issue_commits[1])
  #else {
    # get all commits ever
    full_commit_log <- gert::git_log(ref = NULL)
  #}

}


get_comments <- function(owner, repo, issue_number) {
  comments <- gh::gh(
    "GET /repos/:owner/:repo/issues/:issue_number/comments",
    .api_url = Sys.getenv("GHQC_API_URL"),
    owner = owner,
    repo = repo,
    issue_number = issue_number
  )
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))

  return(comments_df)
}

# returns true if the user can check "compare to most recent qc fix"
# false otherwise
check_if_there_are_update_comments <- function(owner, repo, issue_number) {
  comments <- get_comments(owner, repo, issue_number)
  if (length(comments) == 0) return(FALSE)
  most_recent_qc_commit <- get_commit_from_most_recent_update_comment(comments)
  if (is.na(most_recent_qc_commit)) return(FALSE)
  else return(TRUE)
}

# gets the most recent qc update commit from the comments in the issue
# if there are no update comments from the author, it returns NA
get_commit_from_most_recent_update_comment <- function(comments_df) {
  # sort by descending creation time
  comments_df <- comments_df %>% dplyr::arrange(dplyr::desc(created_at))

  # loop through comments, grab the first one
  for (i in seq_len(nrow(comments_df))) {
    comment <- comments_df[i, ]
    commit_from_comment <- get_comparator_commit_from_comment(comment$body)
    if (!is.na(commit_from_comment)) {
      return(commit_from_comment)
    }
  }

  return(NA)
}

get_comparator_commit_from_comment <- function(body) {
  stringr::str_match(body, "comparator commit: ([a-f0-9]+)")[,2]
}
