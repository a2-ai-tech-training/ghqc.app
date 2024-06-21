#' @export
create_comment_body <- function(owner, repo, issue_number, message = "", force = FALSE, compare_to_first = TRUE) {
  # get issue
  issue <- get_issue(owner, repo, issue_number)

  qc_commit <- {
    comments <- get_comments(owner, repo, issue_number)
    # if the user wants to get comparison from most recent QC fix and there are QC fixes to draw from
    # TODO: fix bug where compare to first false but there's only the first commit
    if (!compare_to_first && nrow(comments) != 0) {
      last_comment <- get_most_recent_comment_body(comments)
      compared_commit <- get_current_commit_from_comment(last_comment)
      context <- glue::glue("Current script compared to <ins>the previous script updated with QC feedback</ins>: {compared_commit}")
      compared_commit
    }
    else { # get commit upon qc request
      compared_commit <- get_metadata(issue$body)$git_sha
      context <- glue::glue("Current script compared to <ins>the original script upon QC request</ins>: {compared_commit}")
      compared_commit
    }
  }

  # get last commit
  last_commit <- gert::git_log(max = 1)$commit

  ## handle errors
  # if there have been no updates
  if (qc_commit == last_commit && !force) {
    # error: didn't commit and push changes to remote repo
    rlang::abort(message = glue::glue("remote repository unchanged since initialized QC request - be sure to commit and push changes."),
                 class = "commit_shas_match",
                 x = last_commit)
  }

  # if there are untracked changes and the user hasn't forced the operation to go through
  else if (untracked_changes() && !force) {
    # error: not all changes committed and pushed
    rlang::abort(message = glue::glue("remote repository has untracked changes - commit these before running again, or rerun with force = TRUE"),
                 class = "commit_shas_match",
                 x = last_commit)
  }

  # get assignees
  assignees_vec <- sapply(issue$assignees, function(assignee) glue::glue("@{assignee$login}"))
  assignees_body <- {
    if (length(assignees_vec) != 0) {
      list <- glue::glue_collapse(assignees_vec, sep = "\n")
      glue::glue("{list}\n\n")
    }
    else {
      ""
    }
  }

  # format message
  message_body <- {
    if (message == "") message
    else glue::glue("{message}\n\n\n")
  }

  diff <- format_diff(issue$title, qc_commit, last_commit)

  # format comment
  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "## {issue$title}\n",
                             "{context}\n",
                             "{diff}\n\n",
                             "## Metadata\n",
                             "* current QC request commit: {last_commit}",
                             .trim = FALSE
                             )

  as.character(comment_body)
}

#' @export
post_comment <- function(owner, repo, issue_number, body) {
  comment <- gh::gh("POST /repos/:owner/:repo/issues/:issue_number/comments",
                    owner = owner,
                    repo = repo,
                    issue_number = issue_number,
                    body = body
  )
}

add_fix_comment <- function(owner, repo, issue_number, message = "", force = FALSE, compare_to_first = TRUE) {
  body <- create_coment_body(owner, repo, issue_number, message, force, compare_to_first)
  post_comment(owner, repo, issue_number, body)
}

