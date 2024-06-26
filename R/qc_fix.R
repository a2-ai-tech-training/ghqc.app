#' @export
create_comment_body <- function(owner, repo, issue_number, message = NULL, diff = FALSE, force = FALSE, compare_to_first = TRUE) {
  # get issue
  issue <- get_issue(owner, repo, issue_number)

  qc_commit <- {
    comments <- get_comments(owner, repo, issue_number)
    update_comments_exist <- check_if_there_are_update_comments(owner, repo, issue_number)
    # if the user wants to get comparison from most recent QC update comment and there are QC update comments to draw from
    if (!compare_to_first && update_comments_exist) {
      compared_commit <- get_commit_from_most_recent_update_comment(comments)
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
  else if (!force && untracked_changes(issue$title)) {
    # error: not all changes committed and pushed
    rlang::abort(message = glue::glue("remote repository has untracked changes - commit these before running again, or rerun with force = TRUE"),
                 class = "commit_shas_match",
                 x = last_commit)
  }

  # get assignees
  assignees_vec <- sapply(issue$assignees, function(assignee) glue::glue("@{assignee$login}"))
  assignees_body <- {
    if (length(assignees_vec) == 0) ""
    else {
      list <- glue::glue_collapse(assignees_vec, sep = "\n")
      glue::glue("{list}\n\n\n")
    }
  }

  # format message
  message_body <- {
    if (is.null(message)) ""
    else glue::glue("{message}\n\n\n")
  }


  diff <- {
    if (!diff) ""
    else {
      diff_formatted <- format_diff(issue$title, qc_commit, last_commit)
      glue::glue("## {issue$title}\n",
                 "{context}\n",
                 "{diff_formatted}\n\n",)
    }
  }

  # format comment
  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "{diff}",
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

add_fix_comment <- function(owner, repo, issue_number, message = NULL, diff = FALSE, force = FALSE, compare_to_first = TRUE) {
  body <- create_comment_body(owner = owner,
                              repo = repo,
                              issue_number = issue_number,
                              message = message,
                              diff = diff,
                              force = force,
                              compare_to_first = compare_to_first)

  post_comment(owner, repo, issue_number, body)
}

