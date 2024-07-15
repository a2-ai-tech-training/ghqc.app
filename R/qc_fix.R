get_init_qc_commit <- function(owner, repo, issue_number) {
  issue <- get_issue(owner, repo, issue_number)
  get_metadata(issue$body)$`git sha`
}

# error_if_repo_unchanged_since_last_qc_request <- function(owner, repo, issue_number) {
#   qc_commit <- {
#     # get comments
#     comments <- get_comments(owner, repo, issue_number)
#
#     # see if there have been updates
#     update_comments_exist <- check_if_there_are_update_comments(owner, repo, issue_number)
#
#     # get sha from most recent qc update
#     if (update_comments_exist) {
#       get_commit_from_most_recent_update_comment(comments)
#     }
#     # else, if not updates, get sha from original qc request
#     else {
#       get_init_qc_commit(owner, repo, issue_number)
#     }
#   }
#
#   # get last commit
#   last_commit <- gert::git_log(max = 1)$commit
#
#   # if the latest commit is the same as the latest qc request commit, the files will just be the same, so error
#   # i.e. the author didn't update the file, so posting a comment about updates doesn't make sense
#   if (qc_commit == last_commit) {
#     # error: didn't commit and push changes to remote repo
#     rlang::abort(message = glue::glue("repository unchanged since initialized QC request - be sure to commit and push changes."),
#                  class = "commit_shas_match",
#                  x = last_commit)
#   }
# } # error_if_repo_unchanged_since_last_qc_request

#' @export
create_comment_body <- function(owner,
                                repo,
                                issue_number,
                                message = NULL,
                                diff = FALSE,
                                reference_commit = "original",
                                comparator_commit = "current") {
  # get issue
  issue <- get_issue(owner, repo, issue_number)

  cat(glue::glue("Creating comment body for issue #{issue_number} in {owner}/{repo}"), "\n")

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

  # get vals if default
  if (reference_commit == "original" && comparator_commit == "current") {
    # reference = oldest
    reference_commit <- get_init_qc_commit(owner, repo, issue_number)
    # comparator = newest
    comparator_commit <- gert::git_log(max = 1)$commit
  }

  # get script contents
  script_contents <- get_script_contents(issue$title, reference = reference_commit, comparator = comparator_commit)
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script

  # get script hashes
  reference_script_hash <- digest::digest(reference_script)
  comparator_script_hash <- digest::digest(comparator_script)

  # format diff
  diff <- {
    if (!diff) ""

    else {
      # get context for diff
      context <- glue::glue(
        "reference commit (older version): {reference_commit}\n
        comparator commit (newer version): {comparator_commit}\n"
      )

      diff_formatted <- format_diff(reference_script = reference_script, comparator_script = comparator_script)
      glue::glue("## File Difference\n",
                 "{context}\n",
                 "{diff_formatted}\n\n",)
    }
  }

  # format comment
  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "{diff}",
                             "## Metadata\n",
                             "* reference commit: {reference_commit}\n",
                             "* reference script hash: {reference_script_hash}\n",
                             "* comparator commit: {comparator_commit}\n",
                             "* comparator script hash: {comparator_script_hash}\n",
                             .trim = FALSE
                             )

  cat(glue::glue("Comment body created for issue #{issue_number} with assignees: {paste(assignees_vec, collapse = ', ')}"), "\n")

  as.character(comment_body)
}

#' @export
post_comment <- function(owner, repo, issue_number, body) {
  cat(glue::glue("Posting comment to issue #{issue_number} in {owner}/{repo}"), "\n")

  comment <- gh::gh("POST /repos/:owner/:repo/issues/:issue_number/comments",
                    owner = owner,
                    repo = repo,
                    issue_number = issue_number,
                    body = body
  )
  cat(glue::glue("Comment posted to issue #{issue_number} in {owner}/{repo}"), "\n")
}

add_fix_comment <- function(owner,
                            repo,
                            issue_number,
                            message = NULL,
                            diff = FALSE,
                            reference_commit = "original",
                            comparator_commit = "current") {
  cat(glue::glue("Adding update comment to issue #{issue_number} in {owner}/{repo}"), "\n")

  body <- create_comment_body(owner,
                              repo,
                              issue_number,
                              message,
                              diff,
                              comparator_commit,
                              reference_commit)

  post_comment(owner, repo, issue_number, body)

  cat(glue::glue("Update comment added to issue #{issue_number} in {owner}/{repo}"), "\n")
}
