get_init_qc_commit <- function(owner, repo, issue_number) {
  issue <- get_issue(owner, repo, issue_number)
  get_metadata(issue$body)$git_sha
}

create_assignees_list <- function(assignees) {
  sapply(assignees, function(assignee) glue::glue("@{assignee$login}"))
}

create_assignees_body <- function(assignees_list) {
  if (length(assignees_list) == 0) ""
  else {
    list <- glue::glue_collapse(assignees_list, sep = "\n")
    glue::glue("{list}\n\n\n")
  }
}

create_message_body <- function(message) {
  if (is.null(message)) ""
  else glue::glue("{message}\n\n\n")
}

get_script_hash <- function(script) {
  collapsed_script <- glue::glue_collapse(script, "\n")
  digest::digest(collapsed_script)
}

create_metadata_body <- function(reference_commit,
                                 comparator_commit,
                                 reference_script,
                                 comparator_script) {

  # get script hashes
  reference_script_hash <- get_script_hash(reference_script)
  comparator_script_hash <- get_script_hash(comparator_script)

  glue::glue("## Metadata\n",
             "* reference commit: {reference_commit}\n",
             "* reference script hash: {reference_script_hash}\n",
             "* comparator commit: {comparator_commit}\n",
             "* comparator script hash: {comparator_script_hash}\n")
}

create_diff_body <- function(diff, reference_commit, reference_script, comparator_commit, comparator_script) {
  if (!diff) ""

  else {
    # get context for diff
    context <- glue::glue(
      "reference commit (previous version): {reference_commit}\n
        comparator commit (current version): {comparator_commit}\n"
    )

    diff_formatted <- format_diff(reference_script, comparator_script)
    glue::glue("## File Difference\n",
               "{context}\n",
               "{diff_formatted}\n\n",)
  }
}

#' @export
create_comment_body <- function(owner,
                                repo,
                                issue_number,
                                message = NULL,
                                diff = FALSE,
                                comparator_commit = "original",
                                reference_commit = "previous") {

  cat(glue::glue("Creating comment body for issue #{issue_number} in {owner}/{repo}"), "\n")

  issue <- get_issue(owner, repo, issue_number)

  # get vals if default
  if (comparator_commit == "original") {
    # comparator_commit is original qc commit
    comparator_commit <- get_metadata(issue$body)$`git sha`
  }

  if (reference_commit == "previous") {
    # reference_commit is most recent commit
    reference_commit <- gert::git_log(max = 1)$commit
  }

  assignees_list <- create_assignees_list(issue$assignees)
  assignees_body <- create_assignees_body(assignees_list)

  message_body <- create_message_body(message)

  script_contents <- get_script_contents(issue$title, comparator_commit, reference_commit)
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script

  diff <- create_diff_body(diff,
                           reference_commit,
                           reference_script,
                           comparator_commit,
                           comparator_script)

  metadata_body <- create_metadata_body(reference_commit,
                                        comparator_commit,
                                        reference_script,
                                        comparator_script)

  # format comment
  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "{diff}",
                             "{metadata_body}",
                             .trim = FALSE
                             )

  cat(glue::glue("Comment body created for issue #{issue_number} with assignees: {paste(assignees_list, collapse = ', ')}"), "\n")

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
                            comparator_commit = "original",
                            reference_commit = "previous") {
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
