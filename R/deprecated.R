
#' @import log4r
get_members_list <- function(org, repo) {
  page <- 1
  all_members <- list()

  repeat {
    debug(.le$logger, glue::glue("Retrieving organization members from page {page}..."))
    members <- tryCatch(
      {
        #"GET /repos/{owner}/{repo}/collaborators"
        # "/orgs/{org}/members"
        members_api_call <- gh::gh("/orgs/{org}/members", .api_url = dirname(gert::git_remote_list()$url), org = org, .limit = 100, page = page)
        debug(.le$logger, glue::glue("Retrieved organization members from page {page} successfully."))
        members_api_call
      },
      error = function(e) {
        error(.le$logger, glue::glue("Error retrieving members from organization {org} on page {page}. {e$message}"))
      },
      warning = function(w) {
        warn(.le$logger, glue::glue("Warning while retrieving members from organization {org} on page {page}. {w$message}"))
      }
    )

    if (length(members) == 0) break
    # concatenate list of members as you loop through
    all_members <- c(all_members, members)
    page <- page + 1
  }

  debug(.le$logger, glue::glue("Retrieving member names from member usernames..."))
  members_list <- purrr::map(all_members, ~ get_names_and_usernames(.x$login))
  debug(.le$logger, glue::glue("Retrieved member names from member usernames..."))
  return(members_list)
}


get_members_df <- function(org) {
  debug(.le$logger, glue::glue("Retrieving organization members..."))
  members_list <- get_members_list(org)
  members_df <- purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
  # collaborators <- get_collaborators()
  # collaborators_with_names <- purrr::map(collaborators, ~ get_names_and_usernames(.x$login))
  # members_df <- purrr::map_df(collaborators_with_names, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))

  # logging
  members_string <- glue::glue_collapse(apply(members_df, 1, function(row) {
    glue::glue("username: {row['username']}, name: {row['name']}")
  }), sep = "\n")

  debug(.le$logger, glue::glue("Retrieved the following organization members:\n", members_string))
  if (nrow(members_df) == 0) {
    warn(.le$logger, glue::glue("No organization members retrived from {org}"))
  }
  info(.le$logger, glue::glue("Retrieved {nrow(members_df)} organization members from {org}"))

  members_df
}
