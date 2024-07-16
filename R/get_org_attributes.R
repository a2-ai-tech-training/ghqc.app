get_names_and_usernames <- function(username) {
  user <- gh::gh("/users/{username}", username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}

get_members_list <- function(org) {
  page <- 1
  all_members <- list()

  repeat {
    members <- tryCatch(
      {
        members_api_call <- gh::gh("/orgs/{org}/members", org = org, .limit = 100, page = page)
        #cat("Retrieved organization members from page", page, "successfully.\n")
        members_api_call
      },
      error = function(e) {
        cat("Error retrieving members from organization", org, "\n", "on page", page, "\n", e$message, "\n")
      },
      warning = function(w) {
        cat("Warning while retrieving members from organization", org, "\n", "on page", page, "\n", w$message, "\n")
      }
    )

    #members <- gh::gh("/orgs/{org}/members", org = org, .limit = 100, page = page)
    if (length(members) == 0) break
    # concatenate list of members as you loop through
    all_members <- c(all_members, members)
    page <- page + 1
  }

  members_list <- purrr::map(all_members, ~ get_names_and_usernames(.x$login))
  #cat("Retrieved names from member usernames.")
  return(members_list)
}

get_members_df <- function(org) {
  members_list <- get_members_list(org)
  purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
}

get_repos <- function(org) {
  repos <- tryCatch(
    {
      gh::gh("GET /orgs/:org/repos", org = org, .limit = Inf)
    },
    error = function(e) {
      cat("An error occcured:", e$message, "\n")
    }
  )

  purrr::map_chr(repos, "name")
}

get_open_milestone_objects <- function(owner, repo) {
  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo, state = "open", .limit = Inf)
  non_empty_milestones <- lapply(milestones, function(milestone) {
    if (check_that_milestone_is_non_empty(milestone)) {
      milestone
    }
    else NULL
  })
  # delete NULLs from list
  non_empty_milestones <- Filter(Negate(is.null), non_empty_milestones)
}

get_all_milestone_objects <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo, state = "all", .limit = Inf)
}

get_open_milestone_names <- function(org, repo) {
  milestones <- get_open_milestone_objects(org, repo)
  purrr::map_chr(milestones, "title")
}

#' @export
get_current_repo <- function() {
  basename(gert::git_find())
}



get_organization_name_from_url <- function(remote_url) {
  if (grepl("https://", remote_url)) {
    # https url
    matches <- regmatches(remote_url, regexec("https://[^/]+/([^/]+)/[^/]+", remote_url))
  }
  else if (grepl("git@", remote_url)) {
    # ssh url
    matches <- regmatches(remote_url, regexec("git@[^:]+:([^/]+)/[^/]+", remote_url))
  }
  else {
    stop("Unknown remote url format")
  }

  if (length(matches[[1]]) < 2) {
    stop("Unable to parse organization from url")
  }

  return(matches[[1]][2])
  # pattern <- "https://[^/]+/([^/]+)/[^/]+\\.git"
  # org_name <- sub(pattern, "\\1", url)
  # return(org_name)
}

get_organization <- function() {
  repo_path <- gert::git_find()
  remotes <- gert::git_remote_list(repo = repo_path)
  remote_url <- remotes$url
  get_organization_name_from_url(remote_url)
}

get_issue <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number",
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue

get_issue_comments <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/comments",
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_comments

get_issue_events <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/events",
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_events

get_issue_timeline <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/timeline",
         owner = owner, repo = repo, issue_number = issue_number)
}

get_issues <- function(owner, repo, milestone) {
  params <- c(owner, repo)
  gh::gh("GET /repos/:owner/:repo/issues",
         owner = owner, repo = repo, milestone = milestone_number, state = "all")
}

get_all_issues_in_repo <- function(owner, repo) {
  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues",
                  owner = owner,
                  repo = repo,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues",
                  owner = owner,
                  repo = repo,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  return(c(open_issues, closed_issues))
}

# sort by open/closed
get_all_issues_in_milestone <- function(owner, repo, milestone_name) {
  # get milestone number from name
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone_name))

  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues",
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues",
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  return(c(open_issues, closed_issues))
}

get_issues_info <- function() {
  issues <- tryCatch({
    gh::gh("GET /repos/{owner}/{repo}/issues",
           owner = get_organization(),
           repo = get_current_repo(),
           per_page = 100)
  }, error = function(e) {
    stop("Failed to fetch issues.")
  })


  if (is.null(issues) || length(issues) == 0) {
    stop("No issues found in the repository.")
  }

  issues_df <- purrr::map_df(issues, ~{
    tibble::tibble(
      number = .x$number,
      title = .x$title,
      milestone = if (!is.null(.x$milestone)) .x$milestone$title else NA,
      milestone_created = if (!is.null(.x$milestone) && !is.null(.x$milestone$created_at)) .x$milestone$created_at else NA
    )
  })

  # Filter out issues without a milestone
  issues_df <- issues_df %>% dplyr::filter(!is.na(milestone))

  return(issues_df)
}

get_milestone_url <- function(owner, repo, milestone_name) {
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone_name))

  milestone <- gh::gh(
    "GET /repos/:owner/:repo/milestones/:milestone_number",
    owner = owner,
    repo = repo,
    milestone_number = milestone_number
  )

  milestone$html_url
}

