#' @import log4r
get_names_and_usernames <- function(username) {
  user <- gh::gh("GET /users/{username}", .api_url = dirname(gert::git_remote_list()$url), username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}

#' @import log4r
get_repos <- function(org) {
  debug(.le$logger, glue::glue("Retrieving repos in org {org}..."))
  repos <- tryCatch(
    {
      gh::gh("GET /orgs/:org/repos", .api_url = dirname(gert::git_remote_list()$url), org = org, .limit = Inf)
    },
    error = function(e) {
      error(.le$logger, glue::glue("Failed to get repos in org {org}. {e$message}"))
    }
  )

  purrr::map_chr(repos, "name")
}


filter_for_non_empty_milestones <- function(milestones) {
  non_empty_milestones <- lapply(milestones, function(milestone) {
    if (check_that_milestone_is_non_empty(milestone)) {
      milestone
    }
    else NULL
  })
  # delete NULLs from list
  non_empty_milestones <- Filter(Negate(is.null), non_empty_milestones)
  return(non_empty_milestones)
}

#' @import log4r
get_open_milestone_objects <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving open milestones in organization {owner}, repo {repo}..."))

  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", .api_url = dirname(gert::git_remote_list()$url), owner = owner, repo = repo, state = "open", .limit = Inf)
  info(.le$logger, glue::glue("Retrieved {length(milestones)} open milestones in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(milestones)
}

#' @import log4r
get_all_milestone_objects <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo, .api_url = dirname(gert::git_remote_list()$url), state = "all", .limit = Inf)
}

#' @import log4r
get_open_milestone_names <- function(org, repo) {
  milestones <- get_open_milestone_objects(org, repo)
  purrr::map_chr(milestones, "title")
}

#' @import log4r
#' @export
list_milestones <- function(org, repo) {
  milestones <- get_all_milestone_objects(org, repo)

  non_empty_milestones <- filter_for_non_empty_milestones(milestones)
  purrr::map_chr(non_empty_milestones, "title")
}

#' @import log4r
#' @export
get_current_repo <- function() {
  debug(.le$logger, glue::glue("Connecting to repository..."))

  repo <- basename(gert::git_find())

  info(.le$logger, glue::glue("Connected to repository: {repo}"))

  repo
}

#' @import log4r
get_organization_name_from_url <- function(remote_url) {
  # https url
  matches <- {
    if (grepl("https://", remote_url)) {
      regmatches(remote_url, regexec("https://[^/]+/([^/]+)/[^/]+", remote_url))
    }
    # ssh url
    else if (grepl("git@", remote_url)) {
      regmatches(remote_url, regexec("git@[^:]+:([^/]+)/[^/]+", remote_url))
    }
    else {
      NULL
      stop("Unknown remote url format")
    }
  } # matches

  # if could match org
  if (length(matches[[1]]) < 2) {
    stop("Unable to parse organization from url")
  }
  else {
    return(matches[[1]][2])
  }
} # get_organization_name_from_url

#' @import log4r
get_organization <- function() {
  debug(.le$logger, glue::glue("Connecting to organization..."))
  # repo
  debug(.le$logger, glue::glue("Retrieving repo path..."))
  repo_path <- gert::git_find()
  debug(.le$logger, glue::glue("Retrieved repo path: {repo_path}"))

  # remotes
  debug(.le$logger, glue::glue("Retrieving list of remotes..."))
  remotes <- gert::git_remote_list(repo = repo_path)
  remotes_string <- glue::glue_collapse(apply(remotes, 1, function(row) {
    glue::glue("name: {row['name']}, url: {row['url']}")
  }), sep = "\n")
  debug(.le$logger, glue::glue("Retrieved list of remotes: \n{remotes_string}"))

  # url
  debug(.le$logger, glue::glue("Retrieving first url in list of remotes..."))
  remote_url <- remotes$url[1]
  debug(.le$logger, glue::glue("Retrieved remote url: {remote_url}"))

  debug(.le$logger, glue::glue("Retrieving organization name from remote url..."))
  org_name <- get_organization_name_from_url(remote_url)
  debug(.le$logger, glue::glue("Retrieved organization name {org_name}"))

  info(.le$logger, glue::glue("Connected to organization: {org_name}", ))
  org_name
}

#' @import log4r
get_issue <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number", .api_url = dirname(gert::git_remote_list()$url),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue

#' @import log4r
get_issue_comments <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/comments", .api_url = dirname(gert::git_remote_list()$url),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_comments

#' @import log4r
get_issue_events <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/events", .api_url = dirname(gert::git_remote_list()$url),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_events

#' @import log4r
get_issue_timeline <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/timeline", .api_url = dirname(gert::git_remote_list()$url),
         owner = owner, repo = repo, issue_number = issue_number)
}

#' @import log4r
get_issues <- function(owner, repo, milestone) {
  params <- c(owner, repo)
  gh::gh("GET /repos/:owner/:repo/issues", .api_url = dirname(gert::git_remote_list()$url),
         owner = owner, repo = repo, milestone = milestone_number, state = "all")
}

#' @import log4r
get_all_issues_in_repo <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving all issues from repo: {repo}..."))
  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = dirname(gert::git_remote_list()$url),
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
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = dirname(gert::git_remote_list()$url),
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

  issues <- c(open_issues, closed_issues)
  num_issues <- length(issues)
  info(.le$logger, glue::glue("Retrieved {num_issues} issues from repo: {repo}"))
  return(issues)

}

# sort by open/closed
#' @import log4r
get_all_issues_in_milestone <- function(owner, repo, milestone_name) {
  debug(.le$logger, glue::glue("Retrieving all issues from milestone: {milestone_name}..."))
  # get milestone number from name
  milestone_number <- look_up_existing_milestone_number(list(owner = owner, repo = repo, title = milestone_name))

  # if the milestone dne, there are no issues in the milestone, return an empty vector
  if (is.null(milestone_number)) {
    info(.le$logger, glue::glue("milestone: {milestone_name} doesn't yet exist"))
    return(c())
  }

  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = dirname(gert::git_remote_list()$url),
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
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = dirname(gert::git_remote_list()$url),
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

  issues <- c(open_issues, closed_issues)
  info(.le$logger, glue::glue("Retrieved {length(issues)} issues from milestone: {milestone_name}"))
  return(issues)
}

#' @import log4r
get_issues_info <- function() {
  issues <- tryCatch({
    gh::gh("GET /repos/{owner}/{repo}/issues", .api_url = dirname(gert::git_remote_list()$url),
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

#' @import log4r
get_milestone_url <- function(owner, repo, milestone_name) {
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone_name))
  milestone <- gh::gh(
    "GET /repos/{owner}/{repo}/milestones/{milestone_number}", .api_url = dirname(gert::git_remote_list()$url),
    owner = owner,
    repo = repo,
    milestone_number = milestone_number
  )

  milestone$html_url
}

#' @import log4r
get_collaborators <- function(owner = get_organization(), repo = get_current_repo()) {
  tryCatch({
    query <- gh::gh("GET /repos/{owner}/{repo}/collaborators", .api_url = dirname(gert::git_remote_list()$url), .limit = Inf, owner = owner, repo = repo)
    members_list <- purrr::map(query, ~ get_names_and_usernames(.x$login))
    members_df <- purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
    return(members_df)
  }, error = function(e) {
    debug(.le$logger, glue::glue("No collaborators found from: {owner} and {repo}"))
    return(e)
  })
}
