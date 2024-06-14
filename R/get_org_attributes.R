#' @export
get_names_and_usernames <- function(username) {
  user <- gh::gh("/users/{username}", username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}

#' @export
get_members_list <- function(org) {
  page <- 1
  # start with empty list of members
  all_members <- list()

  repeat {
    members <- gh::gh("/orgs/{org}/members", org = org, .limit = 100, page = page)
    if (length(members) == 0) break
    # concatenate list of members as you loop through
    all_members <- c(all_members, members)
    page <- page + 1
  }

  purrr::map(all_members, ~ get_names_and_usernames(.x$login))
}

#' @export
get_members_df <- function(org) {
  members_list <- get_members_list(org)
  purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
}


get_repos <- function(org) {
  repos <- gh::gh("GET /orgs/:org/repos", org = org, .limit = Inf)
  purrr::map_chr(repos, "name")
}

get_milestones <- function(org, repo) {
  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", owner = org, repo = repo, .limit = Inf)
  purrr::map_chr(milestones, "title")
}

#' @export
get_current_repo <- function() {
  basename(gert::git_find())
}

#' @export
get_organization <- function() {
  extract_organization_name <- function(remote_url) {
    if (grepl("github.com", remote_url)) {
      url <- sub("https://github.com/", "", remote_url)
      url <- sub("git@github.com:", "", url)
      url <- sub("\\.git$", "", url)

      # split the remaining string to get the organization name
      parts <- strsplit(url, "/")[[1]]
      if (length(parts) >= 2) {
        return(parts[1])
      }
    }
    return(NA)
  }

  repo_path <- gert::git_find()
  remotes <- gert::git_remote_list(repo = repo_path)
  remote_url <- remotes$url

  extract_organization_name(remote_url)
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
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone))
  gh::gh("GET /repos/:owner/:repo/issues",
         owner = owner, repo = repo, milestone = milestone_number, state = "all")
}
