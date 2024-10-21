#' @import log4r
check_git_inited <- function() {
  tryCatch(
    {
      repo <- gert::git_find()
    },
    error = function(e) {
      error(.le$logger, "There was no local Git repository found.")
      rlang::abort("There was no local Git repository found.")
    }
  )
}

#' @import log4r
check_remote_set <- function() {
  remotes <- gert::git_remote_list()

  if (nrow(remotes) == 0) {
    error(.le$logger, "There is no GitHub remote URL set.")
    rlang::abort("There is no GitHub remote URL set.")
  }
}

#' @import log4r
check_upstream_set <- function(remote_name) {
  repo <- get_simple_path()

  current_branch <- gert::git_branch()

  if (is.null(current_branch)){
    error(.le$logger, glue::glue("There were no branches found for the existing repository: {repo} \n",
                                 "To create a branch, use one of the below for you default branch name: \n",
                                 "  git branch -M main \n",
                                 "  git branch -M master \n",
                                 "Push the branch to the remote repository using: \n",
                                 "  git push -u {remote_name} main \n",
                                 "  git push -u {remote_name} master"))
    rlang::abort(glue::glue("There were no branches found for the existing repo: {repo}"))
  }

  tracking_branch <- gert::git_branch_list() %>%
    dplyr::filter(name == current_branch & upstream != "") %>%
    dplyr::pull(upstream)


  if (length(tracking_branch) == 0) {
    error(.le$logger, glue::glue(
      "The current branch '{current_branch}' has no tracking information.  \n",
      "If you are planning on basing your work on an upstream branch that already exists at the remote, retrieve them with: \n",
      "  git fetch {remote_name} \n",
      "If you wish to set tracking information for this branch you can do so with: \n",
      "  git branch --set-upstream-to={remote_name}/{current_branch} {current_branch}"
    ))
    rlang::abort(glue::glue(
      "The current branch '{current_branch}' has no tracking information.
      Please set upstream and restart the app."
    ))
  }
}

#' @import log4r
get_env_url <- function() {
  env_url <- Sys.getenv("GITHUB_API_URL")
  env_url <- gsub("/$", "", env_url)
  env_url <- stringr::str_remove(env_url, "/api/v3$")
  if (!stringr::str_starts(env_url, "https://")) env_url <- paste0("https://", env_url)

  # env_url <- Sys.getenv("GHQC_GITHUB_URL")
  # env_url <- gsub("/$", "", env_url)
  #
  # # if GHQC_GITHUB_URL not set, use github.com
  # if (!nzchar(env_url)) {
  #   warn(.le$logger, "No GHQC_GITHUB_URL environment variable found. Using Github URL \"https://github.com\". To specify otherwise, set GHQC_GITHUB_URL environment variable, likely in your ~/.Renviron file.")
  #   env_url <- "https://github.com"
  # }
  # # else if was set
  # else {
  #   info(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL environment variable: {env_url}"))
  # }
  #
  # # error if not https
  # url_starts_with_https <- stringr::str_starts(env_url, "https://")
  # if (!url_starts_with_https) {
  #   error(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
  #   rlang::abort(message = glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
  # }
  #
  # # remove /api/v3 if at the end
  # env_url <- stringr::str_remove(env_url, "/api/v3$")
  # return(env_url)
}


#' @import log4r
#' @export
# get_gh_url <- function(remote_url) {
#   env_url <- get_env_url()
#
#   check_remote_matches_env_url(remote_url, env_url)
#
#   return(env_url)
# }

#' @import log4r
check_remote_matches_env_url <- function(remote_url) {
  env_url <- get_env_url()
  if (remote_url != env_url && env_url != "https://") {
    info(.le$logger, glue::glue("GITHUB_API_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\". No action necessary"))
  }
}

#' @import log4r
#' @export
get_gh_api_url <- function(remote_url) {
  tryCatch(
    {
      res <- glue::glue("{remote_url}/api/v3")
      info(.le$logger, glue::glue("Configured api url: {res}"))
      res
    }, error = function(e) {
      rlang::abort(message = e$message)
    }
  )
}

#' @import log4r
#' @export
get_gh_token <- function(api_url) {
  tryCatch({
    pat <- gitcreds::gitcreds_get(url = api_url)$password
    info(.le$logger, glue::glue("Found GitHub PAT for {api_url}: {paste0(substr(pat, 1, 4), strrep('*', nchar(pat)))}"))
    pat
  }, error = function(e) {
    error(.le$logger, message = glue::glue("Could not find GitHub PAT for {api_url} due to: {e$message}. Set your GitHub credentials before continuing"))
    rlang::abort(message = glue::glue("Could not find GitHub PAT for {api_url}. Set your GitHub credentials before continuing"), parent = e$parent)
  })
}

#' @import log4r
try_api_call <- function(url, token) {
  tryCatch({
    debug(.le$logger, glue::glue("Attempting test api call..."))
    gh::gh("GET /user", .api_url = url, .token = token)
    info(.le$logger, glue::glue("Successful test api call to {url}"))
  }, error = function(e) {
    pat_substr <- paste0(substr(token, 1, 4), strrep("*", nchar(token)))
    error(.le$logger, message = glue::glue("{url} could not be accessed using {pat_substr} due to: {e$message}. Ensure your GitHub credentials are correct before continuing"))
    rlang::abort(message = glue::glue("{url} could not be accessed using {pat_substr}. Ensure your GitHub credentials are correct before continuing", parent = e$parent))
  })
}

#' @import log4r
#' @export
check_github_credentials <- function() {
  # Check errors
  check_git_inited()
  check_remote_set()

  remote <- get_remote()
  remote_url <- get_remote_url(remote)
  check_upstream_set(remote$name)

  api_url <- get_gh_api_url(remote_url)
  token <- get_gh_token(api_url)
  try_api_call(api_url, token)
  check_remote_matches_env_url(remote_url)
  assign("github_api_url", api_url, envir = .le)
  remote
}

