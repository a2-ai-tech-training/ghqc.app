#' @import log4r
#' @export
get_gh_url <- function() {
  env_url <- Sys.getenv("GHQC_GITHUB_URL")
  env_url <- gsub("/$", "", env_url)

  # if GHQC_GITHUB_URL not set, use github.com
  if (!nzchar(env_url)) {
    # warn that default will be used
    warn(.le$logger, "No GHQC_GITHUB_URL environment variable found. Using Github URL \"https://github.com\". To specify otherwise, set GHQC_GITHUB_URL environment variable, likely in your ~/.Renviron file.")
    env_url <- "https://github.com"
  }
  # else if was set
  else {
    info(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL environment variable: {env_url}"))
  }

  # error if not https
  url_starts_with_https <- stringr::str_starts(env_url, "https://")
  if (!url_starts_with_https) {
    error(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
    rlang::abort(message = glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
  }

  # remove /api/v3 if at the end
  env_url <- stringr::str_remove(env_url, "/api/v3$")

  # get remote url
  remote <- get_remote()
  remote_url <- get_remote_url(remote)

  check_remote_matches_env_url(remote_url, env_url)

  return(env_url)
}

check_remote_matches_env_url <- function(remote_url, env_url) {
  if (remote_url != env_url) {
    error(.le$logger, glue::glue("GHQC_GITHUB_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\""))
    rlang::abort(message = glue::glue("GHQC_GITHUB_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\""))
  }
}

#' @import log4r
#' @export
get_gh_api_url <- function() {
  gh_url <- tryCatch(
    {
    get_gh_url()
    },
    error = function(e) {
      rlang::abort(message =  e$message)
    }
  )

  res <- glue::glue("{gh_url}/api/v3")
  info(.le$logger, glue::glue("Configured api url: {res}"))
  res
}

#' @import log4r
#' @export
get_gh_token <- function() {
  res <- Sys.getenv('GHQC_GITHUB_PAT')
  if (!nzchar(res)) {
    error(.le$logger, "No Github token found. Please set GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No Github token found. Please set GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
  }
  info(.le$logger, glue::glue("Retrieved GHQC_GITHUB_PAT environment variable: {substr(res, 1, 4)}************************************"))
  debug(.le$logger, glue::glue("Retrieved GHQC_GITHUB_PAT environment variable: {res}"))
  res
}

#' @import log4r
#' @export
check_github_credentials <- function() {
  if(file.exists("~/.Renviron")) readRenviron("~/.Renviron")

  api_url <- get_gh_api_url()
  token <- get_gh_token()

  if(token == ""){
    error(.le$logger, glue::glue(
    "To configure GitHub Enterprise connectitivity run:
    {usethis::ui_code(paste0('usethis::create_github_token(host = \"', get_gh_url(), '\")'))}
    and generate token
    Then use {usethis::ui_code('usethis::edit_r_environ()')}
    and fill in {usethis::ui_code('GHQC_GITHUB_PAT = [your token]')}"))
    stop("stopping", call. = TRUE)
  }

  ## workaround to avoid ssl error, remove the following lines if possible
  dconf <- gert::git_config_global()
  if (!identical(dconf$value[dconf$name %in% "http.sslverify"], "false")) {
    gert::git_config_global_set(name = "http.sslverify", value = "false")
  }

  if(nchar(token) == 40) {
    creds <- list(
      url = api_url,
      username = "PersonalAccessToken",
      password = token
    )

    tryCatch(
      {
        gitcreds::gitcreds_approve(creds)
      },
      error = function(e) {
        rlang::abort(message =  e$message)
        error(.le$logger, glue::glue("Could not set github credentials for {api_url}. Double check token or create a new token, then set it as GHQC_GITHUB_PAT environment variable"))
      }
    )

    info(.le$logger, glue::glue("GitHub credentials set"))

  }
  else {
    error(.le$logger, glue::glue("Token not equal to 40 characters. Please reset GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file."))
    rlang::abort(message = "Token not equal to 40 characters. Please reset GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
  }

  #confirm_creds()

}

# confirm_creds <- function() {
#
# }
