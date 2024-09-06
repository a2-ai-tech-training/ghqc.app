check_stored_token_matches_renviron_token <- function(renviron_token, gitcreds_token) {
  if (renviron_token != gitcreds_token) {
    error(.le$logger, glue::glue("gitcreds_get token doesn't match .Renviron token"))
    error(.le$logger, glue::glue("gitcreds_get token: {gitcreds_token}"))
    error(.le$logger, glue::glue(".Renviron token: {renviron_token}"))
  }
  else {
    info(.le$logger, ".Renviron token matches gitcreds_get token")
  }
}

run_gitcreds_get <- function(url, renviron_token) {
  tryCatch({
    retrieved_creds <- gitcreds::gitcreds_get(url, use_cache = FALSE)
    gitcreds_token <- retrieved_creds$password

    check_stored_token_matches_renviron_token(renviron_token = renviron_token,
                                              gitcreds_token = gitcreds_token)

    return(retrieved_creds)
  },
  error = function(e) {
    error(.le$logger, e$message)
    error(.le$logger, "Could not retrieve credentials")

    return(NULL)
  })
}

try_api_call <- function(url) {
  tryCatch({
    user <- gh::gh("GET /user",
                   .api_url = url)
    info(.le$logger, "Successful API call")
    info(.le$logger, "Retrieved user info")
    #info(.le$logger, user)
    info(.le$logger, "Git authentication complete")
    return(0)
  },
  error = function(e) {
    error(.le$logger, "API call unsuccessful")
    error(.le$logger, e$message)

    return(1)
  })
}

run_gitcreds_approve <- function(creds) {
  info(.le$logger, glue::glue("Running gitcreds_approve..."))
  gitcreds::gitcreds_approve(creds)
}

run_gitcreds_reject <- function(creds) {
  # needs this weird format
  reject_creds <- list(url = creds$url)
  info(.le$logger, glue::glue("Running gitcreds_reject..."))
  gitcreds::gitcreds_reject(reject_creds)
}

get_renvion_token <- function() {
  info(.le$logger, "Retrieving GHQC_GITHUB_PAT environment variable from .Renviron...")
  renviron_token <- Sys.getenv('GHQC_GITHUB_PAT')
  info(.le$logger, glue::glue("Retrieved .Renviron token: {renviron_token}"))

  token_length <- nchar(renviron_token)

  if (token_length != 40) {
    error(.le$logger, ".Renviron token not 40 characters")
    error(.le$logger, glue::glue(".Renviron token length: {token_length}"))
  }
  else {
    info(.le$logger, ".Renviron token has correct length of 40 characters")
  }
  return(renviron_token)
}

get_url <- function() {
  info(.le$logger, "Retrieving GHQC_GITHUB_URL environment variable from .Renviron...")
  url <- Sys.getenv("GHQC_GITHUB_URL")
  info(.le$logger, glue::glue("Retrieved url: {url}"))

  return(url)
}

#' @export
ghqc_diagnose_git_auth <- function(org = get_organization(), repo = get_current_repo()) {
  url <- get_url()

  username <- "PersonalAccessToken"

  renviron_token <- get_renvion_token()

  # try api call
  api_call <- try_api_call(url)
  if (api_call == 0) return()

  desired_creds <- list(
    url = url,
    username = username,
    password = renviron_token
  )

  # FIRST: try to approve
  run_gitcreds_approve(desired_creds)

  # try api call
  api_call <- try_api_call(url)
  if (api_call == 0) return()

  # check what creds are stored
  retrieved_creds <- run_gitcreds_get(url = url,
                   renviron_token = renviron_token)

  # SECOND: try to remove stored creds and approve desired creds
  run_giteds_reject_then_approve(desired_creds = desired_creds, renviron_token = renviron_token)

  retrieved_creds2 <- run_gitcreds_get(url = url,
                                      renviron_token = renviron_token)

  # try api call
  api_call <- try_api_call(url)
  if (api_call == 0) return()

  else {
    error(.le$logger,
          glue::glue("Automatic git authentication unsuccessful. Manually set git credentials with gitcreds::gitcreds_set(\"{url}\"), then follow interactive prompts."))
    return()
  }

} # ghqc_diagnose_git_auth


run_giteds_reject_then_approve <- function(desired_creds, renviron_token) {
  tryCatch({
    run_gitcreds_reject(desired_creds)

  }, error = function(e) {
    error(.le$logger, "Failed to run gitcreds_reject")
    error(.le$logger, e$message)
  })

  retrieved_creds <- run_gitcreds_get(url = desired_creds$url,
                                       renviron_token = renviron_token)

  tryCatch({
    run_gitcreds_approve(desired_creds)

  }, error = function(e) {
    error(.le$logger, "Failed to run gitcreds_approve")
    error(.le$logger, e$message)
  })
}


