.lci <- new.env()

#' @import log4r
get_client_git_url <- function() {
  git_url <- Sys.getenv("GIT_CLIENT_URL")

  # error if CLIENT_INFO_URL not set
  if (git_url == ""){
    error(.le$logger, "No client github url found. Please set GIT_CLIENT_URL environmental variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No client github url found. Please set GIT_CLIENT_URL environmental variable, likely in your ~/.Renviron file.")
  }

  # error if not https
  url_starts_with_https <- stringr::str_starts(git_url, "https://")
  if (!url_starts_with_https) {
    error(.le$logger, glue::glue("Retrieved GIT_CLIENT_URL: {git_url} does not start with https"))
    rlang::abort(message = glue::glue("Retrieved GIT_CLIENT_URL: {git_url} does not start with https"))
  }
  git_url
}

#' @import log4r
install_client_git_repo <- function(git_url){
  tryCatch(
    {
      host <- dirname(dirname(git_url))
      repo_name <- file.path(basename(dirname(git_url)), basename(git_url))
      if (grepl("https://github.com", host)) {
        host <- "https://api.github.com"
      } else {
        host <- file.path(host, "api/v3")
      }
      pkg_name <- devtools::install_github(repo_name, host = host, auth_token = Sys.getenv("GHQC_GITHUB_PAT"))
    }, error = function(e) {
      error(.le$logger, glue::glue("Installation of client specific package from {git_url} failed"))
      rlang::abort(message = glue::glue(message = e$message))
    }
  )

  # Since checklists, notes, and logo is now "hidden" down in the libPaths
  # (and the added complexity to check), checking for local changes has been removed
  # Additional checks to make sure current version are handled by pak
}

#' @import log4r
#' @export
load_client_info <- function(){
  if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
  git_url <- get_client_git_url()
  client_pkg_name <- install_client_git_repo(git_url)
  assign("client_pkg_name", client_pkg_name, envir = .lci)
}
