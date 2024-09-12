.lci <- new.env()

#' @import log4r
logo_file_path <- function() file.path(.lci$client_repo_path, "logo.png")

#' @import log4r
get_client_git_url <- function() {
  git_url <- Sys.getenv("GIT_CLIENT_URL")

  # error if CLIENT_INFO_URL not set
  if (length(git_url) == 0){
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
check_client_local <- function(git_url){
  client_repo_name <- basename(tools::file_path_sans_ext(git_url))
  client_repo_path <- file.path("~",client_repo_name)

  if (!file.exists(client_repo_path)){
    # Case 1: Repo not in home dir, cloning down
    debug(.le$logger, glue::glue("{client_repo_name} not found in home directory. Attempting to clone {git_url} to {client_repo_path}..."))
    tryCatch(
      {
        gert::git_clone(git_url, path = client_repo_path)
        info(.le$logger, glue::glue("Successfully cloned {client_repo_name}"))
      }, error = function(e) {
        error(.le$logger, glue::glue("Clone of {client_repo_name} was not successful"))
        rlang::abort(message = e$message)
      }
    )
  } else {
    remote_commit_id <- gert::git_remote_ls(repo = client_repo_path)$oid[1]
    local_commit_id <- gert::git_info(repo = client_repo_path)$commit

    if (remote_commit_id != local_commit_id){
      # Case 2: Repo in home dir, but not most recent main branch commit
      debug(.le$logger, "Most recent remote commit ({remote_commit_id}) does not match local commit ({local_commit_id}). Attempting to pull down update to {client_repo_path}...")
      tryCatch(
        {
          gert::git_pull(repo = client_repo_path)
          info(.le$logger, glue::glue("Update has been successfully pulled down to {client_repo_path}"))
        }, error = function(e) {
          error(.le$logger, glue::glue("Update was unsuccessfully pulled down. Attempted to pull {client_repo_name} remote commit id {remote_commit_id} to {client_repo_path}"))
          rlang::abort(.le$logger, message = e$message)
        }
      )
    } else {
      # Case 3: Repo in home dir and is most recent main branch commit
      info(.le$logger, glue::glue("Most recent commit found in {client_repo_path}. No updates needed"))
    }
  }
  client_repo_path
}

move_logo <- function(client_repo_path){
  new_logo_loc <- file.path(.libPaths()[1], "ghqc/assets/a2ai.jpeg")
  file.copy(file.path(client_repo_path,"a2ai.jpeg"), new_logo_loc)
  new_logo_loc
}

#' @import log4r
#' @export
load_client_info <- function(){
  if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

  # get client url from ~./Renviron
  git_url <- get_client_git_url()

  # check if client local is cloned and most up to date commit
  client_repo_path <- check_client_local(git_url)
  assign("client_repo_path", client_repo_path, envir = .lci)

  new_logo_loc <- move_logo(client_repo_path)
}
