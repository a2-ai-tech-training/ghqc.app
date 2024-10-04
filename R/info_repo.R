check_ghqc_info <- function(repo_path = file.path("~/.local/share/ghqc.launcher", info_repo_name())) {
  check_ghqc_info_repo_exists()
  switch(info_repo_status(repo_path),
         "clone" = prompt_repo_clone(repo_path),
         "pull" = prompt_repo_pull(repo_path),
         "none" = info_files_desc(repo_path)
         )
}

install_ghqc_info <- function(repo_path = file.path("~/.local/share/ghqc.launcher", info_repo_name())) {
  check_ghqc_info_repo_exists()
  switch(info_repo_status(repo_path),
         "clone" = repo_clone(repo_path),
         "pull" = repo_pull(repo_path),
         "none" = info_files_desc(repo_path)
  )
}

# local status check #
info_repo_status <- function(repo_path) {
  if (!file.exists(repo_path)) return("clone")
  if (remote_repo_updates(repo_path)) return("pull")
  return("none")
}

remote_repo_updates <- function(repo_path) {
  remote_repo_updates <- gert::git_remote_ls(repo = repo_path, verbose = FALSE)$oid[1] != gert::git_info(repo = repo_path)$commit
}

# repo clone #
prompt_repo_clone <- function(repo_path) {
  cli::cli_alert_danger(sprintf("Info repository %s is not found locally", basename(repo_path)))
  yN <- readline(prompt = "Would you like to download the repository (y/N)? ")
  if (yN == "y") {
    repo_clone(repo_path)
  } else {
    cli::cli_alert_danger("Run 'ghqc::download_info_repo() before running any other ghqc.launcher")
  }
}

repo_clone <- function(repo_path) {
  cli::cli_alert(glue::glue("Attempting to clone {info_repo_url()} to {repo_path}..."))
  tryCatch({
    gert::git_clone(info_repo_url(), path = repo_path, verbose = FALSE)
    cli::cli_alert_success(glue::glue("Successfully cloned {info_repo_name()}"))
    cli::cli_h2(glue::glue("{basename(repo_path)} Local Content"))
    info_files_desc(repo_path)
  }, error = function(e) {
    error(.le$logger, glue::glue("Clone of {info_repo_name()} was not successful"))
    rlang::abort(message = e$message)
  })
}

# repo pull #
prompt_repo_pull <- function(repo_path) {
  cli::cli_alert_warning(sprintf("Info repository %s was found locally, but is not the most recent version", basename(repo_path)))
  yN <- readline(prompt = "Would you like to update the repository (y/N)? ")
  if (yN == "y") {
    repo_pull(repo_path)
  } else {
    cli::cli_alert_danger("Run 'ghqc::download_info_repo()' before running any other ghqc.launcher")
  }
}

repo_pull <- function(repo_path) {
  cli::cli_alert(glue::glue("Attempting to pull updates from {info_repo_url()} to {repo_path}..."))
  tryCatch({
    gert::git_pull(repo = repo_path, verbose = FALSE)
    cli::cli_alert_success(glue::glue("Successfully pulled updates of {info_repo_name()}"))
  }, error = function(e) {
    if (grepl("prevents checkout", e$message)) {
      repo_pull_conflict(repo_path)
      cli::cli_h2(glue::glue("{basename(repo_path)} Local Content"))
      info_files_desc(repo_path)
    } else {
      error(.le$logger, glue::glue("Update of {info_repo_name()} was not successful"))
      rlang::abort(message = e$message)
    }
  })
}

repo_pull_conflict <- function(repo_path) {
  ### TODO: describe merge conflict
  ### TODO: keep changes in files not in merge conflict
  yN <- readline(glue::glue("Would you like to download the most recent version of {info_repo_name()}? \nThis activity will delete all local changes to {repo_path} (y/N) "))
  if (yN == "y") {
    unlink(repo_path, recursive = TRUE)
    repo_clone(repo_path)
  } else {
    cli::cli_alert_danger("Solve merge conflicts before attempting to run 'ghqc::download_info_repo()'")
  }
}

# info repo description #
info_files_desc <- function(repo_path) {
  repo_files <- info_repo_files(repo_path)
  if (file.exists(repo_files[1])) {
    cli::cli_alert_success("logo.png successfully found")
  } else {
    cli::cli_alert_danger("logo.png not found")
  }

  if (file.exists(repo_files[2])) {
    note_found(repo_files[2])
  } else {
    cli::cli_alert_info("'note' not found. This file is not required")
  }

  if (file.exists(repo_files[3])) {
    if (length(repo_files[3]) == 0) {
      cli::cli_alert_danger("Checklists directory is empty")
    } else {
      checklists_found(repo_files[3])
    }
  } else {
    cli::cli_alert_danger("Checklists directory not found")
  }
}

info_repo_files <- function(repo_path) {
  file.path(repo_path, c("logo.png", "note", "checklists"))
}

note_found <- function(note_path) {
  cli::cli_alert_success("'note' successfully found")
  message(paste0("    ", readLines(note_path)))
}

checklists_found <- function(checklists_dir) {
  cli::cli_alert_success("Checklists directory successfully found")
  message(paste("    ", list.files(checklists_dir), collapse = "\n"))
}

# Checking and setting repo name and url #
check_ghqc_info_repo_exists <- function() {
  if (!file.exists("~/.Renviron")) info_repo_not_found()
  readRenviron("~/.Renviron")
  info_repo <- Sys.getenv("GHQC_INFO_REPO")
  if (info_repo == "") info_repo_not_found()
  if (substr(info_repo, 1, 8) != "https://") {
    error(.le$logger, glue::glue("GHQC_INFO_REPO ({info_repo}) does not start with 'https://'"))
    rlang::abort(sprintf("GHQC_INFO_REPO (%s) does not start with 'https://'"), info_repo)
  }
}

info_repo_name <- function() {
  gsub(".git", "", basename(info_repo_url()))
}

info_repo_url <- function() {
  check_ghqc_info_repo_exists()
  Sys.getenv("GHQC_INFO_REPO")
}

info_repo_not_found <- function() {
  error(.le$logger, "GHQC_INFO_REPO not found. Please set in ~/.Renviron")
  rlang::abort(message = "GHQC_INFO_REPO not found. Please set in ~/.Renviron")
}

#set path environ variable
#' @export
ghqc_set_info_repo <- function(repo_path = file.path("~/.local/share/ghqc.launcher", info_repo_name())) {
  not_files <- NULL
  if (!file.exists(file.path(repo_path, "checklists"))) not_files <- append(not_files, "Checklists directory")
  if (!file.exists(file.path(repo_path, "logo.png"))) not_files <- append(not_files, "logo.png")
  if (!is.null(not_files)) info_repo_files_not_found(not_files, repo_path)
  if (!exists("info_repo_path", .le)) assign("info_repo_path", repo_path, envir = .le)
}

info_repo_files_not_found <- function(not_files, repo_path) {
  error(.le$logger, glue::glue("{paste(note_files, collapse = ' and ')} are not found in {repo_path}. Please ensure files are present before continuing"))
  rlang::abort(glue::glue("{paste(note_files, collapse = ' and ')} are not found in {repo_path}. Please ensure files are present before continuing"))
}
