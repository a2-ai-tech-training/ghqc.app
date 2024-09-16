.lci <- new.env()

#' @import log4r
install_client_git_repo <- function(){
  git_repo <- Sys.getenv("GHQC_INFO_REPO")

  if (git_repo == ""){
    error(.le$logger, "No github repo found. Please set GHQC_INFO_REPO environmental variable, likely in your ~/.Renviron file. (e.g. `GHQC_INFO_REPO=a2-ai/{company_name}.ghqc.info`)")
    rlang::abort(message = "No github repo found. Please set GHQC_INFO_REPO environmental variable, likely in your ~/.Renviron file. (e.g. `GHQC_INFO_REPO=a2-ai/{company_name}.ghqc.info`)")
  }

  tryCatch(
    {
      pkg_dwn_inf <- pak::pkg_install(git_repo, upgrade = TRUE)
      pkg_name <- pkg_dwn_inf$package
    }, error = function(e) {
      error(.le$logger, glue::glue("Installation of client specific package {git_repo} failed"))
      rlang::abort(message = glue::glue("Installation of client specific package {git_repo} failed"))
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

  client_pkg_name <- install_client_git_repo()
  assign("client_pkg_name", client_pkg_name, envir = .lci)
}
