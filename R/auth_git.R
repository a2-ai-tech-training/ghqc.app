#' @import log4r
#' @export
get_ghe_url <- function() {
  res <- Sys.getenv("CPMS_GHE_URL")
  if (!nzchar(res)) {
    error(.le$logger, "No GHE URL found. Please set CPMS_GHE_URL environment variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No GHE URL found. Please set CPMS_GHE_URL environment variable, likely in your ~/.Renviron file.")
  }
  info(.le$logger, glue::glue("Retrived ghe url environment variable: {res}"))
  gsub("/$", "", res)
}

#' @import log4r
#' @export
get_ghe_api_url <- function() {
  res <- Sys.getenv("CPMS_API_URL", glue::glue("{get_ghe_url()}/api/v3"))
  if (!nzchar(res)) {
    error(.le$logger, "No GHE URL found. Please set CPMS_GHE_URL environment variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No GHE URL found. Please set CPMS_GHE_URL environment variable, likely in your ~/.Renviron file.")
  }
  info(.le$logger, glue::glue("Configured api url: {res}"))
  res
}

#' @import log4r
#' @export
get_ghe_token <- function() {
  res <- Sys.getenv('GITHUB_PAT_GHE-GSK_METWORX_COM')
  if (!nzchar(res)) {
    error(.le$logger, "No GHE token found. Please set GITHUB_PAT_GHE-GSK_METWORX_COM environment variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No GHE token found. Please set GITHUB_PAT_GHE-GSK_METWORX_COM environment variable, likely in your ~/.Renviron file.")
  }
  info(.le$logger, glue::glue("Retrived ghe token environment variable: {substr(res, 1, 4)}************************************"))
  res
}

#' @import log4r
#' @export
check_github_credentials <- function() {
  if(file.exists("~/.Renviron")) readRenviron("~/.Renviron")

  api_url <- get_ghe_api_url()
  token <- get_ghe_token()

  if(token == ""){
    error(.le$logger, glue::glue(
    "To configure GitHub Enterprise connectitivity run:
    {usethis::ui_code(paste0('usethis::create_github_token(host = \"', get_ghe_url(), '\")'))}
    and generate token
    Then use {usethis::ui_code('usethis::edit_r_environ()')}
    and fill in {usethis::ui_code('GITHUB_PAT_GHE-GSK_METWORX_COM = [your token]')}"))
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

    ## using internal function of gitcreds :(
    ## this is a workaround to the inconsistent gitcreds_set() behaviour

    gitcreds:::gitcreds$gitcreds_run("approve", creds, character(0))
    #usethis::ui_done("GitHub credentials set")
    info(.le$logger, glue::glue("GitHub credentials set"))
  }
  else {
    error(.le$logger, glue::glue("Token not equal to 40 characters. Please reset GITHUB_PAT_GHE-GSK_METWORX_COM environment variable, likely in your ~/.Renviron file."))
    rlang::abort(message = "Token not equal to 40 characters. Please reset GITHUB_PAT_GHE-GSK_METWORX_COM environment variable, likely in your ~/.Renviron file.")
  }

}
