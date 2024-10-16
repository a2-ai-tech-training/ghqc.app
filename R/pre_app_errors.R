rproj_root_dir <- function() {
  tryCatch(
    {
      root_dir <- rprojroot::find_rstudio_root_file()
      return(root_dir)
    },
    error = function(e) {
      waiter_hide()
      error(.le$logger, glue::glue("There was no Rproj file found within the directory '{getwd()}'."))
      rlang::abort(glue::glue("There was no Rproj file found within the directory '{getwd()}'."))
    }
  )
}


get_valid_checklists <- function() {
  tryCatch(
      {
        yaml_checklists <- get_checklists()
      },
      error = function(e) {
        error(.le$logger, glue::glue("There was an error retrieving checklists: {e$message}"))
        showModal(modalDialog("Error in getting checklists: ", e$message, footer = NULL))
      }
  )

  invalid_search <- grepl("INVALID - ", yaml_checklists)
  if (any(invalid_search)) {
    invalid_checklists <- paste(yaml_checklists[which(invalid_search)], collapse = ", ")
    warn(.le$logger, glue::glue("Checklists {invalid_checklists} are invalid and are not listed as options to choose in the app."))
    yaml_checklists <- yaml_checklists[!invalid_search]
  }

  return(yaml_checklists)
} # get_valid_checklists
