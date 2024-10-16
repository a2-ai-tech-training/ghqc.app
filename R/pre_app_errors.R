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

  return(yaml_checklists)
} # get_valid_checklists
