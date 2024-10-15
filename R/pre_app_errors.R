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

