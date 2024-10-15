is_yaml_without_headers <- function(yaml) {
  # Check if the YAML data is a named list
  if (!is.list(yaml_data) || is.null(names(yaml_data))) {
    return(FALSE)
  }

  # Check each element to see if it matches the expected structure
  is_valid <- all(sapply(yaml_data, function(item) {
    is.character(item) && !is.null(item)
  }))

  return(is_valid)
}

is_yaml_with_headers <- function(yaml) {

}



#' Validate the structure of yamls that can be parsed by ghqc
#'
#' @return
#' @export
#'
#' @examples
validate_checklist_yaml <- function(yaml_path) {
  browser()
  if (!fs::file_exists(yaml_path)) {
    rlang::abort(glue::glue("{yaml_path}: no such file"))
  }
  yaml_contents <- yaml::read_yaml(yaml_path)
  if(is_yaml_with_headers(yaml_contents)) return()
  if(is_yaml_without_headers(yaml_contents)) return()

  yaml_name <- basename(yaml_path)
  rlang::abort(glue::glue("{yaml_name} does not have a valid structure parsed by ghqc"))
}
