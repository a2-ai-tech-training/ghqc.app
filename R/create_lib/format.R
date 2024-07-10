#' @export
get_checklists <- function() {
  checklists_path <- system.file("checklists", package = "ghqc")
  yaml_checklists <- list.files(checklists_path, pattern = "\\.ya?ml$", full.names = TRUE)
  checklists_data <- sapply(yaml_checklists, function(yaml_checklist) {
    yaml::read_yaml(yaml_checklist)
  }, USE.NAMES = FALSE)
  return(checklists_data)
}

format_issue_body <- function(checklist_type, file_path) {
  checklists <- get_checklists()
  file_items <- checklists[[checklist_type]]
  qc_checklist <- format_checklist_items(file_items)
  metadata <- format_metadata(checklist_type, file_path)
  glue::glue("## {checklist_type}
  Note: This checklist is NOT an exhaustive list of all checks. User is encouraged to personalise checklist for individual study needs.\n\n{qc_checklist}\n\n## Metadata\n\n{metadata}")}

format_items <- function(items) {
  formatted_items <- sapply(items, function(item) {
    glue::glue("- [ ] {item}")
  })
  glue::glue_collapse(formatted_items, sep = '\n')
}

format_section_list <- function(section_name, items) {
  formatted_items <- format_items(items)

  glue::glue("### {section_name}\n\n{formatted_items}\n\n")
}

# functions to format body of issue
format_checklist_items <- function(checklist) {
  names <- names(checklist)
  # if no sub-headers
  if (is.null(names)) {
    return(format_items(checklist))
  }
  # else, sub-headers
  else {
    checklist_sections <- lapply(names, function(section_name) {
      section <- checklist[[section_name]]
      format_section_list(section_name, section)
    })
    return(glue::glue_collapse(checklist_sections, sep = '\n'))
  }
}

get_sha <- function() {
  commits <- gert::git_log()
  commits$commit[1]
}

format_metadata <- function(checklist_type, file_path) {
  author <- Sys.info()[["user"]]
  qc_type <- checklist_type
  script_hash <- digest::digest(file = file_path)
  git_sha <- get_sha()
  glue::glue("
             * author: {author}
             * qc_type: {qc_type}
             * script_hash: {script_hash}
             * git_sha: {git_sha}")
}




