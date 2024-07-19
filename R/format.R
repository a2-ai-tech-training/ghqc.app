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

get_git_user <- function() {
  git_user_info <- gert::git_config_global()
  user_name <- git_user_info[git_user_info$name == "user.name", "value"]
}

format_metadata <- function(checklist_type, file_path) {
  author <- get_git_user()
  qc_type <- checklist_type
  script_hash <- digest::digest(file = file_path)
  git_sha <- get_sha()
  glue::glue("
             * author: {author}
             * qc type: {qc_type}
             * script hash: {script_hash}
             * git sha: {git_sha}")
}




