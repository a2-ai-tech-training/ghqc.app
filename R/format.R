format_issue_body <- function(checklist_type, file_path) {
  checklists <- get_checklists()
  file_items <- checklists[[checklist_type]]
  qc_checklist <- format_checklist_items(file_items)
  metadata <- format_metadata(checklist_type, file_path)
  glue::glue("## QC Checklist
  Note:  This checklist is NOT an exhaustive list of all checks.  User is encouraged to personalise checklist for individual study needs.

             {qc_checklist}

             ## Metadata
             {metadata}")
}

format_section_list <- function(section_name, section) {
  formatted_items <- sapply(section, function(item) {
    glue::glue("- [ ] {item}")
  })
  formatted_items_cat <- glue::glue_collapse(formatted_items, sep = '\n')

  glue::glue("### {section_name}

             {formatted_items_cat}

             ")
}

# functions to format body of issue
format_checklist_items <- function(checklist) {
  checklist_sections <- lapply(names(checklist), function(section_name) {
    section <- checklist[[section_name]]
    format_section_list(section_name, section)
  })
  glue::glue_collapse(checklist_sections, sep = '\n')
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




