format_issue_body <- function(file_items, checklist_type, file_path) {
  qc_checklist <- format_checklist_items(file_items)
  metadata <- format_metadata(checklist_type, file_path)
  glue::glue("## QC Checklist
             {qc_checklist}

             ## Metadata
             {metadata}")
}


# functions to format body of issue
format_checklist_items <- function(file_items) {
  formatted_items <- sapply(file_items, function(item) {
    glue::glue("- [ ] {item}")
  })
  paste(formatted_items, collapse = "\n")
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




