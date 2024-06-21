create_gfm_file <- function(comment_body) {

  intro <- glue::glue(
    "---
  output:
    github_document:
      toc: false
  ---\n
  ")

  rmd_content <- paste0(
    intro,
    comment_body
  )

  rmd_file_path <- tempfile(fileext = ".Rmd")
  fs::file_create(rmd_file_path)
  # delete temporary rmd when it's time
  withr::defer_parent(unlink(rmd_file_path))
  writeLines(rmd_content, con = rmd_file_path)

  # rmd_file_path <- tempfile(tmpdir = getwd(), fileext = ".Rmd")
  # withr::defer_parent(fs::file_delete(rmd_file_path))
  # writeLines(md_content, rmd_file_path)

  rmarkdown::render(rmd_file_path, output_format = "github_document", clean = TRUE, quiet = TRUE)
  fs::file_delete(rmd_file_path)
  html_path <- stringr::str_replace(rmd_file_path, "\\.Rmd$", ".html")
  #print(html_path)
  modify_html(html_path)
  #rmd_file_path
}

modify_html <- function(html_file) {
  # Install necessary packages


  # Load the libraries
  library(xml2)
  library(rvest)

  # Define the function to add a CSS class based on text content
  # add_css_class <- function(node, prefix, css_class) {
  #   if (startsWith(xml_text(node), prefix)) {
  #     current_class <- xml_attr(node, "class")
  #     new_class <- if (is.na(current_class)) css_class else paste(current_class, css_class)
  #     xml_set_attr(node, "class", new_class)
  #   }
  # }
  replace_css_class <- function(node, prefix, new_class) {
    if (startsWith(xml_text(node), prefix)) {
      xml_set_attr(node, "class", new_class)
    }
  }

  # Read the HTML content
  html_content <- read_html(html_file)

  # Apply the function to all text nodes
  nodes <- html_nodes(html_content, xpath = "//*[text()]")
  lapply(nodes, replace_css_class, prefix = "-", new_class = "diff-remove")
  lapply(nodes, replace_css_class, prefix = "+", new_class = "diff-add")
  lapply(nodes, replace_css_class, prefix = "@@", new_class = "highlighted-line")

  # Add custom CSS to the HTML document
  css <- "
<style>
.diff-remove { font-weight: normal; color: #a61717; background-color: #ffe6e6; }
.diff-add { font-weight: normal; color: green; background-color: #e6ffe6; }
.highlighted-line { font-weight: bold; color: #AC94F4; }
</style>
"
  head_node <- xml_find_first(html_content, "//head")
  xml_add_child(head_node, read_html(css))

  # Write the modified content back to an HTML file
  modified_html_file <- "modified_example.html"
  write_html(html_content, modified_html_file)
}
