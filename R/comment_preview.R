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

  rmd_path <- tempfile(fileext = ".Rmd")
  fs::file_create(rmd_path)
  # delete temporary rmd when it's time
  withr::defer(fs::file_delete(rmd_path))
  writeLines(rmd_content, con = rmd_path)

  html_path <- stringr::str_replace(rmd_path, "\\.Rmd$", ".html")
  md_path <- stringr::str_replace(rmd_path, "\\.Rmd$", ".md")
  rmarkdown::render(rmd_path, output_format = "github_document", clean = TRUE, quiet = TRUE)
  # withr::defer_parent(fs::file_delete(rmd_path))
  withr::defer(fs::file_delete(html_path))
  withr::defer(fs::file_delete(md_path))

  modified_html_file <- modify_html(html_path)
  withr::defer_parent(fs::file_delete(modified_html_file))
  modified_html_file
}

modify_html <- function(html_file) {
  replace_css_class <- function(node, prefix, new_class) {
    if (startsWith(xml2::xml_text(node), prefix)) {
      xml2::xml_set_attr(node, "class", new_class)
    }
  }

  html_content <- rvest::read_html(html_file)

  nodes <- rvest::html_nodes(html_content, xpath = "//*[text()]")
  lapply(nodes, replace_css_class, prefix = "-", new_class = "diff-remove")
  lapply(nodes, replace_css_class, prefix = "+", new_class = "diff-add")
  lapply(nodes, replace_css_class, prefix = "@@", new_class = "highlighted-line")

  # add css to the html
  css <- "
  <style>
  .diff-remove { font-weight: normal; color: #a61717; background-color: #ffe6e6; }
  .diff-add { font-weight: normal; color: green; background-color: #e6ffe6; }
  .highlighted-line { font-weight: bold; color: #AC94F4; }
  </style>
"
  head_node <- xml2::xml_find_first(html_content, "//head")
  xml2::xml_add_child(head_node, read_html(css))

  modified_html_file <- tempfile(fileext = ".html")
  xml2::write_html(html_content, modified_html_file)
  modified_html_file
}
