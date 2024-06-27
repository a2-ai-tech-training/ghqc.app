issue_to_markdown <- function(owner, repo, issue_number) {
  # collect issue info
  issue <- get_issue(owner, repo, issue_number)
  issue_comments <- get_issue_comments(owner, repo, issue_number)
  issue_events <- get_issue_events(owner, repo, issue_number)
  milestones <- get_milestones(owner, repo)
  timeline <- get_issue_timeline(owner, repo, issue_number)

  # issue
  creation_time <- humanize_time(issue$created_at)
  creator <- issue$user$login
  created_by <- glue::glue("{creator} at {creation_time}")
  issue_body <- glue::glue("
    **File:** {issue$title}

    **Issue number:** {issue$number}

    **Repository:** {repo}

    **QC Issue Created by:** {created_by}
  ")
  issue_section <- create_section("Issue", issue_body)

  # milestone
  milestone_section <- {
    if (!is.null(issue$milestone$title)) {
      milestone_body <- {
        description <- get_milestone_description(issue$milestone$title, milestones)
        if (!is.null(description)) {
          glue::glue("{issue$milestone$title}: {description}")
        }
        else {
          issue$milestone$title
        }
      }
      create_section("QC Identifier", milestone_body)
    }
    else ""
  }

  # assignees
  assignees_list <- sapply(issue$assignees, function(assignee) glue::glue("- {assignee$login}"))
  assignees_body <- glue::glue_collapse(assignees_list, sep = "\n\n")
  assignees_section <- create_section("Assignees", assignees_body)

  # checklist
  checklist_section <- create_section("Issue Body", issue$body)

  # comments
  comments_list <- process_comments(issue_comments)
  comments_body <- glue::glue_collapse(comments_list, sep = "\n\n")
  comments_section <- create_section("Comments", comments_body)

  # events
  events_list <- get_events_list(issue_events)
  events_body <- glue::glue_collapse(events_list, sep = "\n")
  events_section <- create_section("Events", events_body)

  # status
  closures <- events_list[grep("closed", events_list)]

  status <- {
    if (length(closures) == 0) {issue$state} # should be open (in theory)
    else {
      # get the last time it was closed, which is the current status
      last_closure <- closures[length(closures)]
      gsub("- ", "", last_closure)
    }
  }
  status_section <- create_section("Issue Status", status)

  timeline_list <- get_timeline_list(timeline)
  timeline_body <- glue::glue_collapse(timeline_list, sep = "\n")
  timeline_section <- create_section("Detailed Timeline", timeline_body)

  # put it all together
  paste0(
    issue_section,
    milestone_section,
    assignees_section,
    status_section,
    checklist_section,
    comments_section,
    events_section,
    timeline_section
  )
} # issue_to_markdown

markdown_to_pdf <- function(rmd_content, repo, milestone_name, input_name) {
  wd <- getwd()

  pdf_name <- {
    if (is.null(input_name)) {
      glue::glue("{repo}-{milestone_name}.pdf")
    }
    else {
      # they might have already put pdf in the name
      if (stringr::str_detect(input_name, "\\.pdf$")) {
        input_name
      }
      else {
        glue::glue("{input_name}.pdf")
      }
    }
  }

  # create temporary rmd
  rmd <- tempfile(fileext = ".Rmd")
  fs::file_create(rmd)
  # delete temporary rmd when it's time
  withr::defer_parent(unlink(rmd))
  writeLines(rmd_content, con = rmd)

  # create pdf from rmd
  pdf_path <- file.path(wd, pdf_name)
  suppressWarnings(rmarkdown::render(rmd, output_file = pdf_path, quiet = TRUE)) #
  pdf_path_abs <- normalizePath(pdf_path)
  return(glue::glue("Output file: {pdf_path_abs}"))
} # markdown_to_pdf

scrape_issue <- function(owner, repo, issue_number) {
  rmd_contents <- issue_to_markdown(owner, repo, issue_number)
  markdown_to_pdf(rmd_contents, repo, issue_number)
} # scrape

get_summary_table_col_vals <- function(issue) {
  metadata <- get_metadata(issue$body)
  close_data <- get_close_info(issue)

  file_path <- issue$title
  author <- ifelse(!is.null(metadata$author), metadata$author, "NA")
  qc_type <- ifelse(!is.null(metadata$qc_type), metadata$qc_type, "NA")
  file_name <- basename(file_path)
  #git_sha <- ifelse(!is.null(metadata$git_sha), metadata$git_sha, NA)
  qcer <- ifelse(length(issue$assignees) > 0, issue$assignees[[1]], "NA")
  issue_closer <- ifelse(!is.null(close_data$closer), close_data$closer, "NA")
  close_date <- ifelse(!is.null(close_data$closed_at), close_data$closed_at, "NA")

  c(
    file_path = file_path,
    author = author,
    qc_type = qc_type,
    file_name = file_name,
    #git_sha = git_sha,
    qcer = qcer,
    issue_closer = issue_closer,
    close_date = close_date
  )
}

get_summary_df <- function(issues) {
  col_vals <- lapply(issues, get_summary_table_col_vals)
  list_of_vectors <- lapply(col_vals, function(vec) {
    as.data.frame(as.list(vec))
  })

  df <- dplyr::bind_rows(list_of_vectors)
}

get_summary_table <- function(df) {
  ft <- flextable::flextable(df) %>%
    set_header_labels(file_path = "File Path",
                      author = "Author",
                      qc_type = "QC Type",
                      file_name = "File Name",
                      qcer = "QCer",
                      issue_closer = "Issue Closer",
                      close_date = "Close Date") %>%
    autofit() %>%
    theme_vanilla()
  ft
}

create_big_section <- function(section_title, contents) {
  glue::glue("# {section_title}\n{contents}\n\n\\newpage\n\n", .trim = FALSE)
} # create_section

#' @export
scrape_milestone <- function(owner, repo, milestone_name, pdf_name = NULL) {
  # issues
  issues <- get_all_issues(owner, repo, milestone_name)
  summary_df <- get_summary_df(issues)
  summary_csv <- tempfile(fileext = ".csv")
  withr::defer_parent(fs::file_delete(summary_csv))
  write.csv(summary_df, file = summary_csv, row.names = FALSE)
  withr::defer_parent(fs::file_delete(summary_csv))
  author <- Sys.info()[["user"]]

  issue_numbers <- sapply(issues, function(issue) issue$number)
  issue_markdown_strings <- sapply(issues, function(issue) issue_to_markdown(owner, repo, issue$number))
  issue_section_strs <- mapply(function(issue_str, issue) {
    create_big_section(issue$title, issue_str)
  }, issue_markdown_strings, issues)
  issue_sections <- glue::glue_collapse(issue_section_strs, sep = "\n\\newpage\n")

  header_path <- system.file("header.tex", package = "ghqc")
  image_path <- system.file("gsk.png", package = "ghqc")

  header_tex <- paste0(
    "\\usepackage{fancyhdr}\n",
    "\\pagestyle{fancy}\n",
    "\\fancyhead[R]{\\includegraphics[width=2cm]{", image_path, "}}\n",
    "\\fancyhead[C]{}\n",
    "\\fancyhead[L]{}\n",
    "\\setlength{\\headheight}{30pt}\n",
    "\\fancypagestyle{plain}{%\n",
    "    \\fancyhead[R]{\\includegraphics[width=2cm]{", image_path, "}}\n",
    "    \\renewcommand{\\headrulewidth}{0.4pt}\n",
    "}\n",
    "\\fancyfoot[C]{Page \\thepage\\ of \\pageref{LastPage}}\n",
    "\\usepackage{lastpage}\n"
  )
  writeLines(header_tex, header_path)

  date <- format(Sys.Date(), '%B %d, %Y')

  # doc intro
  intro <- glue::glue(
  "---
  title: GSK QC Report
  subtitle: {repo}, {milestone_name}
  author: {author}
  date: {date}
  output:
    pdf_document:
      toc: true
      toc_depth: 1
      includes:
        in_header: {header_path}
  ---

  \\newpage

  ")

  # summary table
  summary_table_section <- glue::glue(
  "```{{r setup, include=FALSE}}
  library(knitr)
  library(dplyr)
  library(flextable)
  knitr::opts_chunk$set(eval=FALSE, warning = FALSE)\n```\n\n",

  "```{{r, include=FALSE, eval=TRUE}}
  summary_df <- read.csv(\"{summary_csv}\")\n
  summary_df <- summary_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), \"NA\", .)))
  invisible(summary_df)\n```\n",

  "# Summary Table\n```{{r, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE}}
  ft <- flextable::flextable(summary_df)
  dimensions <- dim_pretty(ft)
  col_widths <- dimensions$widths * 0.8
  #row_heights <- dimensions$heights * 0.8
  ft <- ft %>%
  set_header_labels(file_path = \"File Path\",
  author = \"Author\",
  qc_type = \"QC Type\",
  file_name = \"File Name\",
  # git_sha = \"Git SHA\",
  qcer = \"QCer\",
  issue_closer = \"Issue Closer\",
  close_date = \"Close Date\") %>%
  set_table_properties(width = 1.0) %>% # , align = \"left\"
  width(j = seq_along(col_widths), width = col_widths) %>%
  fontsize(size = 9, part = 'all') %>%
  theme_vanilla()
  ft\n```\n\\newpage\n",
  .trim = FALSE)

  # appendix


  rmd <- paste0(
    intro,
    summary_table_section,
    issue_sections
  )

 #result <- suppressWarnings({
 capture.output({
      output <- markdown_to_pdf(rmd, repo, milestone_name, pdf_name)
  })
  output
 # })

}
