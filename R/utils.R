#' @importFrom waiter Waiter spin_1
#' @import dplyr
#' @importFrom purrr map_df
NULL

create_waiter <- function(ns, message) {
  Waiter$new(
    id = ns("main_container"),
    html = tagList(
      spin_1(),
      h4(sprintf("%s", message), style = "color: white;")
    ),
    color = "darkgrey"
  )
}

convert_issue_df_format <- function(issue_df){
  issues_df <- map_df(issue_df, ~ {
    tibble(
      number = .x$number,
      title = .x$title,
      state = .x$state
    )
  })
  issues_choices <- issues_df %>%
    mutate(state = case_when(
      state == "open" ~ "Open Items",
      state == "closed" ~ "Closed Items"
    )) %>%
    split(.$state) %>%
    rev() %>%
    lapply(function(x) {
      setNames(nm = paste0("Item ", x$number, ": ", x$title))
    })
  return(issues_choices)
}

convert_commits_df_format <- function(commit_df){
  commits <- commit_df %>%
    split(.$date) %>%
    rev() %>%
    lapply(function(x) {
      setNames(
        object = x$commit,
        nm = x$display
      )
    })
}

split_issue_parts <- function(issue){
  issue_parts <- strsplit(sub("Item ", "", issue), ": ")[[1]]
  issue_number <- as.numeric(issue_parts[1])
  issue_title <- as.character(issue_parts[2])
  list(issue_number = issue_number, issue_title = issue_title)
}

