untracked_changes <- function() {
  status <- gert::git_status()
  not_staged <- subset(status, status == "modified" & !staged)
  nrow(not_staged) != 0
}

checkout_default_branch <- function() {
  branches <- gert::git_branch_list()

  # Determine the default branch
  default_branch <- if ("main" %in% branches$name) {
    "main"
  } else if ("master" %in% branches$name) {
    "master"
  } else {
    stop("Neither 'main' nor 'master' branch found")
  }

  # Checkout the default branch
  gert::git_branch_checkout(default_branch)
}

cleanup_branches <- function(temp_branch) {
  checkout_default_branch()
  gert::git_branch_delete(temp_branch)
}

checkout_temp_branch <- function(temp_branch, commit_sha, env) {
  # get all branches
  branches <- gert::git_branch_list()

  # if it doesn't already exist
  if (!temp_branch %in% branches$name) {
    # create it
    gert::git_branch_create(temp_branch, ref = commit_sha)
  }

  # check it out
  gert::git_branch_checkout(temp_branch)
  withr::defer(cleanup_branches(temp_branch), envir = env)
}

read_file_at_commit <- function(commit_sha, file_path) {
  # withr::defer(
  #   checkout_default_branch()
  # )
  # name of temp branch
  temp_branch <- paste0("temp-", commit_sha)
  # checkout temp branch (defer deletion)
  checkout_temp_branch(temp_branch, commit_sha, parent.frame())
  # read file in previous commit
  file_content <- readLines(file_path)
  #checkout_default_branch()
  #gert::git_branch_delete(temp_branch)
  return(file_content)
}

extract_line_numbers <- function(text) {
  match <- stringr::str_match(text, "@@ ([^@]+) @@")[2]
  first_set <- stringr::str_match(match, "^\\s*(\\d+)(?:,(\\d+))?")[,2:3]
  second_set <- stringr::str_match(match, "/\\s*(\\d+)(?:,(\\d+))?\\s*$")[,2:3]
  list(previous = as.numeric(first_set), current = as.numeric(second_set))
}

format_line_numbers <- function(numbers) {
  # if there's just one line, it prints like
  # ("@@ 1 / 1,5 @@"
  # instead of
  # ("@@ 1,1 / 1,5 @@"
  # to not be verbose
  # so this fixes it to be verbose for parsing ease
  if (is.na(numbers$previous[2])) {numbers$previous[2] <- 1}
  if (is.na(numbers$current[2])) {numbers$current[2] <- 1}

  previous <- glue::glue("{numbers$previous[1]}-{numbers$previous[1]+numbers$previous[2]-1}")
  current <- glue::glue("{numbers$current[1]}-{numbers$current[1]+numbers$current[2]-1}")

  glue::glue("@@ previous script: lines {previous} @@\n@@  current script: lines {current} @@")
}

add_line_numbers <- function(text) {
  # get start and end lines for prev and current scripts
  prev_lines <- stringr::str_match(text, "@@ previous script: lines (\\d+)-(\\d+) @@")[,2:3]
  current_lines <- stringr::str_match(text, "@@  current script: lines (\\d+)-(\\d+) @@")[,2:3]

  prev_start <- as.numeric(prev_lines[1])
  current_start <- as.numeric(current_lines[1])

  # get lines from text
  lines <- stringr::str_split(text, "\n")[[1]]

  # increment on prev and current lines
  prev_line_num <- prev_start
  current_line_num <- current_start

  new_lines <- sapply(lines, function(line) {
    if (stringr::str_detect(line, "^- ")) {
      # prev script line
      new_line <- stringr::str_replace(line, "^- ", glue::glue("- {prev_line_num} "))
      prev_line_num <<- prev_line_num + 1
    } else if (stringr::str_detect(line, "^\\+ ")) {
      # current script line
      new_line <- stringr::str_replace(line, "^\\+ ", paste0("+ {current_line_num} "))
      current_line_num <<- current_line_num + 1
    } else if (stringr::str_detect(line, "^  ")) {
      # unmodified line
      new_line <- stringr::str_replace(line, "^  ", paste0("  {current_line_num} "))
      current_line_num <<- current_line_num + 1
      prev_line_num <<- prev_line_num + 1
    } else {
      # empty line
      new_line <- line
    }
    new_line
  })

  glue::glue_collapse(new_lines, sep = "\n")
}

format_diff <- function(file_path, commit_sha_orig, commit_sha_new) {
  # get file contents at the specified commits
  compared_script <- read_file_at_commit(commit_sha_orig, file_path)
  current_script <- read_file_at_commit(commit_sha_new, file_path)

  diff_output <- diffobj::diffChr(compared_script, current_script, format = "raw", mode = "unified")
  diff_lines <- as.character(diff_output)

  # get the line indices with the file names (either 1,2 or 2,3 depending on if the the files were the same)
  file_index_start <- {
    if (diff_lines[1] == "No visible differences between objects.") {
      2
    }
    else {
      1
    }
  }


  # delete the lines with the file names
  diff_lines <- diff_lines[-c(file_index_start, file_index_start + 1)]

  # now file_index_start is the index where the line numbers are
  # extract the line numbers
  numbers <- extract_line_numbers(diff_lines[file_index_start])
  # reformat line numbers
  context_str <- format_line_numbers(numbers)
  # replace with new context_str
  diff_lines[file_index_start] <- context_str

  # check if last line is tick marks for formatting
  if (stringr::str_detect(diff_lines[length(diff_lines)], "```")) {
    diff_lines <- diff_lines[-c(length(diff_lines))]
  }



  format_diff_for_github <- function(diff_lines) {
    result <- c()
    for (line in diff_lines) {
      if (startsWith(line, ">")) {
        result <- c(result, paste0("+", substr(line, 2, nchar(line))))
      } else if (startsWith(line, "<")) {
        result <- c(result, paste0("-", substr(line, 2, nchar(line))))
      } else {
        result <- c(result, paste0(line))
      }
    }
    return(result)
  }

  github_diff <- format_diff_for_github(diff_lines)

  diff_cat <- glue::glue_collapse(github_diff, sep = "\n")
  diff_with_line_numbers <- add_line_numbers(diff_cat)
  glue::glue("```diff\n{diff_with_line_numbers}\n```")
}

get_comments <- function(owner, repo, issue_number) {
  comments <- gh::gh(
    "GET /repos/:owner/:repo/issues/:issue_number/comments",
    owner = owner,
    repo = repo,
    issue_number = issue_number
  )
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}


get_most_recent_comment_body <- function(comments_df) {
  most_recent_row <- comments_df %>% dplyr::arrange(desc(created_at)) %>%  dplyr::slice(1)
  most_recent_row$body
}

get_current_commit_from_comment <- function(body) {
  stringr::str_match(body, "\\* current QC request commit: ([a-f0-9]+)")[,2]
}
