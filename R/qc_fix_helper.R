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

checkout_temp_branch <- function(temp_branch, commit_sha) {
  # get all branches
  branches <- gert::git_branch_list()

  # if it doesn't already exist
  if (!temp_branch %in% branches$name) {
    # create it
    gert::git_branch_create(temp_branch, ref = commit_sha)
  }

  # check it out
  gert::git_branch_checkout(temp_branch)
}

read_file_at_commit <- function(commit_sha, file_path) {
  # withr::defer(
  #   checkout_default_branch()
  # )
  # name of temp branch
  temp_branch <- paste0("temp-", commit_sha)
  # checkout temp branch (defer deletion)
  checkout_temp_branch(temp_branch, commit_sha)
  # read file in previous commit
  file_content <- readLines(file_path)
  checkout_default_branch()
  gert::git_branch_delete(temp_branch)
  return(file_content)
}

format_diff <- function(file_path, commit_sha_orig, commit_sha_new) {
  # get file contents at the specified commits
  compared_script <- read_file_at_commit(commit_sha_orig, file_path)
  current_script <- read_file_at_commit(commit_sha_new, file_path)

  diff_output <- diffobj::diffChr(compared_script, current_script, format = "raw", mode = "unified")

  # convert to character
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
  # extract the numbers from this line
  numbers <- stringr::str_extract_all(diff_lines[file_index_start], "\\d+")[[1]]
  numbers <- as.numeric(numbers)

  # reformat line numbers
  context_str <- glue::glue("@@ previous script: lines {numbers[1]}-{numbers[1]+numbers[2]-1} @@\n@@  current script: lines {numbers[3]}-{numbers[3]+numbers[4]-1} @@")
  # replace with new context_str
  diff_lines[file_index_start] <- context_str

  # check if last line is tick marks
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

  file_difference <- glue::glue_collapse(github_diff, sep = "\n")
  glue::glue("```diff\n{file_difference}\n```")
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
  current_commit <- stringr::str_match(body, "\\* current QC request commit: ([a-f0-9]+)")[,2]
  #original_commit <- stringr::str_match(body, "\\* original QC request commit: ([a-f0-9]+)")[,2]

  # list(
  #   current_commit = current_commit,
  #   original_commit = original_commit
  # )
  current_commit
}
