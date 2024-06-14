# create an issue for each file given in a yaml file
# - organize issues associated with a set of files with milestones
# - assign a different user to each issue for a given file

#' @export
create_issue <- function(file, issue_params) {
  # issue title is the name of the file
  issue_params$title <- file$name
  # body is checklist
  issue_params$body <- format_issue_body(file$items, file$checklist_type)

  # if file has assignees item, add to issue_params
  if (!is.null(file$assignees)) {
    issue_params$assignees <- I(file$assignees)
  }

  # create the issue
  issue <- do.call(gh::gh, c("POST /repos/{owner}/{repo}/issues", issue_params))

  # return the issue number
  issue$number
} # create_issue

#' @export
create_issues <- function(data) {
  # create list of issue_params to input to api call -
  # will build up in pieces because some are optional
  issue_params <- list(
    owner = data$owner,
    repo = data$repo
  )

  # if milestone is in data struct
  if (!is.null(data$milestone)) {
    # create milestone_params
    milestone_params <- list(
      owner = data$owner,
      repo = data$repo,
      title = data$milestone
    )

    # if a decription was given, add it to the milestone_params
    if (!is.null(data$description)) {
      milestone_params$description <- data$description
    }

    # add milestone to the issue_params
    issue_params$milestone <- get_milestone_number(milestone_params)
  }

  # create an issue for each file
  lapply(data$files, create_issue, issue_params)
} # create_issues


# test with "test_yamls/checklist.yaml"
#' @export
create_checklists <- function(yaml_path) {
  data <- read_and_validate_yaml(yaml_path)
  create_issues(data)
}


