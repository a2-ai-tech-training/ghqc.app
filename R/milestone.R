# milestone helper fns

# check if a milestone exists
milestone_exists <- function(title, owner, repo) {
  # list milestones
  milestones <- get_all_milestone_objects(owner, repo)

  # return true if any matches
  any_matches <- any(sapply(milestones, function(milestone) milestone$title == title))
  return(any_matches)
}

get_milestone_from_name <- function(owner, repo, name_in) {
  # list milestones
  milestones <- get_all_milestone_objects(owner = owner, repo = repo)
  # try to get milestone number
  milestone <- sapply(milestones, function(milestone) {
    if (milestone$title == name_in) {
      milestone
    }
    else {
      NULL
    }
  })
  # filter null values - return first match
  milestone <- Filter(Negate(is.null), milestone)

  if (length(milestone) > 0) {
    milestone[[1]]
  }
  else {
    NULL
  }
}

# look up number for milestone that exists - return null if it can't be found
look_up_existing_milestone_number <- function(params) {
  debug(.le$logger, glue::glue("Retrieving milestone: {params$title}"))
  milestone <- get_milestone_from_name(params$owner, params$repo, params$title)
  if (!is.null(milestone)) {
    info(.le$logger, glue::glue("Retrieved milestone: {params$title}, #{milestone$number}"))
    milestone$number
  }
  else {
    debug(.le$logger, glue::glue("Milestone: {params$title} does not currently exist"))
    NULL
    }
}

create_milestone <- function(params) {
  do.call(gh::gh, c("POST /repos/{owner}/{repo}/milestones", params))
} # create_milestone

get_milestone_number <- function(params) {
  searched_number <- look_up_existing_milestone_number(params)
  if (!is.null(searched_number)) {
    searched_number
  }
  else {
    debug(.le$logger, glue::glue("Creating milestone: {params$title}"))
    milestone <- create_milestone(params)
    info(.le$logger, glue::glue("Created milestone: {params$title}"))
    milestone$number
  }

} # get_milestone_number

get_milestone_description <- function(title, milestones) {
  for (milestone in milestones) {
    if (milestone$title == title) {
      return(milestone$description)
    }
  }
  return(NULL)
}
