# milestone helper fns

# check if a milestone exists
milestone_exists <- function(title, owner, repo) {
  # list milestones
  milestones <- gh::gh("GET /repos/{owner}/{repo}/milestones",
    owner = owner,
    repo = repo
  )

  # return true if any matches
  any(sapply(milestones, function(milestone) milestone$title == title))
}

# look up number for milestone that exists - return null if it can't be found
look_up_existing_milestone_number <- function(params) {
  # list milestones
  milestones <- get_milestones(owner = params$owner, repo = params$repo)

  # try to get milestone number
  milestone <- sapply(milestones, function(milestone) {
    if (milestone$title == params$title) {
      milestone$number
    }
    else {
      NULL
    }
  })

  # filter null values - return first match
  milestone_number <- Filter(Negate(is.null), milestone)

  if (length(milestone_number) > 0) {
    milestone_number[[1]]
  }
  else {
    NULL
  }
}

create_milestone <- function(params) {
  do.call(gh::gh, c("POST /repos/{owner}/{repo}/milestones", params))
} # create_milestone

get_milestone_number <- function(params) {
  milestone_number <- {
    searched_number <- look_up_existing_milestone_number(params)
    if (!is.null(searched_number)) {
      #print("milestone already exists")
      searched_number
    }
    else {
      #print("milestone created")
      milestone <- create_milestone(params)
      milestone$number
    }
  } # milestone_number

  milestone_number
} # get_milestone_number


get_milestones <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo)
}

get_milestone_description <- function(title, milestones) {
  for (milestone in milestones) {
    if (milestone$title == title) {
      return(milestone$description)
    }
  }
  return(NULL)
}
