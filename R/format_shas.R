# get list of all shas
# make sure sorted chronilogically
# get up to point of sha at initial QC commit
# get commit messages
# get dates
# add numbering to df for easy comparison of chronilogicallness
# add "second most recent commit", "most recent commit" and "original qc commit" identifiers
# format in table

get_commits_df <- function() {
  initial_qc_sha
  gert::git_log(max = Inf)
}
