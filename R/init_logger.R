#' @import log4r
NULL

init_logger <- function() {
  LEVEL_NAMES <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  verbosity <- Sys.getenv("GHQC_VERBOSE", unset = "INFO")
  if (!(verbosity %in% LEVEL_NAMES)){
    cat("Invalid verbosity level. Available options are:", paste(LEVEL_NAMES, collapse = ", "), "\n")
  }
  logger(verbosity, appenders = console_appender(logfmt_log_layout()))
}


