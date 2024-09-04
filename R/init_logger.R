
.le <- new.env() # parent = emptyenv()

#' @import log4r
NULL

init_logger <- function() {
  LEVEL_NAMES <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  verbosity <- Sys.getenv("GHQC_VERBOSE", unset = "INFO")
  if (!(verbosity %in% LEVEL_NAMES)){
    cat("Invalid verbosity level. Available options are:", paste(LEVEL_NAMES, collapse = ", "), "\n")
  }

  # logger <- logger(verbosity, appenders = console_appender(logfmt_log_layout()))
  logger <- logger(verbosity, appenders = console_appender(my_layout))
  assign("logger", logger, envir = .le)
}

my_layout <- function(level, ...) {
  paste0(format(Sys.time()), " [", level, "] ", ..., "\n", collapse = "")
}

#' @export
ghqc_set_logger <- function(level) {
  LEVEL_NAMES <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (!(level %in% LEVEL_NAMES)){
    cat("Invalid verbosity level. Available options are:", paste(LEVEL_NAMES, collapse = ", "), "\n")
  }
  else {
    # update level
    logger <- logger(level, appenders = console_appender(my_layout))
    assign("logger", logger, envir = .le)
  }
}


