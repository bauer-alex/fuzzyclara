
#' Print diagnostic messages to console or log file
#'
#' @param message Diagnostic message as character
#' @param reset_logFile If TRUE, the log file \code{clustering_progress.log} is
#' newly created in the current working directory, and a potential previous
#' version of it is deleted. Defaults to FALSE.
#' @inheritParams clustering_sample
#'
print_logMessage <- function(message,
                             verbose_toLogFile = FALSE,
                             reset_logFile     = FALSE) {

  checkmate::assert_character(message, len = 1)
  checkmate::assert_logical(verbose_toLogFile, len = 1)
  checkmate::assert_logical(reset_logFile, len = 1)

  log_file <- "clustering_progress.log"

  # make sure no old log file exists
  if (reset_logFile && file.exists(log_file)) {
    file.remove(log_file)
  }

  # create a new log file, if necessary
  if (!file.exists(log_file)) {
    file.create(log_file)
  }

  # print the message ...
  if (!verbose_toLogFile) { # ... to the console
    message(message)

  } else { # ... to the log file
    write(message, file = log_file, append = TRUE)
  }
}
