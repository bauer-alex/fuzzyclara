
#' Print diagnostic messages to console or log file
#'
#' @param message Diagnostic message as character
#' @param reset_logFile If TRUE, the log file \code{clustering_progress.log} is
#' newly created and a potential previous version of it is deleted. Defaults to
#' FALSE.
#' @inheritParams clustering_sample
#'
print_logMessage <- function(diag_message, verbose_toLogFile = FALSE,
                             reset_logFile = FALSE) {

  log_file <- "clustering_progress.log"

  if (reset_logFile && file.exists(log_file)) { # make sure no old log file exists
    file.remove(log_file)
  }
  if (!file.exists(log_file)) { # create a new log file, if necessary
    file.create(log_file)
  }

  if (!verbose_toLogFile) { # print to console
    message(diag_message)

  } else { # print to log file
    write(diag_message, file = log_file, append = TRUE)
  }
}
