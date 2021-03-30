#' Toggles Grapho recording
#'
#' Either stops or starts Grapho recording.
#'
#'@param show_messages If TRUE then message output displayed
#'@details Displays either
#'  'Grapho logging disabled'
#'or 'Grapho logging enabled' to inform the user is the logging functionality
#'of the Grapho package has been enabled or disabled. The enabling or disabling
#'of Grapho will be recorded in the Grapho log file.
#'
#'Grapho logging is set by the environment variable 'GRAPHO_LOGGING'. The
#'logging functionality in \code{\link{error_scribe}()} and
#'  \code{\link{expression_scribe}()} will only be
#'evaluated if GRAPHO_LOGGING is TRUE.
toggle_grapho <- function(show_messages = TRUE) {

  verbose <- as.logical(Sys.getenv("GRAPHO_LOGGING"))

  log_file <- Sys.getenv("GRAPHO_LOG_FILE")

  if (Sys.getenv("GRAPHO_LOGGING")) {

    Sys.setenv(GRAPHO_LOGGING = FALSE)
    if (verbose & show_messages) {
      cat("\n\n  Grapho logging disabled \n\n")
    }
    result <- tryCatch({
      base::cat("\n", paste0("##------ ",
                              date(), " ------##")
                              , file = log_file, append = TRUE)

      base::cat("\n", "GRAPHO CONTROL", file = log_file, append = TRUE)

      base::cat("\n", "GRAPHO LOGGING DISABLED", file = log_file, append = TRUE)

    }, warning = function(w) {
      cat("\n\n  WARNING when writing to console log file\n\n",
          w
      )
    }, error = function(e) {
      cat(
        paste0("\n\n  ERROR\n\n  ",
        "We were unable to write to the console log file.\n\n",
        "  R returned the error message\n\n:
          "),
          e)
    }, finally = {
    })
  } else {
    Sys.setenv(GRAPHO_LOGGING = TRUE)
    if (verbose & show_messages) {
      cat("\n\n  Grapho logging enabled \n\n")
    }

    result <- tryCatch({
      base::cat("\n",
                paste0("##------ ", date(), " ------##"),
                file = log_file, append = TRUE)

      base::cat("\n", "GRAPHO CONTROL", file = log_file, append = TRUE)

      base::cat("\n", "GRAPHO LOGGING ENABLED", file = log_file, append = TRUE)
    }, warning = function(w) {
      cat("\n\n  WARNING when writing to console log file\n\n",
          w
      )
    }, error = function(e) {
      cat(
        paste0("\n\n  ERROR\n\n  ",
        "We were unable to write to the console log file.\n\n",
        "  R returned the error message\n\n:
          "),
          e)
    }, finally = {
    })
  }
}
