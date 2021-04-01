#' Error scribe
#'
#' Writes to a file an error message and the most recent line sent
#' to the R console.
#'
#' @param error Error message. Default value is the output of
#' \code{\link[base]{geterrmessage}()}
#' which returns the current error message.
#'
#' @details This function is called by the .zzz.R file run when the Grapho
#' package is first loaded into R. The function
#' \code{\link{start_error_scribe}()}
#'  sets
#' \code{\link{error_scribe}()} as the error handling function.
#' @export
error_scribe <- function(error = geterrmessage()) {
 
  # get last command the user ran
  temp_file_location <- tempfile()

  # try and save the users history to the temp file
  tryCatch({
    utils::savehistory(temp_file_location)
  }, warning = function(w) {
  }, error = function(e) {
    cat("Unable to create temporary history file. R returned the error:",
    e)
  })

  # try and read in the saved hisotry file
  user_history <- tryCatch({
    readLines(temp_file_location, warn = FALSE)
  }, warning = function(w) {
  }, error = function(e) {
    cat("Unable to create temporary history file. R returned the error:",
    e)
  })

  # now we can remove the temporary file
  if (file.exists(temp_file_location)) {
    #Delete file if it exists
    file.remove(temp_file_location)
  }

  # Notify user if history variable does not exist
  if (!exists("user_history")) {
    stop("User history does not exist.")
  }

  # Preallocate variable to hold the last command
  last_command <- expression()

  lines_to_retrieve <- 1

  # Run through the history from the end
  # Try to parse the retrieved lines
  # If we fail then go round again
  # NOTE: could be problematic if multiple lines run
  while (length(last_command) == 0) {

    # Load in history from file
    these_lines <- utils::tail(user_history, n = lines_to_retrieve)

    # Try to parse retrieved lines
    # History commands will be empty if parse fails
    try(last_command <- parse(text = these_lines), silent = TRUE)

    # Incriment the number of lines
    lines_to_retrieve <- lines_to_retrieve + 1
  }

  # Only run if grapho logging is enabled
  if (Sys.getenv("GRAPHO_LOGGING")) {

    log_file <- Sys.getenv("GRAPHO_LOG_FILE")
    if (Sys.getenv("GRAPHO_VERBOSE")) {
      message("Writing error command and error statment to log file")
    }

    result <- tryCatch({

      base::cat("\n", paste0("##------ ", date(), " ------##"),
                file = log_file, append = TRUE)
      base::cat("\n", "ERROR COMMAND", "\n",
                file = log_file, append = TRUE)
      base::cat("\n", these_lines,
                file = log_file, append = TRUE)

    }, warning = function(w) {
      cat("\n\n  WARNING when writing to console log file\n\n",
          w
      )
    }, error = function(e) {
      cat(
        paste0("\n\n  ERROR\n\n  We were unable to write to the",
                " console log file.\n\n ",
                " R returned the error message\n\n:
          "),
          e)
    }, finally = {
    })

    result <- tryCatch({
      base::cat("\n",
                paste0("##------ ", date(),
                 " ------##'"),
                 file = log_file,
                 append = TRUE)

      base::cat("\n", "ERROR MESSAGE",
                file = log_file, append = TRUE)

      base::cat("\n", unlist(error),
                file = log_file, append = TRUE)
    }, warning = function(w) {
      cat("\n\n  WARNING when writing to console log file\n\n",
          w
      )
    }, error = function(e) {
      cat(paste0("\n\n  ERROR\n\n ",
                 " We were unable to write to the console log file.\n\n  ",
                 "R returned the error message\n\n:
          "),
          e)
    }, finally = {
    })
  }

}
