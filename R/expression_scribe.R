#' Expression scribe
#'
#' The expression scribe is called when a top-level call is made. For instance,
#' when assigning a variable. The call and contents of plot devices written out
#' to the Grapho directory and log file set in the environment variables
#' GRAPHO_LOG_FILE and GRAPHO_FOLDER.
#'
#' @param top_level_expr Top level expressions sent to the R console
#' @param value List of arguments passed in top level expression
#' @param ok Boolean indicating if call is ok
#' @param visible Boolean indicating if visible
#'
#' @return Logical value TRUE to indicate the callback is complete
#'
#' @details Run after every
#' successful R console command. Errors which prevent a console command
#' finishing are recorded by the \code{\link{error_scribe}()} function.
#' @export
expression_scribe <- function(top_level_expr, value, ok, visible) {

  if (exists("last.warning")) {
    # Get last warning
    warning <- last.warning # will be null if there have been no
  } else {
    warning <- NULL
  }

  # Clear warnings list
  assign("last.warning", NULL, envir = baseenv())

  # Only run if logging is enabled
  if (Sys.getenv("GRAPHO_LOGGING")) {

    log_file <- Sys.getenv("GRAPHO_LOG_FILE")
    # if (Sys.getenv("GRAPHO_VERBOSE")) {
    #   message("Writing command to log file")
    # }

    # Try and write capture command to file
    result <- tryCatch({
      command <- gsub(pattern = "     ",
                      replacement = "\n",
                      x = deparse(top_level_expr))

      base::cat("\n",
        paste0("##------ ",
               date(), " ------##"),
               file = log_file, append = TRUE)

      base::cat("\n", "COMMAND", "\n",
                file = log_file, append = TRUE)

      base::write(command, file = log_file, append = TRUE)

    }, warning = function(w) {
      cat("\n\n  WARNING when writing to console log file\n\n",
          w
      )
    }, error = function(e) {
      cat(
        paste0("\n\n  ERROR\n\n  ",
               "We were unable to write to the console log file.\n\n  ",
               "R returned the error message\n\n:
          "),
          e)
    }, finally = {
    })

    if (!is.null(warning)) {#If there has been a warning

      # if (Sys.getenv("GRAPHO_VERBOSE")) {
      #   message("Writing warning message to log file")
      # }

      result <- tryCatch({
        base::cat("\n",
                  paste0("##------ ", date(), " ------##"),
                   file = log_file, append = TRUE)

        base::cat("\n", "WARNING",
                  file = log_file, append = TRUE)

        base::cat("\n", names(warning),
                  file = log_file, append = TRUE)

      }, warning = function(w) {
        cat("\n\n  WARNING when writing to console log file\n\n",
            w
        )
      }, error = function(e) {
        cat(
          paste0("\n\n  ERROR\n\n",
                 "   We were unable to write to the console log file.\n\n  ",
                 "R returned the error message\n\n:
          "),
            e)
      }, finally = {
      })

    }

    # Capture plot
    grapho_folder_location <- Sys.getenv("GRAPHO_FOLDER")

    devices_open <- length(grDevices::dev.list()) > 0
    recorded_plot_exists <- !is.null(grapho$recorded_plot)

    # if (!devices_open) {
    #   # write out message if verbose is on
    #   if (Sys.getenv("GRAPHO_VERBOSE")) {
    #     message("No plot devices found")
    #   }
    # }

    # if there is a plot device
    if (devices_open) {

      # write out message if verbose is on
      # if (Sys.getenv("GRAPHO_VERBOSE")) {
      #   message("Plot device detected")
      # }

      # record the current plot
      current_plot <- grDevices::recordPlot()

      # and if a recorded plot is in the grapho environment
      if (recorded_plot_exists) {

        # write message if grapho is set to verbose
        # if (Sys.getenv("GRAPHO_VERBOSE")) {
        #   message("Checking if stored and current plot are the same")
        # }

        # check if the recorded and current plot are identical
        is_different_plot <- !identical(current_plot,
                                        grapho$recorded_plot)

        # if we have a new plot
        if (is_different_plot) {

          # write message if grapho is set to verbose
          # if (Sys.getenv("GRAPHO_VERBOSE")) {
          #   msg <-
          #         "Plots are different so saving new plot in grapho environment"
          #   message(msg)
          # }

          # record new plot in grapho environment
          assign(x = "recorded_plot", envir = grapho, value = current_plot)

          # write out the plot file
          write_plot(folder = grapho_folder_location)

        }

        # if the current and recorded plot are the same
        if (!is_different_plot) {

          # write message if grapho is set to verbose
          # if (Sys.getenv("GRAPHO_VERBOSE")) {
          #   message("Grapho stored plot and current plot are identical")
          # }
        }
      }

      # if grapho has not yet recorded a plot
      if (!recorded_plot_exists) {

        # write message if grapho is set to verbose
        # if (Sys.getenv("GRAPHO_VERBOSE")) {
        #   message("Creating plot in grapho environment")
        # }

        # record new plot in grapho environment
        assign(x = "recorded_plot", envir = grapho, value = current_plot)

        # write out the plot
        write_plot(folder = grapho_folder_location)
      }

    }

    }

  # Return TRUE
  TRUE
}
