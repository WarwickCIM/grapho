#' @rdname expression_recorder
#' @title Records expressions evaluated in the console.
#' The expression recorder is called when a top-level call is made.
#'  For instance, when assigning a variable. The call and contents
#' of plot devices written out to the Grapho directory and log
#' file set in the environment variables
#' GRAPHO_LOG_FILE and GRAPHO_FOLDER.
#'
#' @param top_level_expr Top level expressions sent to the R console
#' @param value List of arguments passed in top level expression
#' @param ok Boolean indicating if call is ok
#' @param visible Boolean indicating if visible
#'
#' @import digest
expression_recorder <- function(top_level_expr, value, ok, visible) {
  `%notin%` <- Negate(`%in%`)

  datestamp <- date()

  if ("grapho_archive" %notin% names(.GlobalEnv$.grapho$config)) {
    message(
      'Run setup_grapho() to start logging or detach("package:grapho", unload=TRUE)'
      )
    return(TRUE)
  } else {

    command <- gsub(x = deparse(top_level_expr),
                    pattern =  "     ",
                    replacement = "\n",
                    useBytes = TRUE)

    # check randomising of variables
    randomised_command <- randomise_variable_names(
      code_string = command,
      n_rand = 10)

    #message(randomised_command)


    record_warning <- FALSE
    if (exists("last.warning")) {
      this_warning <- last.warning
      last_warning <- .GlobalEnv$.grapho$store$last_warning
      if (!identical(this_warning, last_warning)){
        .GlobalEnv$.grapho$store$last_warning <- this_warning
        record_warning <- TRUE
      }
    }

    if (.GlobalEnv$.grapho$config$recording) {
      write_to_log("COMMAND", command,
                   datestamp,
                   .GlobalEnv$.grapho$config$grapho_log_file)
      # write_to_log("COMMAND", randomised_command,
      #              datestamp,
      #              .grapho$config$grapho_random_log_file)
      if (record_warning) {
        warning <- names(this_warning)
        write_to_log("WARNING", warning,
                     datestamp,
                     .GlobalEnv$.grapho$config$grapho_log_file)
        # write_to_log("WARNING", warning,
        #              datestamp,
        #              .GlobalEnv$.grapho$config$grapho_random_log_file)
      }
    }
  }

  # write out current plot if needed
  recorded_plot_exists <- !is.null(.GlobalEnv$.grapho_plot)

  devices_open <- length(grDevices::dev.list()) > 0

  # if there is a plot device
  if (devices_open) {
    # record the current plot
    current_plot <- grDevices::recordPlot()
    # and if a recorded plot is in the grapho environment
    if (recorded_plot_exists) {

      # check if the recorded and current plot are identical
      is_different_plot <- !identical(current_plot,
                                      .GlobalEnv$.grapho_plot)

      # if we have a new plot
      if (is_different_plot) {

        # record new plot
        .GlobalEnv$.grapho_plot <- current_plot

        # write out the plot file
        write_plot(
          folder = .GlobalEnv$.grapho$config$grapho_archive$current
          )

      }
    }

    # if grapho has not yet recorded a plot
    if (!recorded_plot_exists) {
      # record new plot in grapho environment
      .GlobalEnv$.grapho_plot <- current_plot
      # write out the plot
      write_plot(folder = .GlobalEnv$.grapho$config$grapho_archive$current)
    }

  }

  # Return TRUE
  TRUE
}

#' @rdname error_recorder
#' @title Records code errors.
#' The error recorder will write any errors to the log file. Commands
#' resulting in errors are not passed to the expression recorder, so we
#' also collect the most recent command from history.
#'
#' @param error error message
#' @export
#' @importFrom utils savehistory tail
error_recorder <- function(error = geterrmessage()) {
  `%notin%` <- Negate(`%in%`)

  datestamp <- date()

  # most recent command
  get_recent_command <- function() {
    # get last command the user ran
    temp_file_location <- tempfile()

    # try and save the users history to the temp file
    utils::savehistory(temp_file_location)

    # try and read in the saved hisotry file
    user_history <-
      readLines(temp_file_location, warn = FALSE)

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
    paste(last_command)
  }

  # prompt user if no config
  if ("grapho_archive" %notin% names(.GlobalEnv$.grapho$config)) {
    message(
      'Run setup_grapho() to start logging or detach("package:grapho", unload=TRUE)'
      )
  } else {

    error_message <- paste(error)

    # record if recording enabled
    if (.GlobalEnv$.grapho$config$recording) {
      write_to_log("ERROR COMMAND", get_recent_command(),
                   datestamp,
                   .GlobalEnv$.grapho$config$grapho_log_file)
      write_to_log("ERROR MESSAGE", error_message,
                   datestamp,
                   .GlobalEnv$.grapho$config$grapho_log_file)
    }
  }
}

#' @rdname write_plot
#' @title Saves the current plot to the grapho folder
#' @param folder folder to write plot into
#' @param return_location if TRUE then location of the plot on
#'  disk is returned.
#' @description Creates a file containing the current
#'  plot into the grapho folder.
#' The filename follows the convention time-userID-sessionID.
#' The \code{\link[rstudioapi]{savePlotAsImage}()} is used if the function is
#' run within RStudio. If the function is not run in RStudio then
#' \code{\link[grDevices]{dev.copy}()} is used.
#' @return Plot file location string if echo_location argument is TRUE
#' @export
#' @import rstudioapi
write_plot <- function(folder = NULL, return_location = FALSE) {

  # are we in RStudio and current device is RStudio device?
  is_rstudio <- Sys.getenv("RSTUDIO") == 1

  using_rstudiogd <-
    attr(grDevices::dev.cur(), "names") == "RStudioGD"

  plot_format <- .GlobalEnv$.grapho$config$image_format

  # plot filename
  plot_file <- paste0(folder, "/",
                      create_filename("current_plot"), ".",
                      plot_format)

  # Use RStudio API
  if (is_rstudio & using_rstudiogd) {

    # Print out current plot
    savePlotAsImage(
      file = plot_file,
      height = grDevices::dev.size(units = "px")[2],
      width = grDevices::dev.size(units = "px")[1],
      format = plot_format
    )

  }

  # Use base device
  if (!(is_rstudio & using_rstudiogd)) {

    # get current height and width
    dev_height <- grDevices::dev.size(units = "px")[2]
    dev_width <- grDevices::dev.size(units = "px")[1]

    # plot file format can be either jpg, png or svg
    grDevices::dev.copy(eval(parse(text = plot_format)),
                        plot_file,
                        width = dev_width,
                        height = dev_height)

    grDevices::dev.off()
  }

  # return the location of the plot
  # if echo_location is TRUE
  if (return_location) {
    return(plot_file)
  }
}

#' @rdname write_to_log
#' @title Write to log
#' Writes an expression or error message to log file
#'
#' @param type Type of expression, such as ERROR or COMMAND
#' @param message Expression to be written to log file
#' @param datestamp Time and date of entry
#' @param logfile Location of log file
write_to_log <- function(type, message, datestamp, logfile) {
  cat(paste0("##------ ", datestamp, " ------##"),
      file = logfile,
      append = TRUE)

  cat("\n", type, "\n",
      file = logfile,
      append = TRUE)

  write(message, file = logfile,
        append = TRUE)
}

randomise_variable_names <- function(code_string, n_rand) {

  df <- utils::getParseData(
    parse(
      text = str2expression(code_string)
    ))

  df <- df[df$terminal==TRUE,]

  assignment_locations <- which(
    (df$token == 'EQ_ASSIGN')|
      (df$token == 'LEFT_ASSIGN'))

  if (length(assignment_locations) > 0) { # if there are any assignments

    # pick out variables
    variable_indexes <- assignment_locations - 1
    variables <- df$text[variable_indexes]

    # find new variables
    new_variables <- variables[
      !variables %in% .GlobalEnv$.grapho_variable_table$var
      ]

    if (length(new_variables) > 0) { # if there are novel variables
      # add new variables to table
      new_var_table <- data.frame(
        var = new_variables,
        var_rand = replicate(
          n = length(new_variables),
          expr = paste(
            sample(LETTERS, n_rand, replace = TRUE),
            collapse = ''
          )
        )
      )

      # add new columns to variable table
      .GlobalEnv$.grapho_variable_table <-
        rbind(
          .GlobalEnv$.grapho_variable_table,
          new_var_table
        )
    }

    # replace variables in code string
    for (variable in .GlobalEnv$.grapho_variable_table$var){
      variable_random <- .GlobalEnv$.grapho_variable_table$var_rand[
        .GlobalEnv$.grapho_variable_table$var == variable
      ]

      pattern = paste('[^|\\W](', variable, ')\\W', sep = '')

      code_string <- gsub(
        pattern = pattern,
        replacement = variable_random,
        x = code_string)
    }

  }

  #print(code_string)

  code_string
}
