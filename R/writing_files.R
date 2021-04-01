# Functions which output files

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
  # write new plot to disk
  if (is.null(folder)) {
    stop("Please provide a folder to write the file to")
  }

  plot_format <- tolower(Sys.getenv("GRAPHO_PLOT_FILE_FORMAT"))

  format_png <- plot_format == "png"
  format_jpeg <- plot_format == "jpeg"

  if (!format_png & !format_jpeg) {
    stop("Plot format setting not png or jpeg")
  }

  # plot filename
  plot_file <- paste0(folder, "/",
                      create_filename("current_plot"), ".",
                      Sys.getenv("GRAPHO_PLOT_FILE_FORMAT"))

  # are we in RStudio and current device is RStudio device?
  is_rstudio <-
    Sys.getenv("GRAPHO_ENVIRONMENT") == "RStudio"

  using_rstudiogd <-
     attr(grDevices::dev.cur(), "names") == "RStudioGD"

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

    # get currently file type for plots
    plot_format <- tolower(Sys.getenv("GRAPHO_PLOT_FILE_FORMAT"))

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

#' @rdname log_session_information
#' @title Logs session information
#' @param return_location If TRUE then location of session information
#'  file is returned.
#' @description Run when
#'  Grapho is loaded and
#' records all of the R environment variables to a CSV file located in the
#' Grapho folder.
#' @importFrom utils write.csv write.table
#' @export
log_session_information <- function(return_location = FALSE) {

  session_information_location <-
    paste0(
      Sys.getenv("GRAPHO_FOLDER"),
      "/",
      create_filename("sessionInformation"),
      ".csv"
  )

  version_df <-
    as.data.frame(unlist(R.version))

  version_vars <- data.frame(
    stringsAsFactors = FALSE,
    var_name = row.names(version_df),
    value = version_df$`unlist(R.version)`

  )

  env_vars <-
    data.frame(
      stringsAsFactors = FALSE,
      var_name = c("LANG", "PATH", "R_HOME", "R_RD4PDF", "RSTUDIO"),
      value = c(Sys.getenv("LANG"),
                Sys.getenv("PATH"),
                Sys.getenv("R_HOME"),
                Sys.getenv("R_RD4PDF"),
                Sys.getenv("RSTUDIO"))
    )

  vars <- rbind(
    version_vars,
    env_vars
  )

  write.csv(
    row.names = FALSE,
    x = vars,
    file = session_information_location,
    fileEncoding = "UTF-8"
    )

  if (return_location) {
    session_information_location
  }
}

#' @rdname create_log_file
#' @title create_log_file
#' @description Run
#'  when Grapho is loaded. The log file is
#' created and the user is shown the location of the Grapho log file.
#' Grapho folder.
#' @param return_location If TRUE then location of log file is returned
#' @param show_messages If TRUE then message output displayed
#' @export
create_log_file <- function(return_location = FALSE, show_messages = TRUE) {

  log_file_location <- Sys.getenv("GRAPHO_LOG_FILE")
  verbose <- as.logical(Sys.getenv("GRAPHO_LOGGING"))

  # Let user know logging is enabled
  if (verbose & show_messages) {
    cat("\n Grapho logging is on \n")
  }

  # attempt to create create log file
  file.create(log_file_location, showWarnings = TRUE)

  if (return_location) {
    log_file_location
  }
}
