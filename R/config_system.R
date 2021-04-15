# functions related to updating or
# saving configuration options

#' @rdname check_config_file_exists
#' @title Checks if config file is present
#' @description Attempts to load in config file and check suitable config
#'  can be read via the function \code{\link{get_latest_settings}()}. Run
#'  when package is loaded.
#' @return list containing latest config file settings and a logical
#'  indicating if there is a config file.
#' @export
check_config_file_exists <- function() {
  latest_settings <- get_latest_settings()

  list(
    settings = latest_settings,
    present = is.data.frame(latest_settings)
  )
}

#' @rdname check_for_config_file
#' @title Check for grapho config file
#' @description Creates config file is it is not present
#' @param config_file_location Location of config file.
#'  If no location passed then system default will be used.
#' @importFrom tools R_user_dir
#' @importFrom utils write.csv
#' @export
check_for_config_file <- function(config_file_location = NULL) {

  if (is.null(config_file_location)) {
    config_file_location <- get_config_file_location()
  }

  grapho_config_dir <- tools::R_user_dir("grapho", "config")

  # Create the config directory if it does not exist
  if (!dir.exists(grapho_config_dir)) {
    dir.create(grapho_config_dir, recursive = TRUE)
  }

  # Check if there is a config file
  config_file_present <- file.exists(config_file_location)

  # if a config file exists
  if (config_file_present) {
    message(paste0("Config file found. Run show_config() ",
    "to view your current config options"))
  } else { # create config file
    message("Config file not found. Creating config file.")
    write.csv(
      fileEncoding = "UTF-8",
      row.names = FALSE,
      x = data.frame(
        date = character(),
        options = character(),
        value = character()
      ),
      file = config_file_location
    )
  }
  config_file_present
}

#' @rdname check_if_folder_is_writable
#' @title Find out if folder can be written to
#' @param folder Folder to try and write to
#' @description Tries to write a file into a folder.
#' @return TRUE if folder is writable and false if folder cannot be written to
#' @importFrom utils write.csv
#' @export
check_if_folder_is_writable <- function(folder = NULL) {
  # Check if folder passed
  if (is.null(folder)) {
    stop("No folder location specified")
  }

  # Check file location is writable by writing and then removing file
  test_location <- paste0(folder, "test.csv")

  # Function to try and write to location
  test_write <- function() {
    write.csv("test", test_location, fileEncoding = "UTF-8")
    file.remove(test_location)
  }

  # Try to write and return TRUE is write is successful
  test_write()
}

#' @rdname get_latest_settings
#' @title Retrieves the latest config settings
#' @description Loads in the current config file by
#' calling read_config_file() and then returns
#' the latest folder, plot_file_format and verbose option
#' @return Dataframe containing latest options
#' @export
get_latest_settings <- function() {
  # try to read config file
  # function will error our if
  # no config file found
  config_file <- read_config_file()

  # Turn date column into date object
  config_file$date <-
    as.POSIXct(config_file$date)

  # Check if there are entries for all options.
  # Stop and warn user if there are not.
  if (sum(config_file$options == "folder") == 0) {
    stop("No folder option in config file.")
  }

  if (sum(config_file$options == "plot_file_format") == 0) {
    stop("No plot_file_folder option in config file.")
  }

  if (sum(config_file$options == "verbose") == 0) {
    stop("No verbose option in config file.")
  }

  ## FINDING LATEST OPTIONS
  folder_df <- config_file[config_file$options == "folder", ]
  latest_folder <- folder_df[folder_df$date == max(folder_df$date), ]

  plot_file_df <- config_file[config_file$options == "plot_file_format", ]
  latest_plot_file <-
    plot_file_df[plot_file_df$date == max(plot_file_df$date), ]

  verbose_df <- config_file[config_file$options == "verbose", ]
  latest_verbose <- verbose_df[verbose_df$date == max(verbose_df$date), ]

  # create dataframe returning latest options
  rbind(
    latest_folder,
    latest_plot_file,
    latest_verbose
  )

}

#' @rdname get_config_file_location
#' @title Retrieves the location of the config file
#' @description Gets the config file location
#' @return String config file location
#' @importFrom tools R_user_dir
#' @export
get_config_file_location <- function() {
  grapho_config_dir <- tools::R_user_dir("grapho", "config")

  # use test location if set
  test_location <- Sys.getenv("GRAPHO_TEST_CONFIG_DIR")
  if (length(test_location) > 1) {
    grapho_config_dir <- test_location
  }

  paste0(grapho_config_dir, "/config.csv")

}

#' @rdname read_config_file
#' @title Attempts to read the config file
#' @description Config file is loaded in, the attempted load location is echoed
#' and a Dataframe containing the contents of the config file is returned
#' @param print_location If TRUE then config file location is also printed
#' @return Dataframe containing the grapho configuration record
#' @importFrom utils read.csv
#' @export
read_config_file <- function(print_location = FALSE) {

  config_file_location <- get_config_file_location()

  if (print_location) {
    message(paste("Config file location is", config_file_location))
  }

  # Try to load config file
  read.csv(config_file_location, encoding = "UTF-8")

}


#' @rdname show_config
#' @title Print out config file
#' @param console If TRUE then forces the function to print out
#'  to the console if run in RStudio
#' @description Config file is loaded using the function
#' \code{\link{read_config_file}()} and displayed
#' @importFrom utils View
#' @export
show_config <- function(console = FALSE) {

  # try to read in config file
  grapho_config <- read_config_file()

  # show config file contents in View panel if in RStudio
  if ((Sys.getenv("RSTUDIO") == 1)) {
    if (console) { # if console option is TRUE
      print(grapho_config)
    } else { # Use the RStudio View function
      View(grapho_config)
    }
  } else { # print out config file
    print(grapho_config)
  }
}



#' @rdname set_config
#' @title Updates folder location, plot file formats or verbosity of grapho
#' @param folder Folder to save grapho records to
#' @param file_format Preferred file format for graphics files. JPG, PNG,
#'  SVG supported
#' @param verbose If TRUE then additional grapho messages are displayed
#'  in the terminal
#' @description Configuration options for grapho are set in the config
#' file and grapho global environment variables GRAPHO_FOLDER,
#'  GRAPHO_PLOT_FILE_FORMAT and GRAPHO_VERBOSE. The function
#'  \code{\link[base]{Sys.getenv}()} will print out all of
#'  your environment variables.
#' @export
set_config <- function(folder = NULL, file_format = NULL, verbose = NULL) {

  # check for config file
  # creates a config file if needed
  check_for_config_file()

  # update folder
  if (!is.null(folder)) {
    # try to set folder option in config file
    # this function will error if location is
    # not writable
    write_folder_location(folder = folder)

    # set system environment
    Sys.setenv(GRAPHO_FOLDER = folder)
  }

  # update file format
  if (!is.null(file_format)) {
    # try to set file format in config file
    # this function errors out if file format
    # is not png, jpg, or svg
    write_plot_file_format(plot_file_format = file_format)

    # set system environment
    Sys.setenv(GRAPHO_PLOT_FILE_FORMAT = file_format)
  }

  # update verbosity
  if (!is.null(verbose)) {
    # try to set the verbosity option in the config file
    # this function will error out if verbosity
    # is not a logical
    write_verbosity(verbosity = verbose)
  }

  # error out if no config option is provided
  if (is.null(folder) & is.null(file_format) & is.null(verbose)) {
    stop("No folder, file format or verbose option provided")
  }
}



#' @rdname write_config_file
#' @title Writes values to the grapho config file
#' @param option Config option being changed.
#'  Either 'folder' or 'file_format' are accepted
#' @param value Value for new option e.g., folder location
#' @description Config file is loaded and displayed
#' @importFrom utils write.csv
#' @export
write_config_file <- function(option = NULL, value = NULL) {

  # Try to load the config file
  grapho_config <- read_config_file()

  # Get current time
  timestamp <- Sys.time()

  # Notify user if an option is not passed to function
  if (is.null(option)) {
    stop("Option missing. Cannot add new entry to config file.")
  }

  # Notify user if a value is not passed to function
  if (is.null(value)) {
    stop("Value missing. Cannot add new entry to config file.")
  }

  # Update config file
  grapho_config <- rbind(
    data.frame(
      date = timestamp,
      options = option,
      value = value
    ),
    grapho_config
  )

  config_file_location <- get_config_file_location()

  # Try and write out the config file
  write.csv(
    file = config_file_location,
    x = grapho_config,
    fileEncoding = "UTF-8",
    row.names = FALSE
  )
}


#' @rdname write_config
#' @title Writes config setting to config file
#' @param folder Folder to save grapho records to
#' @param file_format Preferred file format for graphics
#'  files. JPG, PNG, SVG supported
#' @param verbose If TRUE then additional grapho messages are
#'  displayed in the terminal
#' @description Function is passed the folder, file_format and verbosity values
#' which are then written to the config file.
#' @export
write_config <- function(folder = NULL, file_format = NULL, verbose = NULL) {
  # inform user if not passing any values to function
  if (is.null(folder) | is.null(file_format) | is.null(verbose)) {
    stop("No value of either folder, file_format or verbose")
  }

  # write verbose option

}

#' @rdname write_folder_location
#' @title Write folder location
#' @param folder Folder to save grapho records to
#' @description Writes folder location to config file
#' @export
write_folder_location <- function(folder = NULL) {

  if (is.null(folder)) {
    stop("No folder location specified")
  }

  is_writable <- check_if_folder_is_writable(folder)

  if (!is_writable) {
    stop("Unable to write to folder location")
  }

  if (is_writable) {
    # Write new folder option to Grapho config file
    write_config_file(option = "folder", value = folder)
  }

}

#' @rdname write_verbosity
#' @title Write verbostiy setting
#' @param verbosity verbosity setting of either TRUE or FALSE
#' @description Writes verbosity option to config file
#' @export
write_verbosity <- function(verbosity = NULL) {

  # check if logical has been passed
  if (!is.logical(verbosity)) {
    stop("Verbosity argument is not a logical TRUE or FALSE value")
  }

  # Write new folder option to Grapho config file
  write_config_file(option = "verbose", value = verbosity)
}

#' @rdname write_plot_file_format
#' @title Write verbostiy setting
#' @param plot_file_format file format of
#'  either 'jpg', 'png', or 'svg'
#' @description Writes verbosity option to config file
#' @export
write_plot_file_format <- function(plot_file_format = NULL) {

  # Check if file format data type
  if (is.null(plot_file_format)) {
    stop("No plot file format provided.")
  }

  if (!is.character(plot_file_format)) {
    stop("File format is not a character")
  }

  plot_file_format <- tolower(plot_file_format)

  # TRUE if one of the possible file formats
  is_accepted_format <-
    (plot_file_format == "jpg") |
    (plot_file_format == "svg") |
    (plot_file_format == "png")

  # error is incorrect format
  if (!is_accepted_format) {
    stop("File format should be either 'png', 'jpg', or 'svg'")
  }

  # update config file if format is correct
  if (is_accepted_format) {

    # write new folder option to Grapho config file
    write_config_file(option = "plot_file_format", value = plot_file_format)
  }

}
