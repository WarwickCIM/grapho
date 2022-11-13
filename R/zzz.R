.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "Good ", get_salulatation(), " and welcome to grapho")
  )

  # new flow
  # Check if there is a config file
  #   yes - load existing settings
  #   no - create config file
  #      - create default grapho directory
  #         (notify user temp is created in hoem directory)
  #         prompt user to run setup to configure dir location
  #
  #   grapho_setup
  #     Notify user that they are configuring grapho options
  #     prompt to press enter and continue
  #
  #     Set new grapho folder
  #
  #     Copy over any file from existing grapho folder and
  #     remove old folder

  packageStartupMessage(
    "You can learn more about the package via the grapho_info() function")

  past_state_loaded <- load_past_state()
  generate_ids()

  if (past_state_loaded) {
    packageStartupMessage("Run show_config() to view your grapho configuration")

    grapho_archive_location <- .GlobalEnv$.grapho$config$grapho_archive$current

    # Create grapho archive if there is no recorded archive location
    if(!is.null(grapho_archive_location)) {
      dir.create(grapho_archive_location)
    }

    # create grapho archive if not exists
    # useful if archive location has been deleted after being set
    if (!dir.exists(grapho_archive_location)) {
      dir.create(grapho_archive_location)
    }

    #create new log file
    .GlobalEnv$.grapho$config$grapho_log_file <-
      paste0(
        grapho_archive_location,
        "/",
        create_filename("consolelog"),
        ".txt"
      )
    file.create(.GlobalEnv$.grapho$config$grapho_log_file)

    # create random variable log file
    .GlobalEnv$.grapho$config$grapho_random_log_file <-
      paste0(
        grapho_archive_location,
        "/",
        create_filename("random_consolelog"),
        ".txt"
      )
    file.create(.GlobalEnv$.grapho$config$grapho_random_log_file)

    # create session variable name store - for random log file
    .GlobalEnv$.grapho_variable_table <- data.frame(
      var = character(),
      var_rand = character()
    )
  } else {
    packageStartupMessage("Run setup_grapho() to configure the grapho package")
  }

  # insert expression recorder
  while (is.element("expression_recorder", getTaskCallbackNames()))
    removeTaskCallback("expression_recorder")
  addTaskCallback(expression_recorder, name = "expression_recorder")

  # insert error recorder
  options(
    error = function(...) {
      grapho::error_recorder()
    }
  )

}

#' @rdname generate_ids
#' @title Creates session and user IDs then grapho savepaths.
#' @import digest
generate_ids <- function() {

  .GlobalEnv$.grapho$ids <- list()

  .GlobalEnv$.grapho$ids$user_id <-
    substr(digest::digest(
      c(Sys.getenv("HOME"),
        Sys.getenv("LANG"),
        Sys.getenv("R_PLATFORM")),
      algo = "sha512"
    ),  1, 40)

  .GlobalEnv$.grapho$ids$session_id <-
    substr(digest::digest(
      date(), algo = "sha512"
    ), 1, 40)

  save_grapho_state()
}

load_past_state <- function(){

  # grapho_config_folder <-
  #   tools::R_user_dir("grapho", "config")
  #
  # grapho_state_file_location <-
  #   paste0(grapho_config_folder, '/grapho_state.RDS')
  #
  # print(grapho_state_file_location)
  #
  # if (file.exists(grapho_state_file_location)) {
  #
  #   # load previous state
  #   .GlobalEnv$.grapho <- readRDS(grapho_state_file_location)
  #   return(TRUE)
  #
  # } else {

    grapho_config_folder <-
      tools::R_user_dir("grapho", "config")

    grapho_state_file_location <-
      paste0(grapho_config_folder, '/grapho_state.RDS')

    # create new state
    .GlobalEnv$.grapho <- list()
    .GlobalEnv$.grapho$grapho_state_file <-
      grapho_state_file_location
    .GlobalEnv$.grapho$config$recording <-
      FALSE

    # save state
    # create config directory if required
    if (!dir.exists(grapho_config_folder)) {
      dir.create(grapho_config_folder, recursive = TRUE)
    }

    # create space for last warning store
    .GlobalEnv$.grapho$store <- list()
    # last warning is stored here
    .GlobalEnv$.grapho$store$last_warning <- NULL

    # Need to set default grapho folder location
    saveRDS(.GlobalEnv$.grapho, grapho_state_file_location)

    return(FALSE)
#  }

}

create_filename <- function(filetype) {
  paste0(
    format(Sys.time(), "%y%d%mT%H%M%S"), "-",
    filetype, "-",
    .GlobalEnv$.grapho$ids$user_id, "-",
    .GlobalEnv$.grapho$ids$session_id
  )
}

get_salulatation <- function() {
  # get timezone
  current_hour <- as.numeric(
    format(Sys.time(), "%H")
  )

  #  find our current period of the day
  if ((current_hour == 0) | (current_hour > 0 & current_hour < 12)) {
    this_period <- "morning"
  }

  if ((current_hour == 12) | (current_hour > 12 & current_hour < 18)) {
    this_period <- "afternoon"
  }

  if ((current_hour == 18) | (current_hour > 18 & current_hour < 21)) {
    this_period <- "evening"
  }

  if ((current_hour == 21) | (current_hour > 21 & current_hour < 0)) {
    this_period <- "night"
  }

  this_period
}

#' @rdname show_config
#' @title Displays grapho config options in a user readable format.
#' @param return_dataframe If TRUE then a data frame containing your
#' current settings is returned
#' @export
#' @import digest
show_config <- function(return_dataframe = FALSE) {

  grapho_recording <- ifelse(.GlobalEnv$.grapho$config$recording,
                            "ENABLED",
                            "DISABLED")

  settings <- data.frame(
    Setting = c(
      'Grapho recording',
      'Grapho archive',
      'Past archives',
      'Plot file format',
      'User ID',
      'Session ID',
      'Log file location',
      'Randomised log file location'
    ),
    Value = c(
      grapho_recording,
      .GlobalEnv$.grapho$config$grapho_archive$current,
      paste(.GlobalEnv$.grapho$config$grapho_archive$past, collapse = ', '),
      .GlobalEnv$.grapho$config$image_format,
      .GlobalEnv$.grapho$ids$user_id,
      .GlobalEnv$.grapho$ids$session_id,
      .GlobalEnv$.grapho$config$grapho_log_file,
      .GlobalEnv$.grapho$config$grapho_random_log_file
    ), stringsAsFactors = FALSE
  )

  if(return_dataframe) {
    settings
  } else {
    print(settings, right=F)
  }
}
