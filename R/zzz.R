.onLoad <- function(libname, pkgname) {
  assign("grapho",
         new.env(),
         envir = parent.env(environment()))

  introduce_grapho()
  setup_grapho_folder()
  log_session_information()
  start_expression_scribe()
  start_error_scribe()
  create_log_file()
}

#' @rdname introduce_grapho
#' @title Introduce Grapho
#' @description Prints out the first part of the Grapho welcome message. Runs
#' when Grapho is loaded.
#' @export
introduce_grapho <- function() {
  message("
  Welcome to grapho.\n
  This package creates an archive of your R activity.
  We developed the package at the University of Warwick.

  For information about the project contact:
      Dr Greg McInerny (g.mcinerny@warwick.ac.uk)
      Dr James Tripp (james.tripp@warwick.ac.uk)")
}

#' @rdname setup_grapho_folder
#' @title Setup Grapho folder
#' @description Creates the Grapho folder, user and session IDs. Records the
#' folder location and ID information as global variables. Runs
#' when Grapho is loaded.
#' @export
#' @import digest
setup_grapho_folder <- function() {
  home_folder <- Sys.getenv("HOME")
  grapho_folder_location <- paste0(home_folder, "/grapho_archive")

  ### Set environment variables
  ### GRAPHO_FOLDER, GRAPHO_VERBOSE,
  ### GRAPHO_USER_ID, GRAPHO_SESSION_ID,
  ### GRAPHO_HISTORY_FILE, GRAPHO_ENVIRONMENT
  ### GRAPHO_PLOT_FILE_FORMAT, GRAPHO_LOGGING
  Sys.setenv(GRAPHO_FOLDER = grapho_folder_location)
  Sys.setenv(GRAPHO_PLOT_FILE_FORMAT = "png")

  # Indicate if we aire in RStudio
  if (Sys.getenv("RSTUDIO") == 1) {
    Sys.setenv(GRAPHO_ENVIRONMENT = "RStudio")
  } else {
    Sys.setenv(GRAPHO_ENVIRONMENT = "R")
  }

  # Set grapho logging flag to on
  Sys.setenv(GRAPHO_LOGGING = TRUE)

  # Verbose messaging
  Sys.setenv(GRAPHO_VERBOSE = TRUE)
  # Unique user ID hash from home, language and platform
  # SHA1 is broken but good enough for our purposes
  user_hash <- digest(
    c(Sys.getenv("HOME"),
      Sys.getenv("LANG"),
      Sys.getenv("R_PLATFORM")),
    algo = "sha512"
  )

  Sys.setenv(
    GRAPHO_USER_ID = substr(user_hash, 1, 40)
      )

  message(
    paste0("\n\n  We will add your user id (shown below) to\n",
    "  files in your data archive.\n\n   "),
   substr(user_hash, 1, 40))

  session_hash <- digest::digest(
    date(), algo = "sha512"
  )

  Sys.setenv(
    GRAPHO_SESSION_ID = substr(session_hash, 1, 40)
  )

  # History file location
  Sys.setenv(
    GRAPHO_LOG_FILE = paste0(grapho_folder_location, "/",
                             create_filename("consolelog"),
                             ".txt")
  )

  # Check if grapho folder exists
  if (file.exists(grapho_folder_location)) {
    message(
      paste0("\n\n  You appear to have a grapho folder.\n\n",
      "  We will place new grapho data in\n   "),
    grapho_folder_location,
    "\n\n  There are currently",
    length(dir(grapho_folder_location)),
    "files there.\n")
  }

  # Attempt to create grapho folder if
  # it does not exist.
  if (!file.exists(grapho_folder_location)) {
    # Attempt to create grapho folder
    result <- tryCatch({
      dir.create(grapho_folder_location)
    }, warning = function(w) {
      message("\n\n  WARNING when trying to create grapho folder\n\n",
          w,
          "\n\n  We were trying to create the folder\n",
          grapho_folder_location
          )
    }, error = function(e) {
      message(
        paste0("\n\n  ERROR\n\n  ",
        "We were unable to create the grapho folder.\n\n ",
        " R returned the error message\n\n:")
          ,
          e,
          "\n\nYou may want to try reloading grapho or creating the directory\n
          ",
          grapho_folder_location
      )
    }, finally = {
    message("\n\n  Your date archive can be found at \n\n    ",
        grapho_folder_location,
        "\n\n  and will record your R history and visualisations.\n\n"
        )
    })
  }
}

#' @rdname start_expression_scribe
#' @title Starts expression scribe
#' @description Run when Grapho is loaded and
#' adds the expression scribe as a task callback. Expression scribe will
#' run when a command is sent to the R console.
start_expression_scribe <- function() {

  # remove any existing callbacks
  while (is.element("expression_scribe", getTaskCallbackNames()))
    removeTaskCallback("expression_scribe")

  # Attempt to start expression scribe
  result <- tryCatch({
    addTaskCallback(expression_scribe, name = "expression_scribe")
  }, warning = function(w) {
    message("
      Warning when trying to start the expression scribe
          ",
        w
        )
  }, error = function(e) {
    message("
      ERROR
      We were unable to start the expression scribe
      R returned the error message:
          ",
        e
        )
  }, finally = {
    if (Sys.getenv("GRAPHO_VERBOSE")) {
      message("Expression scribe started")
      }
    })
}

#' @rdname start_error_scribe
#' @title Starts error scribe
#' @description Run
#'  when Grapho is loaded and
#' changes the error options to run the error scribe when an error occurs.
#' @export
start_error_scribe <- function() {

  # remove any existing callbacks
  while (is.element("error_scribe", getTaskCallbackNames()))
    removeTaskCallback("error_scribe")

  # Attempt to start expression scribe
  result <- tryCatch({
    options(
      error = function(...) {
        error_scribe()
      }
    )
  }, warning = function(w) {
    message("
      Warning when trying to start the error scribe
          ",
        w
    )
  }, error = function(e) {
    message("
      ERROR
      We were unable to start the error scribe
      R returned the error message:
          ",
        e
    )
  }, finally = {
  })
}
