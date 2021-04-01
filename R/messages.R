# Functions which return formatted messages

#' @rdname introduce_grapho
#' @title Introduce Grapho
#' @description Prints out the first part of the Grapho welcome message. Runs
#' when Grapho is loaded.
#' @export
introduce_grapho <- function() {

  # check if we are in a test environment
  test <- Sys.getenv("GRAPHO_TEST_ENVIRONMENT")

  if (is.null(test)) {
    message("
    Welcome to grapho.\n
    This package creates an archive of your R activity.
    We developed the package at the University of Warwick.

    For information about the project contact:
      Dr Greg McInerny (g.mcinerny@warwick.ac.uk)
      Dr James Tripp (james.tripp@warwick.ac.uk)")
  }
}

#' @rdname print_welcome_message
#' @title Welcome use to the grapho package
#' @description Displays a welcome message introducing the user to grapho
#' @param returning If TRUE message says welcome back
#' @importFrom utils packageVersion
#' @export
print_welcome_message <- function(returning = TRUE) {
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

  if (returning) {
    message("\nGood ", this_period, ". Welcome back to Grapho.\n")
  } else {
    message("\nGood ",this_period,
            ". Welcome to Grapho ", utils::packageVersion("grapho"),
            ".\n\nGrapho is research package written by\nJames Tripp and Gregory McInerny at the\nUniversity of Warwick.\n")
  }

}
