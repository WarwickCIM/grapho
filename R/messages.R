# Functions which return formatted messages

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

#' @rdname print_welcome_message
#' @title Welcome use to the grapho package
#' @description Displays a welcome message introducing the user to grapho
#' @export
print_welcome_message <- function() {
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

  message(
    paste0("Good ", this_period, ". Welcome to the Grapho package.")
  )

}