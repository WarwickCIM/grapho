#' Prepares Grapho Archive
#'
#' Creates a zip archive of
#'  all the files in the Grapho
#' folder and prints out a message informing the user where to submit the
#' zip archive.
#'
#' Please do submit your Grapho data for the WAYS project at
#' the University of Warwick - we are looking at the workflow of R users and
#' would greatly appreciate your data submission.
prepare_archive <- function() {
  result <- tryCatch({
    cat("Creating zip file from your grapho archive\n\n")

    grapho_archive_location <- Sys.getenv("GRAPHO_FOLDER")
    home_folder <- Sys.getenv("HOME")
    grapho_files <- dir(grapho_archive_location, full.names = TRUE)
    zip_file_location <-  paste0(home_folder, "/",
                                 format(Sys.time(),
                                 "%y%d%mT%H%M%S"),
                                 "_grapho.zip")

    utils::zip(zipfile = zip_file_location, grapho_files, flags = "-qj")

    form_location <- "Form not created yet"

    cat("Zip file created at \n\n", zip_file_location,
        "\n\nPlease submit this file using the form at:\n\n",
        form_location)
  }, warning = function(w) {
    cat("\n\n  Problem when writing zip file. See R output \n\n",
        w
    )
  }, error = function(e) {
    cat(
      paste0("\n\n  ERROR\n\n We were unable to create a zip file containing",
      " \n\n grapho files\n\n:
          "),
        e)
  }, finally = {
  })
}
