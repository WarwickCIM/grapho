# test_that("error_scribe parsed and recorded correctly", {
#   library(grapho)
#
#   # Save old log file location
#   past_grapho_log_location <- Sys.getenv("GRAPHO_LOG_FILE")
#
#   # Set new temporary log file location
#   log_file_location <- tempfile()
#   Sys.setenv(GRAPHO_LOG_FILE = log_file_location)
#
#   # Create log file
#   create_log_file(show_messages = FALSE)
#
#   # Add entry to log file
#   error_scribe(
#       error = "This is a test error."
#     )
#
#   # Set log file location to old location
#   Sys.setenv(GRAPHO_LOG_FILE = past_grapho_log_location)
#
#   # read in file and cleanup
#   log_data <- paste(readLines(log_file_location, warn = FALSE),
#                         collapse = "\n")
#
#   # Remove temporary log
#   file.remove(log_file_location)
#
#   # check command was recorded
#   expect_true(grepl("This is a test error", log_data))
# })
