# # write_plot
# test_that("plots can be recorded and read in png", {
#   # libraries
#   library(grapho)
#   library(png)
#
#   # create a plot
#   plot(c(1, 2, 3))
#
#   # get old plot file format
#   old_plot_format <- Sys.getenv("GRAPHO_PLOT_FILE_FORMAT")
#
#   # set plot format to png
#   Sys.setenv(GRAPHO_PLOT_FILE_FORMAT = "png")
#
#   # write out plot to temporary directory
#   tmp_dir <- tempdir()
#
#   plot_location <-
#     write_plot(
#     folder = tmp_dir,
#     return_location = TRUE)
#
#   # try to read in the file using the png package
#   # returns TRUE if plot is read in
#   # returns FALSE on any error
#   png_loaded_into_r <-
#     tryCatch(
#       error = function(cnd) FALSE,
#       is.numeric(png::readPNG(plot_location))
#     )
#
#   # cleanup
#   file.remove(plot_location)
#
#   expect_true(png_loaded_into_r)
# })
#
# test_that("plots can be recorded and read in jpeg", {
#   # libraries
#   library(grapho)
#   library(jpeg)
#
#   # create a plot
#   plot(c(1, 2, 3))
#
#   # get old plot file format
#   old_plot_format <- Sys.getenv("GRAPHO_PLOT_FILE_FORMAT")
#
#   # set plot format to svg
#   Sys.setenv(GRAPHO_PLOT_FILE_FORMAT = "jpeg")
#
#   # write out plot to temporary directory
#   tmp_dir <- tempdir()
#
#   plot_location <-
#     write_plot(
#       folder = tmp_dir,
#       return_location = TRUE)
#
#   # try to read in the file using the jpeg package
#   # returns TRUE if plot is read in
#   # returns FALSE on any error
#   jpeg_loaded_into_r <-
#     tryCatch(
#       error = function(cnd) FALSE,
#       is.numeric(jpeg::readJPEG(plot_location))
#     )
#
#   # cleanup
#   file.remove(plot_location)
#
#   expect_true(jpeg_loaded_into_r)
# })
#
# # log session information
# test_that("r version information is recorded and can be read back correctly", {
#   library(grapho)
#
#   # past verbosity settings
#   past_verbosity <- Sys.getenv("GRAPHO_VERBOSE")
#
#   # disable messages
#   Sys.setenv(GRAPHO_VERBOSE = "FALSE")
#
#   # Save old log file location
#   past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")
#
#   # Set new temporary grapho folder
#   new_grapho_folder <- tempdir()
#
#   # Set environment variable
#   Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)
#
#   # Save session information to file
#   file_location <-
#     log_session_information(return_location = TRUE)
#
#   # Revert grapho folder to old location
#   Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)
#
#   # Revert verbosity setting
#   Sys.setenv(GRAPHO_VERBOSE = past_verbosity)
#
#   # read in file and cleanup
#   log_data <- read.csv(
#     file = file_location,
#     fileEncoding = "UTF-8")
#
#   # Remove temporary log
#   file.remove(file_location)
#
#   # get session information
#   version_df <-
#     as.data.frame(unlist(R.version))
#
#   version_vars <- data.frame(
#     stringsAsFactors = FALSE,
#     var_name = row.names(version_df),
#     value = version_df$`unlist(R.version)`
#
#   )
#
#   env_vars <-
#     data.frame(
#       stringsAsFactors = FALSE,
#       var_name = c("LANG", "PATH", "R_HOME", "R_RD4PDF", "RSTUDIO"),
#       value = c(Sys.getenv("LANG"),
#                 Sys.getenv("PATH"),
#                 Sys.getenv("R_HOME"),
#                 Sys.getenv("R_RD4PDF"),
#                 Sys.getenv("RSTUDIO"))
#     )
#
#   vars <- rbind(
#     version_vars,
#     env_vars
#   )
#
#   # check if information saved correctly
#   expect_equal(log_data, vars)
# })
#
# # create_log_file
# test_that("Grapho log can be created", {
#   log_file_location <- create_log_file(
#     return_location = TRUE,
#     show_messages = FALSE)
#   expect_true(file.exists(log_file_location))
# })
