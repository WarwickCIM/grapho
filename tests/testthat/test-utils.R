## TODO Write test for callback functionality


## Plot figures

# svg plots are correctly written
# jpg plot are successfully written
# png plots are successfully written

## Text files

# grapho log created
test_that('Grapho log can be created', {
  log_file_location <- create_log_file(
    return_location = TRUE,
    show_messages = FALSE)
  expect_true(file.exists(log_file_location))
})

# console commands are correctly recorded to the log
test_that('expressions are parsed and recorded correctly', {
  library(grapho)

  # Save old log file location
  past_grapho_log_location <- Sys.getenv('GRAPHO_LOG_FILE')

  # Set new temporary log file location
  log_file_location <- tempfile()
  Sys.setenv(GRAPHO_LOG_FILE = log_file_location)

  # Create log file
  create_log_file()

  # Add entry to log file
  expression_scribe(top_level_expr = expression('g <- 22'))

  # Set log file location to old location
  Sys.setenv(GRAPHO_LOG_FILE = past_grapho_log_location)

  # read in file and cleanup
  log_data <- paste(readLines(log_file_location),
                        collapse='\n')

  # Remove temporary log
  file.remove(log_file_location)

  # check command was recorded
  expect_true(grepl("g <- 22", log_data))
})

# version information is saved correctly
test_that('r version information is recorded and can be read back correctly', {
  library(grapho)

  # past verbosity settings
  past_verbosity <- Sys.getenv("GRAPHO_VERBOSE")

  # disable messages
  Sys.setenv(GRAPHO_VERBOSE = "FALSE")

  # Save old log file location
  past_grapho_folder <- Sys.getenv('GRAPHO_FOLDER')

  # Set new temporary grapho folder
  new_grapho_folder <- tempdir()

  # Set environment variable
  Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)

  # Save session information to file
  session_information_file_location <-
    log_session_information(return_location = TRUE)

  # Revert grapho folder to old location
  Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)

  # Revert verbosity setting
  Sys.setenv(GRAPHO_VERBOSE = past_verbosity)

  # read in file and cleanup
  log_data <- read.csv(
    file = session_information_file_location,
    fileEncoding = 'UTF-8')

  # Remove temporary log
  file.remove(session_information_file_location)

  # get session information
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
      var_name = c("LANG", "PATH", "R_HOME", "R_RD4PDF", 'RSTUDIO'),
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

  # check if information saved correctly
  expect_equal(log_data, vars)
})

## Parsing and processing grapho archive

# grapho logs are read recorded correctly
# grapho logs are correctly parsed
# commands are parsed as expected
# function dependencies are correctly identified

# test_that("svg plots are saved accurately", {
#
#   # extract svg representation of plot
#   library(svglite)
#   my_plot <- svgstring()
#   plot(rnorm(10))
#   dev.off()
#
#   # create plot and save using
#   temp_location <- tempdir()
#   plot_location <- write_plot(
#     folder = temp_location,
#     format = 'svg',
#     return_location = TRUE)
#
#
#   svglite::
#
#   expect_equal(str_length("a"), 1)
#   expect_equal(str_length("ab"), 2)
#   expect_equal(str_length("abc"), 3)
# })
