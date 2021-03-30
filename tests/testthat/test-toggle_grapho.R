test_that(" grapho can be toggled on and off", {
    library(grapho)

    # existing settings
    past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    past_log_file_location <-
        Sys.getenv("GRAPHO_LOG_FILE")

    # write new settings
    new_grapho_folder <- getwd()
    Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)
    Sys.setenv(
        GRAPHO_LOG_FILE = paste0(new_grapho_folder, "/",
                             create_filename("consolelog"),
                             ".txt")
    )

    log_file_location <- Sys.getenv("GRAPHO_LOG_FILE")

    # toggle grapho does two things
    #   Adds text to the current archive
    #   Disables the recording of commands
    #       to the log file
    toggle_grapho(show_messages = FALSE)

    # try to write to the log
    expression_scribe(
        top_level_expr = expression("g <- 22")
    )

    # check grapho logging off message created
    log_data <- paste(readLines(log_file_location, warn = FALSE),
                        collapse = "\n")

    # make sure grapho off message is present
    grapho_message_disabled <-
     grepl("GRAPHO LOGGING DISABLED", log_data)

    # find out if expression has been written
    writing_disabled <- !grepl("g <- 22", log_data)

    # re-enable grapho logging
    toggle_grapho(show_messages = FALSE)

    # check files are written
    expression_scribe(
        top_level_expr = expression("t <- 41")
    )

    log_data <- paste(readLines(log_file_location, warn = FALSE),
                        collapse = "\n")

    grapho_message_enabled <-
     grepl("GRAPHO LOGGING ENABLED", log_data)

    writing_enabled <-
     grepl("t <- 41", log_data)

    # reset environment variables
    Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)
    Sys.setenv(GRAPHO_LOG_FILE = past_log_file_location)

    # remove created log file
    file.remove(log_file_location)

    test_passed <- all(grapho_message_disabled,
                        writing_disabled,
                        grapho_message_enabled,
                        writing_enabled)

    expect_true(test_passed)
})