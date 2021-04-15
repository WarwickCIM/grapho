test_that(" sample grapho file is parsed correctly", {
    library(grapho)

    past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    new_grapho_folder <- getwd()

    # Set environment variable
    Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)

    # load in test data
    parsed_folder <- read.csv("sample_parsed_folder.csv",
     fileEncoding = "UTF-8")

    # correct columns to match expected format
    parsed_folder$session_id <-
     as.character(parsed_folder$session_id)

    parsed_folder$session_start_time <-
     as.POSIXct(parsed_folder$session_start_time)
    parsed_folder$time <-
     as.POSIXct(parsed_folder$time)

    attr(parsed_folder$session_start_time, "tzone") <- "Europe/London"
    attr(parsed_folder$time, "tzone") <- "Europe/London"

    row.names(parsed_folder) <- NULL

    # parse the archive
    sample_parsed_folder <- parse_grapho_archive()

    row.names(sample_parsed_folder) <- NULL

    # cleanup
    Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)

    # check expected and prerecorded output is the same
    expect_identical(
        sample_parsed_folder,
        parsed_folder)
})