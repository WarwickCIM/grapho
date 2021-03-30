test_that(" we can correctly extract commands from grapho archive", {
    library(grapho)

    past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    new_grapho_folder <- getwd()

    # Set environment variable
    Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)

    # load in test data
    load("test.RData")
    load("parsed_commands.RData")

    # parse the archive
    sample_parsed_commands <-
     parse_commands(parsed_folder)

    # cleanup
    Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)

    # check expected and prerecorded output is the same
    expect_identical(
        sample_parsed_commands,
        parsed_commands)
})