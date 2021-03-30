test_that(" sample grapho file is parsed correctly", {
    library(grapho)

    past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    new_grapho_folder <- getwd()

    # Set environment variable
    Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)

    # load in test data
    load("test.RData")

    # parse the archive
    sample_parsed_folder <- parse_grapho_archive()

    # cleanup
    Sys.setenv(GRAPHO_FOLDER = past_grapho_folder)

    # check expected and prerecorded output is the same
    expect_identical(
        sample_parsed_folder,
        parsed_folder)
})