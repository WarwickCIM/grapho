test_that(" sample grapho file is parsed correctly", {
    library(grapho)

    past_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    # Set new temporary grapho folder
    new_grapho_folder <- paste0(
        getwd(),
        "/tests/",
        "test_grapho_folder"
        )

    # Set environment variable
    Sys.setenv(GRAPHO_FOLDER = new_grapho_folder)

    # load in test data
    load(paste0(new_grapho_folder, "/test.RData"))

    # parse the archive
    parsed_folder <- parse_grapho_archive()

    # check expected and prerecorded output is the same
    expect_identical(
        test_parsed_archive,
        parsed_folder)
})
