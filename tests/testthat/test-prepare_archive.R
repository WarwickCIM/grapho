test_that(" grapho archive can be zipped", {
    # grab old settings
    old_home <- Sys.getenv("HOME")
    old_grapho_folder <- Sys.getenv("GRAPHO_FOLDER")

    # put everything in tests directory
    Sys.setenv(HOME = getwd())
    Sys.setenv(GRAPHO_FOLDER = getwd())

    # write out zip file
    prepare_archive()

    # return to old settings
    Sys.setenv(HOME = old_home)
    Sys.setenv(GRAPHO_FOLDER = old_grapho_folder)

    # check if zip file in folder
    zip_file <- any(grepl(".zip", dir()))

    # remove created zip file
    file.remove(dir()[grepl(".zip", dir())])

    expect_true(zip_file)
})