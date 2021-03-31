test_that("config file correctly created by check_for_config_file", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    # find out if the config file has been created
    config_file <- file.exists(config_file_location)

    # clean up
    file.remove(config_file_location)

    expect_true(config_file)
})

test_that("config file can be read", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    # read in the newly created config file
    config <- read_config_file()

    # clean up
    file.remove(config_file_location)

    expect_true(is.data.frame(config))
})

test_that("check_if_folder is writable works", {
    library(grapho)

    expect_true(check_if_folder_is_writable(getwd()))
})

test_that("folder information is written to config file", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    folder_location <- getwd()

    # write the folder location
    write_folder_location(folder = folder_location)

    # read in config file
    config <- read_config_file()

    # clean up
    file.remove(config_file_location)

    # check if our folder location is there
    expect_true(any(grep(folder_location, config$value)))
})

test_that("verbosity is written to config file", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    verbosity <- TRUE

    # write the verbosity value
    write_verbosity(verbosity)

    # read in config file
    config <- read_config_file()

    # clean up
    file.remove(config_file_location)

    # check if our verbosity is there
    expect_true(any(grep(verbosity, config$value)))
})

test_that("verbosity is written to config file", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    verbosity <- TRUE

    # write the verbosity value
    write_verbosity(verbosity)

    # read in config file
    config <- read_config_file()

    # clean up
    file.remove(config_file_location)

    # check if our verbosity is there
    expect_true(any(grep(verbosity, config$value)))
})

test_that("plot preference is written to config file", {
    library(grapho)

    # set OS dependent config file location
    grapho_config_dir <- getwd()
    config_file_location <- paste0(grapho_config_dir, "/config.csv")

    # set test environment variable
    Sys.setenv(GRAPHO_TEST_CONFIG_DIR = getwd())

    # check config file exists
    check_for_config_file()

    plot_file_type <- "png"

    write_plot_file_format(plot_file_format = plot_file_type)
 
    # read in config file
    config <- read_config_file()

    # clean up
    file.remove(config_file_location)

    expect_true(any(grep(plot_file_type, config$value)))
})