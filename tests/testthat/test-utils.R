# Possible tests

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

# console commands are recorded to the log
# version information is saved correctly

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
