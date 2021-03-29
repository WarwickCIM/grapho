# create file name
test_that("create filename returns a filename", {
  filename <- create_filename("test")
  expect_true(grepl("test", filename))
})

# test error scribe can be set
test_that("error scribe can be registered as a callback", {
    # callback if registered as part of grapho
    library(grapho)

    # clear callback
    while (is.element("error_scribe", getTaskCallbackNames()))
        removeTaskCallback("error_scribe")

    # add error scribe callback
    start_error_scribe()

    # check callback is registered
    scribe_present <-
         any("expression_scribe" %in% getTaskCallbackNames())

    # clear callback
    while (is.element("error_scribe", getTaskCallbackNames()))
        removeTaskCallback("error_scribe")

    expect_true(scribe_present)
})