# # create file name
# test_that("create filename returns a filename", {
#     library(grapho)
#     filename <- create_filename("test")
#     expect_true(grepl("test", filename))
# })
#
# # check setup_grapho_folder can create a folder archive
# test_that("grapho folder created", {
#     library(grapho)
#
#     temp_dir <- tempdir()
#
#     setup_grapho_folder(
#         grapho_folder_location = temp_dir
#         )
#
#     folder_location <- Sys.getenv("GRAPHO_FOLDER")
#
#     expect_true(file.exists(folder_location))
# })
#
# # test error scribe can be set
# test_that("error scribe can be registered as a callback", {
#     # callback if registered as part of grapho
#     library(grapho)#
#
#     # add error scribe to options for error
#     start_error_scribe()
#
#     # check callback is registered
#     scribe_present <- !is.null(options()$error)
#
#     expect_true(scribe_present)
# })
#
# # test expression scribe can be set
# test_that("expression scribe can be registered in options", {
#     # callback if registered as part of grapho
#     library(grapho)
#
#     # clear callback
#     while (is.element("expression_scribe", getTaskCallbackNames()))
#         removeTaskCallback("expression_scribe")
#
#     # add error scribe callback
#     start_expression_scribe()
#
#     # check callback is registered
#     scribe_present <-
#          any("expression_scribe" %in% getTaskCallbackNames())
#
#     # clear callback
#     while (is.element("expression_scribe", getTaskCallbackNames()))
#         removeTaskCallback("expression_scribe")
#
#     expect_true(scribe_present)
# })
