.onLoad <- function(libname, pkgname) {
  assign("grapho",
         new.env(),
         envir = parent.env(environment()))

  introduce_grapho()
  setup_grapho_folder()
  log_session_information()
  start_expression_scribe()
  start_error_scribe()
  create_log_file()
}