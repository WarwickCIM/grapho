.onLoad <- function(libname, pkgname) {
  assign("grapho",
         new.env(),
         envir = parent.env(environment()))

  # check if there is a previous config file
  config_check <- check_config_file_exists()

  # if there is a config file
  if (config_check$present) {
    print_welcome_message(returning = TRUE)
    message("Found config file")
  }

  # if there is no config file
  if (!config_check$present) {
    print_welcome_message(returning = FALSE)
    message("No config file!")
  }

  # if there is a config file
  #     Tell user
  #     Welcome them back
  #     Tell them how to check and change their config
  #     Confirm log file created
  #     Confirm expression scribe started
  #     Confirm error scribe started
  # if there is no config file
  #     Tell user
  #     Let them know a bit about the package
  #

  #introduce_grapho()
  setup_grapho_folder()
  log_session_information()
  start_expression_scribe()
  start_error_scribe()
  create_log_file()
}
