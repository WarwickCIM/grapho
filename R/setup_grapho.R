#' @rdname setup_grapho
#' @title Set the grapho configuration options
#' @description Provides an interface for setting up the grapho
#' configuration options. The current options are the grapho
#' arhive location and the image format used for plot files.
#' You can access the grapho configuration options by running 
#' .grapho$config at the console. The current options are saved 
#' in an RDS file at the location found at .grapho$grapho_state_file.
#' @export
#' @import rstudioapi
#' @import utils
setup_grapho <- function() {
  message('Choose grapho archive location')
  
  if(Sys.getenv("RSTUDIO") == 1) {
    grapho_archive_location <- 
      rstudioapi::selectDirectory(
        label = 'Choose this Grapho archive location',
        caption = 'Select grapho archive location'
      )
  } else {
    grapho_archive_location <-
      tcltk::tk_choose.dir(
        default = getwd(),
        caption = 'Select grapho archive location')
  }
  
  if(is.na(grapho_archive_location)) {
    stop('No grapho archive location chosen')
  }
  
  grapho_archive <- paste0(grapho_archive_location, '/grapho_archive')
  
  # create archive
  dir.create(grapho_archive)
  
  .GlobalEnv$.grapho$config$grapho_archive$current <- 
    grapho_archive
  
  # create place to store older grapho archive locations
  if('past' %in% names(.GlobalEnv$.grapho$config$grapho_archive)) {
    
  } else {
    .GlobalEnv$.grapho$config$grapho_archive$past <- c()
  }
  
  .GlobalEnv$.grapho$config$grapho_archive$past <-
    unique(
      c(.GlobalEnv$.grapho$config$grapho_archive$past,
        .GlobalEnv$.grapho$config$grapho_archive$current)
    )
    
  # grapho log location
  .GlobalEnv$.grapho$config$grapho_log_file <-
    paste0(
      .GlobalEnv$.grapho$config$grapho_archive$current,
      '/',
      create_filename("consolelog"),
      '.txt'
    )
  
  # create variable table for random log
  .GlobalEnv$.grapho_variable_table <- data.frame(
    var = character(),
    var_rand = character(),
    current = logical()
  )
  
  image_format_choice <- utils::menu(
    choices = c("png", "jpeg", "tiff"),
    graphics = FALSE,
    title = 'What image format should grapho use?'
  )
  
  image_format <- c("png", "jpeg", "tiff")[image_format_choice]
  
  grapho_recording_choice <- utils::menu(
    choices = c("YES", "NO"),
    graphics = FALSE,
    title = 'Should grapho start recording now?'
  )
  
  grapho_recording <- c(TRUE, FALSE)[grapho_recording_choice]
  
  .GlobalEnv$.grapho$config$image_format <- 
    image_format
  
  .GlobalEnv$.grapho$config$recording <- 
    grapho_recording
  
  save_grapho_state()
  
  message(
    'Run toggle_grapho() to enable or disable grapho recording'
    )
}

#' @rdname toggle_grapho
#' @title Turns grapho recording on or off
#' @import digest
#' @export
toggle_grapho <- function(){
  if(.GlobalEnv$.grapho$config$recording) {
    .GlobalEnv$.grapho$config$recording <- FALSE
    message('Grapho recording stopped')
  } else {
    .GlobalEnv$.grapho$config$recording <- TRUE
    message('Grapho recording started')
  }
}

save_grapho_state <- function(){
  # save state
  saveRDS(
    object = .GlobalEnv$.grapho,
    file = .GlobalEnv$.grapho$grapho_state_file)
}

#' @rdname print_log
#' @title Prints the content of the current log to the console
#' @export
print_log <- function() {
  if(file.exists(.GlobalEnv$.grapho$config$grapho_log_file)) {
    log_file <- file(
      .GlobalEnv$.grapho$config$grapho_log_file,
      'r')
    
    while (TRUE) {
      line <- readLines(log_file, n = 1)
      if (length(line) == 0) {
        break
      }
      print(line)
    }
    
    close(log_file)
  } else {
    message('Log file does not exist. Please run setup_grapho()')
  }
}

#' @rdname print_random_log
#' @title Prints the content of the current randomised log to the console
#' @export
print_random_log <- function() {
  if(file.exists(.GlobalEnv$.grapho$config$grapho_random_log_file)) {
    log_file <- file(
      .GlobalEnv$.grapho$config$grapho_random_log_file,
      'r')
    
    while (TRUE) {
      line <- readLines(log_file, n = 1)
      if (length(line) == 0) {
        break
      }
      print(line)
    }
    
    close(log_file)
  } else {
    message('Random log file does not exist. Please run setup_grapho()')
  }
}