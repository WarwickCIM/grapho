#' read_archives
#'
#' Reads the files
#'  in your Grapho archive and returns a
#' structured overview of your activities.
#'
#'@return Dataframe containing the time, type and record for commands, errors,
#'warnings and plots.
#'
#'@details Go to each grapho archive location in
#' .grapho$config$grapho_archive$past and .grapho$config$grapho_archive$current 
#' load each of the log files and parse out the time, type, session and user 
#' from each. The function returns this data in a dataframe.
#' @export
read_archives <- function() {
  archives <- unique(
    .GlobalEnv$.grapho$config$grapho_archive$current,
    .GlobalEnv$.grapho$config$grapho_archive$past
  )
  
  result <- data.frame(
    session_id = character(),
    session_start_time = character(),
    time = numeric(),
    type = character(),
    entry = character(),
    stringsAsFactors = FALSE
  )
  
  for (archive in archives) {
    if(file.exists(archive)) {
      
      files <- dir(archive)
      logs <- grep('consolelog', files, value = TRUE)
      
      for (log in logs) {
        
        filename_parts <- strsplit(log, split = "-")[[1]]
        session_start_time <-
          as.POSIXct(filename_parts[1], "%y%d%mT%H%M%S", tz = Sys.timezone())
        user_id <-
          filename_parts[3]
        session_id <-
          gsub(pattern = ".txt", replacement = "", x = filename_parts[4])
        
        con <- file(paste0(
          archive, "/", log
        ))
        logfile <- readLines(con, warn = FALSE)
        close(con)
        
        entry_start_idx <- grep(pattern = "^##------",
                                logfile,
                        perl = TRUE)

        if (length(entry_start_idx) == 1) {
          entry_end_idx <- length(logfile)
        } else {
          entry_end_idx <- c(
            entry_start_idx[2:length(entry_start_idx)] - 1,
            length(logfile)) 
        }

        for (entry_idx in seq_len(length(entry_start_idx))) {
          entry <- logfile[entry_start_idx[entry_idx]:entry_end_idx[entry_idx]]
          
          datestamp <- gsub(useBytes = TRUE,
                            pattern = "##------ ",
                            replacement = "",
                            x = gsub(
                              pattern = " ------##",
                              replacement = "",
                              x = entry[1]
                            )
          )
          
          entry_time <- as.POSIXct(datestamp,
                                   "%a %b  %d %H:%M:%S %Y",
                                   tz = Sys.timezone())
          
          entry_type <- entry[2]
          entry_message <- entry[3:length(entry)]
          
          result <- rbind(
            result,
            data.frame(
              session_id = session_id,
              session_start_time = session_start_time,
              time = entry_time,
              type = entry_type,
              # paste0 prevents command spanning multiple rows
              entry = paste0(entry_message, collapse = "\n")
            ))

        }
      }
    }
  }
  
  result
}