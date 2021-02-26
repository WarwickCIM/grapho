#' Parses Grapho archive
#'
#' \code{\link{Parse_grapho_archive()}} reads the files
#'  in your Grapho archive and returns a
#' structured overview of your activities.
#'
#'@return Dataframe containing the time, type and record for commands, errors,
#'warnings and plots.
#'
#'@details The \code{\link{parse_grapho_archive()}} function
#'  retrieves the locations of your
#'Grapho archive. For each logfile the function parses the logfile contents
#'retrieving the session ID, user ID, time, type and details of each event.
#'For each plot the function compares hashes of each plot file to remove
#'duplicates. The resulting dataframe offers a detailed overview
#'  of ones session.
parse_grapho_archive <- function() {

  grapho_folder_location <- Sys.getenv("GRAPHO_FOLDER")
  grapho_files <- dir(grapho_folder_location)

  console_logs <- grep(pattern = "consolelog", grapho_files, value = TRUE)

  message("Processing console logs")

  # entry counter
  entry_idx <- 0

  # process the console logfiles
  for (current_logfile in console_logs) {

    logfile_parts <- strsplit(current_logfile, split = "-")[[1]]

    # select the different parts of the console log file name
    session_start_time <-
      as.POSIXct(logfile_parts[1], "%y%d%mT%H%M%S", tz = Sys.timezone())
    user_id <- logfile_parts[3]
    session_id <- gsub(pattern = ".txt", replacement = "", x = logfile_parts[4])

    # load in file. Character vector one line per entry
    con <- file(paste0(grapho_folder_location, "/", current_logfile))
    linn <- readLines(con, warn = FALSE)
    close(con)

    # find out where the entries are
    entries <- grep(pattern = "^ ##------", linn, perl = TRUE)

    # for each console log entry
    for (i in seq_len(length(entries))) {

      # Select start and end points of record entry
      entry_start <- entries[i]

      if (i == length(entries)) {
        entry_end <- length(linn) + 1
      } else {
        entry_end <- entries[i + 1]
      }

      datestamp <- linn[entry_start]

      # Clean datestamp
      datestamp <- gsub(
        pattern = " ##------ ",
        replacement = "",
        x = gsub(
          pattern = " ------##",
          replacement = "",
          x = datestamp
        )
      )

      # If file uploaded then timezone may need to be changed
      time <- as.POSIXct(datestamp,
                "%a %b  %d %H:%M:%S %Y",
                tz = Sys.timezone())

      type <- linn[entry_start + 1]

      entry <- paste(linn[(entry_start + 2) : (entry_end - 1)],
                     collapse = "\n")

      # if first entry then create dataframe
      if (entry_idx == 0) {
        grapho_archive <- data.frame(
          session_id,
          session_start_time,
          time,
          type,
          entry,
          stringsAsFactors = FALSE
        )

        # iterate entry counter
        entry_idx <- entry_idx + 1

      } else {
        grapho_archive <- rbind(
          grapho_archive,
          data.frame(
            session_id,
            session_start_time,
            time,
            type,
            entry,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  message("Processing images")

  # Images are created every time a command is run (if available)
  # We need to record only the time the image is first recorded
  # To do this we cycle through the image files

  # process the image files
  get_file_ext <- function(x) {
    substr(x, start = nchar(x) - 3, stop = nchar(x))
    }

  file_extensions <- apply(X = data.frame(grapho_files),
                          MARGIN = 1, FUN = get_file_ext)

  image_file_idx <- grep(perl = TRUE,
                         pattern = "png|jpg|svg",
                         x = file_extensions)

  image_filenames <- grapho_files[image_file_idx]

  if (length(image_filenames) > 0) { # If there are any image files
    # Takes 1 filename and return a dataframe containing
    # time, session_id, user_id, etc.
    parse_filename <- function(x) {
      split_filename <- strsplit(x = x, split = "-")[[1]]
      data.frame(
        session_id = tools::file_path_sans_ext(split_filename[4]),
        session_start_time = NA,
        time = as.POSIXct(split_filename[1],
                          "%y%d%mT%H%M%S",
                          tz = Sys.timezone()),
        type = "PLOT",
        entry = x,
        md5 = as.character(
          tools::md5sum(
            paste0(grapho_folder_location, "/", x)
            )),
        stringsAsFactors = FALSE
      )
    }

    images_df <- do.call(rbind,
                         apply(as.data.frame(image_filenames),
                         MARGIN = 1,
                         FUN = parse_filename))

    # Go through each session and only pick out images when made
    image_idx <- 0

    for (images_session in unique(images_df$session_id)) {
      this_session <- images_df[images_df$session_id == images_session, ]
      for (this_md5 in unique(this_session$md5)) {
        this_image <- this_session[this_session$md5 == this_md5, ]
        earlier_image_record <-
           this_image[this_image$time == min(this_image$time), ]
        if (image_idx == 0) { # if first parsed image
          image_record <- earlier_image_record
          image_idx <- image_idx + 1
        } else {
          image_record <- rbind(image_record, earlier_image_record)
        }
      }
    }

    # Add parsed images to the grapho archive
    grapho_archive <- rbind(grapho_archive,
                            image_record[c("session_id", "session_start_time",
                                           "time", "type", "entry")])
  }

  grapho_archive
}
