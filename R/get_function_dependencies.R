#' Tries to identify package dependencies
#'
#' \code{\link{get_function_dependencies()}} helps the user to identify the
#' packages used grapho archive.
#'
#'@details Running \code{\link{get_function_dependencies()}} will read through
#'your Grapho archive to identify the packages required in each session. If you
#'do not have the packages installed then you
#'  will be prompted to do so. Finally,
#' the package for each function is identified using the
#' \code{{utils}\link{find()}} function.
#'
#'@returns Dataframe containing your package
get_function_dependencies <- function() {

  # get dataframe containing the commands in Grapho archive
  df <- parse_commands()

  # pick out individual sessions.
  # Important as we need to recreate load order to ensure we have
  # the correct path
  sessions <- unique(df$session_id)

  # create empty results dataframe
  results <- data.frame(
    session_id = character(),
    packages = character()
    )

  for (this_session in sessions) { # for each session ID

    # data for one session
    current_session <- df[df$session_id == this_session, ]

    # find where the package commands require and library are
    load_locations <- grepl(pattern = "require|library", current_session$text)

    # shift the location vector of logicals one down
    # - the location of the library name
    package_name_idx <-
       c(FALSE, load_locations[seq_len(length(load_locations) - 1)])

    if (sum(load_locations) > 0) { # if there are any packages in this session

      # create dataframe containing the session ID and packages
      out <- data.frame(
        session_id = this_session,
        packages = unique(current_session$text[package_name_idx])
      )

      # add package of this session to a package list
      results <- rbind(results, out)
    }

  }

  # get archive packages
  archive_packages <- unique(results$packages)

  # inform user of the good news
  message(paste0(
    "Your grapho archive contains references to the following packages:",
    paste(archive_packages, collapse = ",")
  ))

  # find if any packages user does not have installed
  ip <- names(installed.packages()[, 1])
  package_matches <- archive_packages %in% ip

  if (sum(!package_matches) == 0) { # if there are no missing packages
    message("You seem to have all of the required packages installed.")
  } else { # if there are missing packages

    # prompt user to see if they would like to install the missing packages
    message(paste0("The following packages must be installed to continue ",
                   paste(archive_packages[!package_matches], collapse = ",")))

    packages_install <- FALSE
    install <-
      readline(prompt = "Would you like to install these packages (Y/N): ")

    if (install == "Y") { # install packages if required
      parsed_entry <- tryCatch({
        install.packages(archive_packages[!package_matches])
        packages_installed <- TRUE

      }, warning = function(w) {
        cat("\n\n  WARNING when installing packages\n\n",
            as.character(w)
        )
      }, error = function(e) {
        cat(
          paste0("\n\n  ERROR\n\n  Unable to install packages.\n\n  ",
          "R returned the error message\n\n:
          "),
            as.character(e))
      }, finally = {
      })

    } else { # abort if user has chosen to not install packages
      stop("Required packages not installed. Aborting.")
    }
  }

  # Go through each session, then, line by line, identify the
  # package associated to each function

  # Create package column
  df$package <- NaN

  for (this_session in sessions) { # for each session

    current_session <- df[df$session_id == this_session, ]

    message(paste0("Parsing session:", this_session))

    for (i in seq_len(nrow(current_session))) { # for each row

      this_row <- current_session[i, ]

      if (this_row$text %in% archive_packages) { # if package name
        require(this_row$text, character.only = TRUE) # load package
        current_session$package[i] <- this_row$text[i]
      } else {
        package <- tryCatch({
          find(this_row$text)
        }, warning = function(w) {
          "warning"
        }, error = function(e) {
          "error"
        }, finally = {
        })

        if (length(package) == 0) {
          package <- "Not found"
        }
        current_session$package[i] <- package[1]
      }
    }

    df$package[df$session_id == this_session] <- current_session$package
  }

  df

}
