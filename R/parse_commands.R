#' Parses commands from Grapho archive
#'
#' Goes through your Grapho archive and
#' returns a summary of the R commands you have run.
#' @return Dataframe containing the token type, text, session ID, session time
#' and time for elements of commands.
#'
#'@details Creates a Dataframe of your
#'Grapho archive using the \code{\link{parse_grapho_archive}()} command.
#'Commands are extracted from this Dataframe and then each command in turn is
#'passed to the \code{\link[utils]{getParseData}()} function which breaks down
#'the command into tokens such as SYMBOL_FUNCTION_ALL, SYMBOL, LEFT_ASSIGN, etc.
#'Tokens of type SYMBOL_FUNCTION_ALL, SYMBOL, LEFT_ASSIGN are extracted from
#'the output and labeled with the corresponding session id, sessions start time
#'and time.

parse_commands <- function() {
  df <- parse_grapho_archive()

  # do not include errors or warnings
  commands <- df[
    df$type == " COMMAND " &
    df$type != " ERROR COMMAND ",
    ]

  # Go through each entry to keep the time
  first_run <- TRUE

  for (i in seq_len(nrow(commands))) {
    this_entry <- commands[i, ]

    # Try to parse the entry
    parsed_entry <- tryCatch({
      utils::getParseData(
        parse(
          text = str2expression(this_entry$entry)
          )
      )
    }, warning = function(w) {
      cat("\n\n  WARNING when parsing comamnd file\n\n",
          this_entry$entry,
          as.character(w)
      )
    }, error = function(e) {
      cat(
        paste0("\n\n  ERROR\n\n  We were unable to parse the command.\n\n ",
        " R returned the error message\n\n:
          "),
          this_entry$entry,
          as.character(e))
    }, finally = {
    })

    this_result <- parsed_entry[
      ((parsed_entry$token == "SYMBOL_FUNCTION_CALL") |
        (parsed_entry$token == "SYMBOL") |
        (parsed_entry$token == "LEFT_ASSIGN")),
      c("token", "text")
    ]

    this_result$order <- seq_len(nrow(this_result))
    this_result$session_id <- this_entry$session_id
    this_result$session_start_time <- this_entry$session_start_time
    this_result$time <- this_entry$time

    if (first_run) {
      result <- this_result
      first_run <- FALSE
    } else {
      result <- rbind(
        result,
        this_result
      )
    }
  }

  result
}
