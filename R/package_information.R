#' @rdname grapho_info
#' @title Prints information about the grapho package
#' @description Grapho is part of the What Aren't You
#'  Seeing (WAYS) project. The function print out
#'  information about the aims of the package and
#'  the authors.
#' @export
grapho_info <- function() {
  message("The grapho is a package developed by Dr Greg McInerny and
 Dr James Tripp at the University of Warwick.

The aim of the package is to understand how people visualise in R.
Our work is part of the What Aren't You Seeing (WAYS) project and is
funded by the Alan Turing Institute.

If you have any questions then please contact either:
          Dr James Tripp (James.Tripp@warwick.ac.uk)
          Dr Greg McInerny (G.McInerny@warwick.ac.uk)

For information about the project and those involved please go to:
          https://www.turing.ac.uk/research/research-projects/ways-what-arent-you-seeing
          https://warwick.ac.uk/jamestripp
          https://warwick.ac.uk/cim/people/greg-mcinerny/")

}

#' @rdname how_does_grapho_work
#' @title An explanation of the inner workings of grapho
#' @description Some may be interested in how grapho works. Understanding
#'  what grapho is doing is particularly important because grapho is
#'  storing your code activity in your grapho archive folder.
#' @export
how_does_grapho_work <- function() {
  info <- paste0("Grapho saves the code that you run and plots
which you create in folder. This folder is called your 'grapho archive'
You can find your grapho archive at the location below.\n\n",
                 .GlobalEnv$.grapho$grapho_state_file)

  message(info)
}