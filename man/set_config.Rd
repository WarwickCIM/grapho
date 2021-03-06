% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_system.R
\name{set_config}
\alias{set_config}
\title{Updates folder location, plot file formats or verbosity of grapho}
\usage{
set_config(folder = NULL, file_format = NULL, verbose = NULL)
}
\arguments{
\item{folder}{Folder to save grapho records to}

\item{file_format}{Preferred file format for graphics files. JPG, PNG,
SVG supported}

\item{verbose}{If TRUE then additional grapho messages are displayed
in the terminal}
}
\description{
Configuration options for grapho are set in the config
file and grapho global environment variables GRAPHO_FOLDER,
 GRAPHO_PLOT_FILE_FORMAT and GRAPHO_VERBOSE. The function
 \code{\link[base]{Sys.getenv}()} will print out all of
 your environment variables.
}
