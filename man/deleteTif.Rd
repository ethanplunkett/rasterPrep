% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteTif.R
\name{deleteTif}
\alias{deleteTif}
\title{Function to delete one or more .tif files and all associated files}
\usage{
deleteTif(x, includeVrt = TRUE)
}
\arguments{
\item{x}{(character) the path to one or more .tif files.}

\item{includeVrt}{if TRUE (the default) .vrt files with the same base name
will also be deleted.}
}
\value{
this function returns nothing
}
\description{
This will delete the files listed in \code{x} and all associated files,
specifically: \itemize{
\item any file that includes the full name with .tif extension and an
additional extension  e.g. .../name.tif.ovr.
\item a world file which has the same base name but a ".twf" extension
instead of ".tif"
\item if \code{includeVrt} is TRUE a .vrt file with the same name.
}
}
