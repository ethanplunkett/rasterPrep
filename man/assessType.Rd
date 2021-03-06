% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assessType.R
\name{assessType}
\alias{assessType}
\title{function to standardize type and determine associated no data value.}
\usage{
assessType(type)
}
\arguments{
\item{type}{A numeric type designation as used by gdal_warp and
gdal_translate or as used by the raster package.}
}
\value{
\code{assessType} returns a list with:
\describe{
  \item{type}{(character) indicating the datatype in the format used by gdal}
  \item{isSignedByte}{(logical) indicating if it should be written as a signed
  byte }
  \item{noDataValue}{(numeric) the value that will be used to represent
  no data in a raster of this type.}
 }
}
\description{
This function accepts type strings as used by gdal_translate and gdal_warp:
\code{"Byte"},  \code{"Int16"},  \code{"UInt16"},  \code{"Int32"},
\code{"UInt32"},  \code{"Float32"},  \code{"Float64"} or as used
by the raster package (See \code{\link[raster]{dataType}}), standardizes them
to work with gdal and returns the type, a logical indicating if it's a signed
byte, and the no data value which should be used with that type. No data
values used here are not consistent with the text in the
\code{\link[raster]{dataType}} for signed integer types (values here are one
less).
}
\details{
SignedBytes are special case in gdal and are written as bytes with an
additional flag that indicates they are signed. I'm not sure how widely this
is supported in other software, although I've included it here I don't
currently support it in the user level functions in this package.

This function is primarily intended for internal use.
}
\keyword{internal}
