#' function to standardize type and determine associated no data value.
#'
#' This function accepts type strings as used by gdal_translate and gdal_warp:
#' \code{"Byte"},  \code{"Int16"},  \code{"UInt16"},  \code{"Int32"},
#' \code{"UInt32"},  \code{"Float32"},  \code{"Float64"} or as used
#' by the raster package (See \code{\link[raster]{dataType}}), standardizes them
#' to work with gdal and returns the type, a logical indicating if it's a signed
#' byte, and the no data value which should be used with that type. No data
#' values used here are not consistent with the text in the
#' \code{\link[raster]{dataType}} for signed integer types (values here are one
#' less).
#'
#' SignedBytes are special case in gdal and are written as bytes with an
#' additional flag that indicates they are signed. I'm not sure how widely this
#' is supported in other software, although I've included it here I don't
#' currently support it in the user level functions in this package.
#'
#' This function is primarily intended for internal use.
#'
#'
#' @param type A numeric type designation as used by gdal_warp and
#'   gdal_translate or as used by the raster package.
#'
#' @return \code{assessType} returns a list with:
#' \describe{
#'   \item{type}{(character) indicating the datatype in the format used by gdal}
#'   \item{isSignedByte}{(logical) indicating if it should be written as a signed
#'   byte }
#'   \item{noDataValue}{(numeric) the value that will be used to represent
#'   no data in a raster of this type.}
#'  }
#' @export
#' @keywords internal
assessType <- function(type){
  isSignedByte <- FALSE
  tab <- data.frame(type = c("SignedByte",
                             "Byte",
                             "Int16",
                             "UInt16",
                             "Int32",
                             "UInt32",
                             "Float32",
                             "Float64"),
                    raster = c("INT1S",
                               "INT1U",
                               "INT2S",
                               "INT2U",
                               "INT4S",
                               "INT4U",
                               "FLT4S",
                               "FLT8S") ,
                    no.data.value = c(
                     -2^7,        #  Signed byte
                      2^8 -1,  #"Byte",
                      -2^15,# "Int16",
                      2^16 - 1, # "UInt16",
                      -2^31 -1 , #"Int32", Note this is one less than the theor
                      2^32-1, #"UInt32",
                      -3.4E+38,# "Float32",
                      -1.7E+308), # "Float64"
                    stringsAsFactors = FALSE
  )
   if(tolower(type) %in% tolower(tab$raster))
     type <-  tab$type[tolower(tab$raster) == tolower(type) ]


   if(!tolower(type) %in% tolower(tab$type))
     stop("Unrecognized type value: '", type, "' use one of '",
          paste(tab$type, collapse ="', '"), "'", sep ="")

  noDataValue <- tab$no.data.value[tolower(tab$type) == tolower(type)]

  if(tolower(type) == "signedbyte"){
    isSignedByte = TRUE
    type = "Byte"
  }

  # Make sure the returned type has proper capitilization
  type <-  tab$type[tolower(tab$type) == tolower(type) ]

  if(type == "UInt32") warning("You can make 32 Bit unsigned integer files but they aren't supported by R.")

  return(list(type = type, isSignedByte = isSignedByte, noDataValue =  noDataValue))

}
