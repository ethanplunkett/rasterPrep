#' Standardize type and determine associated no data value.
#'
#' This function accepts type strings as used by gdal_translate and gdal_warp:
#' `"Byte"`,  `"Int16"`,  `"UInt16"`,  `"Int32"`,
#' `"UInt32"`,  `"Float32"`,  `"Float64"`; or as used
#' by [terra::writeRaster()], standardizes them
#' to work with gdal and returns the type, a logical indicating if it's a signed
#' byte, and a no data value appropriate for that type.
#'
#' There is no universally accepted no data value for each type. Every
#' file can have it's own no data value.  `rasterPrep` uses the
#' highest possible value for unsigned integers, lowest for signed integers.
#' For floating point numbers it uses values close to the lowest possible.
#' These follow the defaults used by the deprecated **raster** package,
#' and for integers are consistent with [terra::writeRaster()] documentation.
#'
#' SignedBytes are special case in gdal and are written as bytes with an
#' additional flag that indicates they are signed. I'm not sure how widely this
#' is supported in other software. Although its included it here it is not
#' currently support by other **rasterPrep** functions.
#'
#' Default No Data Values used in **rasterPrep**
#'
#' | gdal      |terra  | No Data Value |  Formula  |
#' |:----------|:------|---------------|----------:|
#' |SignedByte |INT1S  |  -1.280000e+02| -2^7      |
#' |Byte       |INT1U  |   2.550000e+02| 2^8 -1    |
#' |Int16      |INT2S  |  -3.276700e+04| -2^15+1   |
#' |UInt16     |INT2U  |   6.553500e+04| 2^16 - 1  |
#' |Int32      |INT4S  |  -2.147484e+09| -2^31+1   |
#' |UInt32     |INT4U  |   4.294967e+09| 2^32-1    |
#' |Float32    |FLT4S  |  -3.400000e+38| -3.4E+38  |
#' |Float64    |FLT8S  | -1.700000e+308| -1.7E+308 |
#'
#' @param type A data type designation as used by
#' [gdalwarp](https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-ot) and
#' [gdal_translate](https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-ot) # nolint: line_length_lintr
#'  `-ot` arguments, or as used by the [terra::writeRaster] `datatype` argument.
#'
#' @return `assessType` returns a list with:
#' \describe{
#'   \item{type}{(character) indicating the data type in the format used by
#'   gdal}
#'   \item{isSignedByte}{(logical) indicating if it should be written as a
#'   signed byte}
#'   \item{noDataValue}{(numeric) the value that will be used to represent
#'   no data in a raster of this type.}
#'  }
#' @export
#' @keywords internal
assessType <- function(type) {
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
                    no.data.value = c(-2^7,        #  Signed byte
                                      2^8 - 1,  #"Byte",
                                      -2^15 + 1, # "Int16",
                                      2^16 - 1, # "UInt16",
                                      -2^31 + 1, #"Int32",
                                      2^32 - 1, #"UInt32",
                                      -3.4E+38, # "Float32",
                                      -1.7E+308), # "Float64"
                    stringsAsFactors = FALSE)

  if (tolower(type) %in% tolower(tab$raster))
    type <-  tab$type[tolower(tab$raster) == tolower(type)]

  if (!tolower(type) %in% tolower(tab$type))
    stop("Unrecognized type value: '", type, "' use one of '",
         paste(tab$type, collapse = "', '"), "'", sep = "")

  noDataValue <- tab$no.data.value[tolower(tab$type) == tolower(type)]

  if (tolower(type) == "signedbyte") {
    isSignedByte <- TRUE
    type <- "Byte"
  }

  # Make sure the returned type has proper capitilization
  type <-  tab$type[tolower(tab$type) == tolower(type)]

  if (type == "UInt32")
    warning("You can make 32 Bit unsigned integer files but they ",
            "aren't supported by R.")

  return(list(type = type,
              isSignedByte = isSignedByte,
              noDataValue =  noDataValue))

}
