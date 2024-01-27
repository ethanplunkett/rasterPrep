
#' Create a VAT for a raster file
#'
#' This function creates a new sidecar file next to `x` which contains a
#' value attribute table (VAT).  A VAT is used by ESRI GIS software to encode
#' additional information about each value in a classified integer raster.
#'
#' It calls [terra::freq()] to generate a table of values and
#' frequencies from the raster `x` which can take a long time for large
#' files.  It should only be called on integer encoded files.
#'
#' A VAT is a a dbf file with "VALUE" and "COUNT" fields containing all the unique
#' values and their associated frequency in the raster file. The VAT file's
#' name will be the name of the image file with ".vat.dbf" appended; for a tif
#' this means the file will end in ".tif.vat.dbf". The column names in a .dbf
#' file are restricted to 11 characters and cannot contain periods; for detailed
#' limitations see [foreign::write.dbf()] which is used to write the
#' VAT.
#'
#'
#' @param x the path to a geoTIFF file; use with other raster formats
#'   is untested.
#' @param attributes a `data.frame` with a column `VALUE` (case insensitive) along with
#'   any other data that should be added to the VAT. Column names should not
#'   contain periods and are limited to 11 characters.
#' @param skipCount if TRUE then this function will assume that all values in
#'   the grid are represented in `attributes` and will not calculate the
#'   frequency of each class. This speeds things up tremendously but means the
#'   VAT will not have a COUNT field. In tests these files display in ArcGIS and
#'   allow for identification of cells and their attributes; but it does not
#'   conform to ESRI's apparent standard of having both VALUE and COUNT in a
#'   VAT.
#' @return This function creates a VAT file but returns nothing.
#' @export
addVat <- function(x, attributes, skipCount = FALSE){

  r <- terra::rast(x)
  if(length(r) > 1)
    stop("addVat only works with single layer TIFF files")

  has.att <- !missing(attributes)
  # Pre-check attribute name length
  if(has.att){
    names(attributes)[tolower(names(attributes)) == "value"] <- "VALUE"
    stopifnot("VALUE" %in% names(attributes))
    long <- nchar(names(attributes)) > 11
    if(any(long))
      stop("Atribute column names are too long for dbf format:", names(attributes)[long])
  }

  if(skipCount){
    if(!has.att)
      stop("attributes required if skipCount is TRUE")
    ft <- attributes[order(attributes$VALUE), ]
  } else {

    ft <- terra::freq(r)

    # terra::freq() returns labels as the "value" if the raster has labels
    # The code  below retrieves the associated values
    if(!is.null(terra::cats(r)[[1]])){
      names(ft)[names(ft) == "value"] <- "category"
      cgs <- terra::cats(r)[[1]]
      stopifnot(all(ft$category %in% cgs$category))
      mv <- match(ft$category, cgs$category)
      ft$value <- cgs$value[mv]
    }

    names(ft)[names(ft) == "value"] <- "VALUE"
    names(ft)[names(ft) == "count"] <- "COUNT"

    ft <- ft[, c("VALUE", "COUNT"), drop = FALSE]
  }

  if(has.att){
    stopifnot(all(ft$VALUE %in% attributes$VALUE))
    mv <- match(ft$VALUE, attributes$VALUE)
    ft <- cbind(ft, attributes[mv, !names(attributes) == "VALUE",
                               drop = FALSE])
  }


foreign::write.dbf(ft, paste0(x, ".vat.dbf"), factor2char = TRUE,
                   max_nchar = 254)
}
