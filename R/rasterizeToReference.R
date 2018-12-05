if(FALSE){
  source <- "Y:/Projects/JFSP/gisdata/finalcut/Alpine/districtBuffer.shp"
  coresource <- "Y:/Projects/JFSP/gisdata/finalcut/Alpine/district.shp"
  destination <- "Y:/Projects/JFSP/gisdata/finalcut2/Alpine/buffer.tif"
  reference <- "Y:/Projects/JFSP/gisdata/finalcut2/Alpine/aspect.tif"
  burn <- 1
  overwrite <- TRUE
  type <- "byte"
  extra = " -a_nodata 255 "

  gdal_rasterize(source, destination, reference, burn = burn, type = type, extra = extra)

  gdal_rasterize(coresource, destination, burn = 2)

}


#' Function to rasterize vector data
#'
#' This function rasterizes vector data into a file that coincides (same extent,
#' cellsize, projection, and cell alignment) with a reference raster file.
#'
#' If the file at \code{destination} exists the new pixels will be overlaid on
#' existing data such that existing data will persist where it does not underlie
#' the vector data in \code{source}.
#'
#' Either the \code{burn} or \code{attribute} argument should be used to set the
#' value assigned to cells.  The default type (\code{"Byte"}) supports integers 0 to
#' 255.  If you are writing real numbers or outside
#' of that range be sure to set \code{type} to something appropriate.  Similarly,
#' if you are only writing a few values you may want to set \code{type} to something
#' that supports fewer values such as \code{"Byte"}.
#'
#' When creating a new file
#' the CRS of the source must match that of the reference and this is checked
#' with \code{\link[raster]{compareCRS}}. In some cases the same CRS may be represented
#' in different ways and fail the comparison even though they are equivalent.  In
#' those cases you may want to set \code{checkCRS} to \code{FALSE}. In contrast
#' if you are overlaying data on an existing file then the underlying gdal_rasterize
#' function will reproject while rasterizing and this function will always skip
#' the CRS check.
#'
#'
#' @param source (character) path to a vector GIS file (such as a shapefile)
#' that contains data to be rasterized.
#' @param destination (character) path to a \code{.tif} file that will either be created or added to.
#' @param reference (character) path to raster dataset will be used to define extent, cellsize, and projection
#' used to define the extent of destination if destination does not already exist.
#' @param burn (optional, numeric) value to be burnt into grid (everywhere the \code{source} has data)
#' @param attribute (optional, character) the column name in the source to extract values from
#' @param type the type of grid to create. Should be one of \code{"Byte"}, \code{"UInt16"},
#' \code{"Int16"}, \code{"UInt32"}, \code{"Int32"}, \code{"Float32"},
#' \code{"Float64"} or for convenience you may
#' also use the raster package's designations: \code{\link[raster]{dataType}}.
#' @param checkCRS (logical) if FALSE skip the check that verifies the
#' Coordinate Reference Systems (CRS) of the source matches that of the reference.
#' Only use FALSE if you think that your CRSs are equivalent but you are getting
#'  an error that they don't match.
#' @param allTouched (optional, logical) defaults to FALSE.  If TRUE
#'   "all pixels touched by lines or polygons will be updated, not just those on
#'   the line render path, or whose center point is within the polygon"
#'   see "-at" flag description for gdal_rasterize: \url{http://www.gdal.org/gdal_rasterize.html}
#' @return
#' This function creates a new raster or writes values to an existing raster
#'  at the destination.  It does not return anything.
#' @export
rasterizeToReference <- function(source, destination, reference, burn, attribute,
                           type = "Byte", allTouched = FALSE, checkCRS=TRUE){

  if(missing(burn) & missing(attribute))
    stop("You must specify either burn or attribute for rasterization to work.")

  a <- assessType(type)
  type <- a$type
  no.data.value <- a$noDataValue
  if(type == "SignedByte")
    stop("SignedByte isn't currently supported by rasterizeToReference")


  if(!file.exists(source)) stop("source file ", source, " doesn't exist")

  source.dir <- dirname(source)
  source.layer <- basename(source)
  source.layer <- gsub("\\.[Ss][Hh][Pp]$", "", source.layer)
  src.proj <- rgdal::OGRSpatialRef(dsn = source.dir, layer = source.layer)
  #oi <- ogrInfo(dsn = source.dir, layer = source.layer)
  if(is.na(src.proj) || src.proj == ""){
    "Stop source file must have a defined projection"
  }

  dest.exists <- file.exists(destination)

  if(!dest.exists){
    if(!file.exists(reference)) stop("reference file ", reference, " doesn't exist.")
    # Reference info is only used if the grid doesn't already exist
    ref.info <- rgdal::GDALinfo(reference)
    ref.proj <- attr(ref.info, "projection")
    if(is.na(ref.proj) || ref.proj == ""){
      "Stop reference file must have a defined projection"
    }
    if(checkCRS && !raster::compareCRS(src.proj, ref.proj)){
      stop("When creating a new raster file the source vector coordinate reference system (CRS) must match the reference CRS.  Reproject source to your reference projection.")
    }
    ref.rows <- ref.info[1]
    ref.cols <- ref.info[2]
    ref.xll <- ref.info[4]
    ref.yll <- ref.info[5]
    ref.resx <- ref.info[6]
    ref.resy <- ref.info[7]
    ref.xmax <-  ref.xll + ref.cols * ref.resx
    ref.ymax <- ref.yll + ref.rows * ref.resy
  }


  command <- paste0(shQuote("gdal_rasterize"), " ")
  if(!dest.exists){
    # Note if the destination file exists this function will update it (as long as overwrite = FALSE)
    command <- paste0( command,
                       "-a_srs ", shQuote(ref.proj), " ",
                       "-te ", ref.xll, " ", ref.yll, " ", ref.xmax, " ", ref.ymax, " ",
                       "-tr ", ref.resx, " ", ref.resy, " ",
                       ifelse(missing(type), "", paste0("-ot ", type, " ")),
                       " -a_nodata ", no.data.value, " ")
  }
  if(!missing(burn))
    command <- paste0(command, "-burn ", burn, " ")

  if(!missing(attribute))
    command <- paste0(command, "-a ", shQuote(attribute), " ")

  if(allTouched)
    command <- paste0(command, "-at ")

  command <- paste0( command, shQuote(source), " ", shQuote(destination))

  cat("Rasterizing with:\n", command, "\n")
  a <- system(command = command, intern = TRUE, wait = TRUE)

  if(!file.exists(destination)){
    print(a)
    stop("Output file wasn't created. Something went wrong.")
  }
}
