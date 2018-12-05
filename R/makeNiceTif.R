#' Function to compress and possibly reformat a tif with options to build
#' overviews and vat
#'
#' This function creates a compressed copy of a tif with LZW compression,
#' stats, and internal tiling. It will optionally build overview and add a vat.
#' The output should be ready to view quickly and easily in interactive GIS
#' software.
#'
#' This function is intended to be called after analysis to prepare result
#' rasters so they can be viewed easily in GIS software. Because it creates
#' tiled .tif files it is not a good idea to run on files in preparation for
#'  analysis with the raster package as that package reads by lines.
#'
#' type is currently passed to gdal_translate which will reset the
#'  type to the desired value but will NOT update NA values appropriately.
#'  This may still work out if you (1) are not converting between signed and
#'  unsigned types and (2) are moving to a smaller bit depth. For example
#'  moving from an Int32 to and Int16 should work because the NA value from Int32
#'  will be out of range and truncated to the highest value in the Int16 which
#'  is the appropriate NA value for Int16. In the future I might add an internal
#'  call to do the type conversion for you.  For now if you want to change the
#'  type you can do so safely in a call to warpToReference prior to using this
#'  function.
#'
#'
#' This is a wrapper to \code{gdaltranslate} coupled with
#' \code{\link{addOverviews}} and \code{\link{addVat}}.  For finer control over
#' translation see \code{\link[gdalUtils]{gdal_translate}}.
#'
#' If you intend to create a color table for a categorical raster first call
#' \code{\link{addColorTable}} and then pass the .vrt file it creates to this function.
#
#' @param source (character) path to a raster file readable by gdal.
#' @param destination (character) path to a .tif file to be created for viewing with GIS software
#' @param type (character) NOT CURRENTLY SUPPORTED! In the future if supplied
#'  the output may be converted to this type.
#'   Should be one of \code{"Byte"}, \code{"UInt16"}, \code{"Int16"},
#'   \code{"UInt32"}, \code{"Int32"}, \code{"Float32"}, \code{"Float64"} or for convenience you may
#' also use the raster package's designations: \code{\link[raster]{dataType}}.
#' @param overwrite (logical) if \code{TRUE} any existing file will be overwritten
#' @param buildOverviews (logical) if \code{TRUE} overviews (AKA pyramids) will be built
#' @param overviewResample (character) one of  \code{"nearest"},
#'  \code{"average"}, \code{"gauss"}, \code{"cubic"}, \code{"cubicspline"},
#'  \code{"lanczos"}, \code{"average_mp"}, \code{"average_magphase"}, \code{"mode"} see \href{https://www.gdal.org/gdaladdo.html}{gdaladdo} for details. For convenience \code{"near"} is silently updated to \code{"nearest"}
#' @param vat (logical) if \code{TRUE} an ESRI Value Attribute Table (VAT) sidecar file will be
#'   created containing all the unique values in the grid and
#'    their associated count of cells. This is only recommended for categorical
#'     data and can be slow but will speed up setting up symbology of that data in ArcGIS.
#' @param stats (logical) if \code{TRUE} than statistics are generated and saved; this helps GIS software transform continuous data (e.g. make a standard deviation color ramp)
#' @return This function creates a copy of the source raster at the destination
#' path that is formatted to facilitate viewing in GIS software. It does not return
#' anything.
#'
#' @export
makeNiceTif <- function(source, destination,  type, overwrite = FALSE,
                        buildOverviews = TRUE, overviewResample = "nearest",
                        vat = FALSE, stats = TRUE ){
  if(!file.exists(source)) stop("input file", source, "is missing.")
  if(file.exists(destination)){
   if(overwrite){
     deleteTif(destination)
   } else {
    stop("destination file already exists: ", destination)
   }
  }

  has.type <- FALSE
  is.signed.byte <- FALSE

  if(!missing(type)){
    warning("Type conversion will work in some cases but in others will not properly conserve NA encoding. Use with caution.  I might fix this someday.")
    has.type <- TRUE
    a <- assessType(type)
    type <- a$type
    is.signed.byte <- a$isSignedByte
    no.data.value <- a$noDataValue
  }

  qc <- '"'
  command <- paste0("gdal_translate -stats -co compress=LZW -co TFW=YES -co TILED=YES ",
                    qc, source, qc, " ", qc, destination, qc, " ")

  if(has.type)
    command <- paste0(command,
                      " -ot ", type, " ",
                      " -a_nodata ", no.data.value, " "  # NOTE
                 )

  if(is.signed.byte)
    command <- paste0(command,
                      " -co  PIXELTYPE=SIGNEDBYTE ")

  if(stats)
    command <- paste0(command, "-stats ")

  cat("Compressing with system command:\n", command, "\n")
  a <- system(command = command, intern = TRUE, wait = TRUE)

  stopifnot(file.exists(destination))

  if(buildOverviews){
   addOverviews(x = destination, method = overviewResample)
  }

  if(vat){
    cat("building vat")
    addVat(destination)
  }

}
