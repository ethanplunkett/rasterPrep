if(FALSE){
  # Example usage
  shapefile <- "gisdata/projects/Boundaries/projectFlagstaff.shp"

  out5 <- "gisdata/projects/projectFlagstaff/ref5.tif"
  out30 <- "gisdata/projects/projectFlagstaff/ref30.tif"
  out1 <- "gisdata/projects/projectFlagstaff/ref1.tif"
  dir.create(dirname(out5), showWarnings = FALSE)
  burn = 1

  makenewextent(polyFile = shapefile, destination = out5, burn = 1,
                cellsize = 5, nestingCellsize = 30)
  makenewextent(polyFile = shapefile, destination = out30, burn = 1,
                cellsize = 30, nestingCellsize = 30)
  makenewextent(polyFile = shapefile, destination = out1, burn = 1,
                cellsize = 1, nestingCellsize = 30)

  polyFile <- "Z:/Working/ethan/gdaldebugging/testextent.shp"

}


#'This function is for setting up a reference grid from a polygon file
#'containing a focal area boundary.
#'
#'\code{makeReference} creates a raster file large enough to contain the polygon
#'with pixels that are either aligned to the origin (default) or aligned to
#'pixels in another raster.
#'
#'The new extent will be large enough to contain all the pixels within the
#'polygon. By default it will contain pixels aligned to the origin with the
#'pixels edges that are even multiple of the \code{cellsize}.  However, if
#'\code{alignTo} is set to \code{"reference"} and the \code{reference} argument
#'is the path to an existing raster file than the pixels will align with the
#'pixels in that file.
#'
#'If nestingCellsize (optional and probably rarely used) is set to a multiple of
#'cellsize, than the extent will be expanded out to match the extent that would
#'be needed by this larger cellsize. This is useful if you are working at
#'multiple scales and want all scales to have the exact same extent. One example
#'would be to set cellsize to 5 and nestingCellsize to 30. That will produce a
#'reference raster with 5 meter pixels and an extent that will work well with
#'both 5 and 30 meter pixels. Setting 30 and 30 will result in the same extent
#'but with with 30 meter pixels.

#' @param polyFile (character) path to a polygon shapefile (with .shp extension)
#'    containing the boundaries of the focal area.
#' @param destination (character) path to a .tif file where the reference grid
#'   will be created.
#' @param cellsize (numeric) the desired resolution or cellsize of the the
#'   reference file.
#' @param burn (integer between 0 and 255) the value to burn into cells that fall
#'    inside the polygon
#' @param alignTo (character) either "origin", the default, which causes cells to
#'   align to the origin of the projection such that cell boundaries fall on
#'   integer multiples of the cellsize; or "reference" in which case cells will
#'   be aligned with cells in the \code{reference} raster. If \code{alignTo} is
#'   set to "reference" than \code{cellsize} is optional and defaults to the
#'   resolution of the reference raster, but the extent is still taken from
#'   \code{polyFile}.
#' @param reference (optional, character) if \code{alignTo} is set to "reference" than
#'   this should be the path to an existing raster file who's cell alignment
#'   we would like to match.
#' @param nestingCellsize (numeric) This is optional and likely not needed.  If
#' \code{nestingCellsize} is provided it must be a multiple of \code{cellsize} and
#'  the extent will be expanded out to match the extent that would be
#'  needed by this larger cellsize. This facilitates multi-scale analysis with
#'  identical extents for all scales.
#'
#' @return This function is called for the side effect of creating a reference
#'   tif at the \code{destination} path. It does not return anything.
#' @export
makeReference <- function(polyFile, destination, cellsize,  burn = 1,
                          alignTo = "origin", reference,  nestingCellsize = cellsize){
  if(!missing(reference)){
    refinfo <- suppressWarnings(as.list(rgdal::GDALinfo(reference)))
    if(missing(cellsize)){
      # If cellsize not set and reference is take scellsize (and likely nestingCellsize) from reference
      stopifnot(isTRUE(all.equal(refinfo$res.x, refinfo$res.y)))
      cellsize <- refinfo$res.x
      if(missing(nestingCellsize))
        nestingCellsize <- cellsize
    }
  }


  stopifnot(nestingCellsize %% cellsize == 0)   # nestingCellsize must be the same or an integer multiple of cellsize
  stopifnot(alignTo %in% c("origin", "reference"))

  layer <- gsub("\\.[Ss][Hh][Pp]$", "", basename(polyFile))
  poly <- rgdal::readOGR(dsn = dirname(polyFile), layer = layer)
  # poly <- readshape(polyFile)

  # Find the extent that contains the polygon and where the cell edges fall
  # neatly on multiples of the cellsize.
  bbox <- poly@bbox

  if(alignTo == "origin"){
    yoffset <- xoffset <- 0
  }  else {  #  ( alignTo == "reference")

    cellsizes.ok <- function(a, b){
      # This checks to make sure a is either
      #  equal to b
      #   a multiple of b
      #   or a factor of b
      # allowing for some tiny rounding errors via all.equal
      ok <- FALSE
      if(isTRUE(all.equal(a  %% b,  0))) ok <- TRUE
      if(isTRUE(all.equal(b  %% a,  0))) ok <- TRUE
      if(isTRUE(all.equal(a, b))) ok <- TRUE
      return(ok)
    }

    if( !(cellsizes.ok(refinfo$res.x, nestingCellsize) & cellsizes.ok(refinfo$res.y, nestingCellsize) ) )
      stop("The cellsize is incompatible with the reference cellsize")

    # calculate the (positive) offset of the 1'st vertical cell edge from the origin.
    xoffset <- refinfo$ll.x %% nestingCellsize
    yoffset <- refinfo$ll.y %% nestingCellsize


  }


  # Figure out extent that contains the bounding box of the polygon and snaps to the origin of the
  # projection
  xmin <- bbox[1, 1]
  ymin <- bbox[2, 1]
  xmax <- bbox[1, 2]
  ymax <- bbox[2, 2]

  xmin <- snapToEdge(x = xmin, cellsize = nestingCellsize, FALSE, offset = xoffset)
  ymin <- snapToEdge(x = ymin, cellsize = nestingCellsize, FALSE, offset = yoffset)
  xmax <- snapToEdge(x = xmax, cellsize = nestingCellsize, TRUE, offset = xoffset)
  ymax <- snapToEdge(x = ymax, cellsize = nestingCellsize, TRUE, offset = yoffset)

  # Double check alignment
  is.aligned <- function(loc, cs, offset){
    # Function to check alignment of an x or y location
    # given the cellsize and offset
    # Returns TRUE if aligned
    if(isTRUE(all.equal(loc %% cs,  offset))) return(TRUE)
    if(isTRUE(all.equal(loc %% cs,  offset + cs))) return(TRUE)
    return(FALSE)
  }
  stopifnot(
    isTRUE(is.aligned(xmin , nestingCellsize,  xoffset)),
    isTRUE(is.aligned(ymin, nestingCellsize,  yoffset)),
    isTRUE(is.aligned(xmax, nestingCellsize,  xoffset)),
    isTRUE(is.aligned(ymax, nestingCellsize,  yoffset))
  )

  gdalUtils::gdal_rasterize(src_datasource = polyFile,
                            dst_filename = destination,
                            burn = burn,
                            te = c(xmin, ymin, xmax, ymax),
                            tr = c(cellsize, cellsize),
                            ot = "byte",
                            a_nodata = 2^8 -1)

}

