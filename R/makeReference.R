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
#'    containing the boundaries of the focal area in the desired projection.
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
#'   we would like to match. Its projection should match that of the polyFile
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

  # Note in March 2020 with current GDAL and PROJ I couldn't get gdal_rasterize to honor the projection
  # of the input file.  Prior functionality and documentation represent that the projection information
  # is retained from the polygon file used but I found that not to be the case. I also found that
  # specifying the output projection either with a proj4 style string or with a path to a wkt file
  # also resulted in a file with no projection.
  #
  # To solve this I did something hacky that I hate. I did the rasterizing to a temporary file
  # and then used gdal_translate to copy while specifying an output rpojection. Finally, I deleted
  # the temp file and then wkt file that specified the projection.
  #  At some future date it might make sense to test if gdal_translate is behaving again and save
  # creating an extra copy.


  # based on terra
  if(!missing(reference)){
    refinfo <- terra::rast(reference)
    ref.res <- terra::res(refinfo)
    if(missing(cellsize)){
      # If cellsize not set and reference is take cellsize (and likely nestingCellsize) from reference
      stopifnot(isTRUE(all.equal(ref.res[1], ref.res[2])))
      cellsize <- ref.res[1]
      if(missing(nestingCellsize))
        nestingCellsize <- cellsize
    }
  }



  stopifnot(nestingCellsize %% cellsize == 0)   # nestingCellsize must be the same or an integer multiple of cellsize
  stopifnot(alignTo %in% c("origin", "reference"))

  #layer <- gsub("\\.[Ss][Hh][Pp]$", "", basename(polyFile))
  # poly <- rgdal::readOGR(dsn = dirname(polyFile), layer = layer)
  # proj <- wkt(poly)
  # bbox <- poly@bbox

  poly <- sf::st_read(polyFile)
  proj <- sf::st_crs(poly)$wkt
  bbox <- sf::st_bbox(poly)

  wkt.file <- tempfile(fileext = ".txt")
  cat(proj,  file = wkt.file, append = FALSE)

  # Find the extent that contains the polygon and where the cell edges fall
  # neatly on multiples of the cellsize.


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

    if( !(cellsizes.ok(ref.res[1], nestingCellsize) & cellsizes.ok(ref.res[2], nestingCellsize) ) )
      stop("The cellsize is incompatible with the reference cellsize")

    # calculate the (positive) offset of the 1'st vertical cell edge from the origin.

  # rgdal code:
  #  xoffset <- refinfo$ll.x %% nestingCellsize
  #  yoffset <- refinfo$ll.y %% nestingCellsize

  # terra based code:
    xoffset <- terra::ext(refinfo)[1] %% nestingCellsize  # xmin
    yoffset <- terra::ext(refinfo)[3] %% nestingCellsize  # ymin
  }


  # Figure out extent that contains the bounding box of the polygon and snaps to the origin of the
  # projection
  xmin <- as.numeric(bbox[1])  # sf bbox is a list of 4
  ymin <- as.numeric(bbox[2])
  xmax <- as.numeric(bbox[3])
  ymax <- as.numeric(bbox[4])

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

  stopifnot(grepl(".tif$", destination, ignore.case = TRUE))

  command <- "gdal_rasterize"
  command <- paste0(command, " -burn ", burn, " -te ", xmin, " ", ymin, " ", xmax, " ", ymax)
  command <- paste0(command, " -tr ", cellsize, " ", cellsize)
  command <- paste0(command, " -ot byte")
  command <- paste0(command, " -a_nodata ", 255)
  command <- paste0(command, " ", polyFile, " ", destination)

  # Temporarily reset the PROJ_LIB environmental setting for system call (if indicated by settings)
  oprojlib <- Sys.getenv("PROJ_LIB")
  if(rasterPrepSettings$setProjLib){
    Sys.setenv(PROJ_LIB = rasterPrepSettings$projLib )
    on.exit(Sys.setenv(PROJ_LIB = oprojlib))
  }

  cat("Rasterizing polygon to new extent with system command:\n", command, "\n")
  a <- system(command = command, intern = TRUE, wait = TRUE)
  a <-  gsub("[[:blank:]]", " ", a)
  if(!grepl("- done.[[:blank:]]*$", a[length(a)]) ){
    stop("An error might have occured.  The function returned: ", a)
  }

  if(!file.exists(destination))
    stop("Output file", destination, "was not created. System call returned: ", a)

  stopifnot(file.exists(destination))
}

