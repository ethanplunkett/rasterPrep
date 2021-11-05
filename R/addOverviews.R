#'Function to add overviews to an image file
#'
#' This function creates overviews (aka pyramids) with compressed copies
#'  of the data from a raster at multiple coarser resolutions. They are saved in
#'  a side car file with the extension ".orv" appended to the original file's
#'  full name (e.g. "dem.tif.orv" would contain the overviews for "dem.tif").
#'  Overview are used by GIS software to display the raster file more quickly
#'  when zoomed out. They are especially useful for large files.
#'
#'Generally the default arguments are good but you may want to change the method
#'to "average" for continuous raster files and possibly to "mode" for categorical
#'files. For continuous files, especially imagery, if you are OK with
#' lossy compression in the overviews you may change the compression to
#' \code{"JPEG"} which should result in smaller files.
#'
#'#' This is a convenience wrapper to the
#' \href{https://www.gdal.org/gdaladdo.html}{gdaladdo}  command line utility.
#' \code{addOverviews} sets reasonable defaults, only some of
#' which can be overridden.
#'
#' @param x (character) the path to a tif or other raster file
#' @param clean (logical) defaults to FALSE; if TRUE overviews are removed
#'   instead of added
#' @param compression (character) the style of compression used in the overviews
#'  options are: "LZW" (the default), "DEFLATE", and "JPEG"
#' @param method (character) the resampling method one of nearest,
#'   average, gauss, cubic, cubicspline, lanczos,
#'   average_mp, average_magphase, or mode. It is passed to
#'   \href{http://www.gdal.org/gdaladdo.html}{gdaladdo} check there for details.
#'    \code{"near"} is also accepted and silently converted to, \code{"nearest"}.
#' @return this function creates an additional ".orv" file alongside \code{x} with
#'  overview information.  It does not return anything.
#' @export
addOverviews <- function(x, clean = FALSE, compression = "LZW", method = "nearest"){

  # Note gdalwarp uses "near" while gdaladdo uses "nearest"
  # this allows addOverviews function to use either
  if(method == "near") method <- "nearest"


  valid.methods <- c("nearest","average","gauss","cubic","cubicspline","lanczos",
                     "average_mp","average_magphase", "mode")

  if(! method %in% valid.methods)
    stop("resampling method '", method, "' isn't recognized", "use one of '" ,
         paste(valid.methods, collapse = "' '"), "'", sep = "")

  command <- paste0("gdaladdo -ro ", shQuote(x), " 2 4 8 16 32 64 128 256",
                    " --config COMPRESS_OVERVIEW ", compression, " -r ", method )
  if(clean)
    command <- paste0(command, " -clean")

  cat("Adding overviews with system command:\n", command, "\n")

  # Temporarily reset the PROJ_LIB environmental setting for system call (if indicated by settings)
  oprojlib <- Sys.getenv("PROJ_LIB")
  ogdaldata <- Sys.getenv("GDAL_DATA")
  if(rasterPrepSettings$resetLibs){
    Sys.setenv(PROJ_LIB = rasterPrepSettings$projLib )
    Sys.setenv(GDAL_DATA = rasterPrepSettings$gdalData)
    on.exit({
      Sys.setenv(PROJ_LIB = oprojlib)
      Sys.setenv(GDAL_DATA = ogdaldata)
    })
  }

  a <- system(command = command, intern = TRUE, wait = TRUE)
}

