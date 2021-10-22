rasterPrepSettings <- new.env()

rasterPrepSettings$resetLibs <- TRUE
rasterPrepSettings$projLib <- ""
rasterPrepSettings$gdalData <- ""

#' Function to change rasterPrep package settings
#'
#' This functions allows the user to print or change the settings for the rasterPrep package
#'
#' Currently there are three settings:
#'
#' \code{resetLibs} -  controls whether raster prep should attempt to reset
#' the system environmental settings for PROJ_LIB and GDAL_DATA prior to executing system
#' calls it defaults to TRUE.
#'
#' \code{projLib} - is what the PROJ_LIB environental setting should be set to orior to the calls.
#'   It defaults to an empty string (\code{""}).
#'
#'\code{gdalData} - is what the GDAL_DATA environemental variable should be set to prior to
#'  system calls (if resetsLibs is TRUE). It defaults to an empty string (\code{""}).
#'
#' On my system in a clean R session Sys.getenv("PROJ_LIB") will return "" but
#' after rgdal or sp are loaded it will be "C:/Users/user/Documents/R/win-library/4.0/rgdal/proj"
#' GDAL_DATA is similarly changed.  Also note if sf is loaded it changes the GDAL_DATA variable to
#'  something else.
#'
#' Unfortunately that means that when you try to run any  of the gdal utilities via a system call
#' they don't use the co-installed proj library but instead use rgdal's version.
#' If the versions match this may not be a problem but if they don't things may go wrong
#' in sometimes obvious and sometimes very subtle ways.  gdal_utilities gets around this
#' by using GDAL installed with sf but doesn't support all of the utilities I use here - gdaladdo
#' is missing.
#'
#' With the default values
#' rasterPrep will change the PROJ_LIB and GDAL_DATA enviromental setting  to "" before making a
#'system call.
#'
#' This can be turned off with \code{rasterPrepOptions(resetLibs = FALSE)} or changed to
#' to different location with for example \code{rasterPrepOptions(projLib = "C:/Proj/")}
#' or some appropriate for your system.
#'
#' The package will always reset the PROJ_LIB and GDAL_DATA variables to the original value after
#' each the system call so subsequent rgdal, sp, and sf functions will  find them set as expected
#' or at least as it was (there seem to be dueling settings!).
#'
#' Also, note all the changes made by rgdal, sp, sf, and this package are just for the r session not
#' for the computer as a whole.
#'
#' See:   https://github.com/r-spatial/discuss/issues/31 for a discussion of this issue.
#'
#'
#'
#'
#' @param ... Arguments should be settings to reset with their value the new setting.
#'  If called with no arguments nothing is changed but the current settings are printed.
#'
#' @return
#' @export
#' @return a list of the current settings is returned if the function is called with no
#' arguments.

rasterPrepOptions <- function(...){

  # This function allows changing raster prep settings
  # ... arguments must be setting names and their new value
  #  if called without arguments will print current settings
  #

  args <- list(...)
  if(!all(names(args) %in% names(rasterPrepSettings))){
    bogus <- setdiff(names(args), names(rasterPrepSettings) )
    stop("These arguments are not valid rasterPrepSettings: ", paste(bogus, collapse = ", "))
  }

  if("resetLibs" %in% names(args)){
    a <- args$resetLibs
    if(!is.logical(a) || length(a) != 1 || is.na(a))
      stop("resetLibs should be TRUE or FALSE" )
    rasterPrepSettings$resetLibs <- a
  }
  if("projLib" %in% names(args)){
    a  <- args$projLib
    if(!is.character(a) || length(a) != 1 || is.na(a))
      stop("projLib should be a character string" )
    rasterPrepSettings$projLib <- a
  }
  if("gdalData" %in% names(args)){
    a  <- args$gdalData
    if(!is.character(a) || length(a) != 1 || is.na(a))
      stop("gdalData should be a character string" )
    rasterPrepSettings$gdalData <- a
  }
  if(length(args) == 0) print(as.list(rasterPrepSettings))


}
