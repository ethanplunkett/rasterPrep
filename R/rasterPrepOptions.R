rasterPrepSettings <- new.env()

rasterPrepSettings$setProjLib <- TRUE
rasterPrepSettings$projLib <- ""

#' Function to change rasterPrep package settings
#'
#' This functions allows the user to print or change the settings for the rasterPrep package
#'
#' Currently there are two settings:
#'
#' setProjLib -  controls whether raster prep should attempt to set
#' the system environmental setting "PROJ_LIB" before executing system and gdalUtils calls
#' it is initially TRUE.
#'
#' projLib - is what the PROJ_LIB environental setting should be set to, intitially "", an empty string.
#'
#' On my system in a clean R session Sys.getenv("PROJ_LIB") will return "" but
#' after rgdal is loaded it will be "C:/Users/user/Documents/R/win-library/4.0/rgdal/proj"
#' Unfortunately that means that when you try to run gdal_utulities via a system call
#' they don't use the co-installed proj library but instead use rgdal's version.
#' If the versions match this may not be a problem but if they don't things may go wrong
#' in sometimes obvious and sometimes very subtle ways.
#'
#' With the intial values
#' rasterPrep will change the PROJ_LIB enviromental setting to "" before making a system call.
#'
#' This can be turned off with rasterPrepOptions(setProjLib = FALSE) or changed to
#' a different proj lib location with rasterPrepOptions(projLib = "C:/Proj/") or some other path
#' appropriate for your system.
#'
#' The package will always reset the PROJ_LIB value to the original value after the system call
#' so subsequent  rgdal functions will still find it set as expected by that package.
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
#'
#' @examples
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

  if("resetProjLib" %in% names(args)){
    a <- args$resetProjLib
    if(!is.logical(a) || length(a) != 1 || is.na(a))
      stop("resetProjLib should be TRUE or FALSE" )
    rasterPrepSettings$resetProjLib <- a
  }
  if("projLib" %in% names(args)){
    a  <- args$projLib
    if(!is.character(a) || length(a) != 1 || is.na(a))
      stop("projLib should be a single character string" )
    rasterPrepSettings$projLib <- a
  }
  if(length(args) == 0) print(as.list(rasterPrepSettings))


}
