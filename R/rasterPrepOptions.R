rasterPrepSettings <- new.env()
rasterPrepSettings$resetLibs <- TRUE
rasterPrepSettings$projLib <- ""
rasterPrepSettings$gdalData <- ""
rasterPrepSettings$verbose <- FALSE
rasterPrepSettings$usesf <- TRUE

#'change rasterPrep package settings
#'
#'This function allows the user to retrieve or change the settings for the
#'rasterPrep package
#'
#' Settings in order of decreasing relevance:
#'
#'`verbose` - defaults to `FALSE`. If `TRUE` than progress and GDAL Utiltiy
#'arguments will be printed to the console.
#'
#'`usesf` - If `TRUE` use GDAL Utilty wrappers from the \pkg{sf} package.
#' Set to `FALSE` to use system installed GDAL utilities via `shell`, which
#' must be installed separately.  `usesf = FALSE` has not been tested on all
#' platforms and may be dropped in the future.
#'
#' The remaining three options are all only relevant if `usesf` is `FALSE` and
#' are to manage the issues created by the `sp` and `rgdal`.
#' They may be dropped in the future.
#'
#'`resetLibs` -  controls whether raster prep should attempt to reset the system
#'environmental settings for PROJ_LIB and GDAL_DATA prior to executing system
#'calls it defaults to`TRUE`.
#'
#'`projLib` - is what the `PROJ_LIB` environmental setting should be when making
#' shell calls to GDAL Utilities. It defaults to the system environment
#' variable `RASTERPREP_PROJ` or if that's not set to an empty string (`""`).
#'
#'`gdalData` - is what the GDAL_DATA environmental variable should be set to
#' during shell calls (if `resetsLibs` is `TRUE`). It defaults to an empty
#' string (`""`).
#'
#' There's a lot of historical junk here.  R spatial packages were all put in
#' disarray when the PROJ library was updated from version 4.
#' Prior to that change PROJ was stable and all installed GDAL versions
#' would use the same PROJ library directory.  However, when PROJ started
#' changing it became important that the right PROJ directory was
#' used. To get around PROJ version conflics `sp` and  `rgdal`  started
#' changing system environmental variables when they were loaded.
#' This helped calls to GDAL from those packages find the right PROJ directory
#' but would break calls from R to the system installed GDAL if it used a
#' different PROJ version.  In response `rasterPrep`
#' would defensively reset the the `PROJ_LIB` and `GDAL_DATA` environmental
#' variables for the duration of the system calls it made.
#'
#' See:
#' [https://github.com/r-spatial/discuss/issues/31](https://github.com/r-spatial/discuss/issues/31)  # nolint: line_length_linter
#' for a discussion of this 'issue.
#'
#' \pkg{rasterPrep} currently uses \pkg{sf} by default bipassing all these
#' complications.  As of April 2024 this is new, but assuming it proves stable
#' the options to use system commands (`usesf = FALSE`) may be dropped
#' completely.
#'
#'@param ... Arguments should be settings to reset with their value the new
#'  setting. If called with no arguments nothing is changed but the current
#'  settings are returned.
#'@export
#'@return a list of the current settings is returned if the function is called
#'  with no arguments.
rasterPrepOptions <- function(...) {

  # This function allows changing raster prep settings
  # ... arguments must be setting names and their new value
  #  if called without arguments will print current settings
  #

  args <- list(...)
  if (!all(names(args) %in% names(rasterPrepSettings))) {
    bogus <- setdiff(names(args), names(rasterPrepSettings))
    stop("These arguments are not valid rasterPrepSettings: ",
         paste(bogus, collapse = ", "))
  }

  if ("resetLibs" %in% names(args)) {
    a <- args$resetLibs
    if (!is.logical(a) || length(a) != 1 || is.na(a))
      stop("resetLibs should be TRUE or FALSE")
    rasterPrepSettings$resetLibs <- a
  }
  if ("projLib" %in% names(args)) {
    a  <- args$projLib
    if (!is.character(a) || length(a) != 1 || is.na(a))
      stop("projLib should be a character string")
    rasterPrepSettings$projLib <- a
  }
  if ("gdalData" %in% names(args)) {
    a  <- args$gdalData
    if (!is.character(a) || length(a) != 1 || is.na(a))
      stop("gdalData should be a character string")
    rasterPrepSettings$gdalData <- a
  }

  if ("verbose" %in% names(args)) {
    verbose <- args$verbose
    if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose))
      stop("verbose should be TRUE or FALSE")
    rasterPrepSettings$verbose <- verbose
  }

  if ("usesf" %in% names(args)) {
    usesf <- args$usesf
    if (!is.logical(usesf) || length(usesf) != 1 || is.na(usesf))
      stop("usesf should be TRUE or FALSE")
    rasterPrepSettings$usesf <- usesf
  }

  if (length(args) == 0)
    return(as.list(rasterPrepSettings))

}
