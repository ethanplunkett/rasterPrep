#' warp raster data to match a reference raster file.
#'
#' Function to reproject or resample the data in `source` into a new file
#' at `destination` with the same extent, projection, cellsize, and
#' alignment as `reference`, and (optionally) with cells outside the
#' polygons within `clip` set to no data.
#'
#' This is a convenience wrapper to gdalwarp that facilitates making raster data
#' align with other raster data while attempting to set many options to sensible
#' defaults.  [gdalUtilities::gdalwarp()] provides direct access to all
#' the options.
#'
#' Clipping and reprojecting in a single step may result
#' in slight imperfections in the NA footprint in the final grid;
#' some cells along the edge may end up with NA
#' despite being centered within the polygon or vice +versa.
#' I think this is because gdalwarp may be masking based
#' on the center of the source pixels rather than the destination pixels.
#' For that reason if you are both reprojecting/resampling
#' and clipping to a polygon I recommend calling this function twice.
#' In the first call omit the `clip` argument.
#' It will reproject and resample.
#' Then in the second call pass in the output of first and use `clip`.
#'
#' @param source (character) path to a raster file containing data to be warped
#'   (reprojected and or resampled)
#' @param destination (character) path to a .tif file to be created
#' @param reference (character) path to a reference raster that defines the
#'   desired extent, projection, and cellsize.
#' @param clip (optional, character) path to a polygon file (typically a
#'   shapefile) if supplied cells that fall outside of the polygon will be
#'   assigned NA.
#' @param method (Character) the resampling method one of "near", "bilinear",
#'   "cubic", "cubicspline", "lanczos", "average", "mode", "max", "min", "med",
#'   "q1", or "q3" passed to gdalwarp see:
#'   <http://www.gdal.org/gdalwarp.html> for more details.
#' @param overwrite (optional, logical) if TRUE (the default) than a preexisting
#'   destination file be overwritten.  If FALSE it will throw an error if
#'   the file already exists.
#' @param overlay (optional, logical) if TRUE then add the data in source to a
#'   preexisting destination grid in which case reference is ignored.  This can
#'   be used to combine tiled rasters into a single grid (likely with a separate
#'   `clip` file for each tile).
#' @param type (optional, character) if not set than both the data type and the
#'   no data value in the destination file will match the values in the source
#'   file.  If set then the data will be encoded with the selected type and the
#'   no data value will be set to the lowest possible value for signed types and
#'   the highest possible value for unsigned types.
#' @param compression (optional, character) can be set to 'LZW' or 'DEFLATE'
#'   in which case the gdal_warp creation option -co compress=(compression) will
#'   be used if creating a new file.
#' @param bigtiff (optional) defaults to FALSE, can be set to TRUE, 'YES', 'NO',
#'    'IF_NEEDED', or 'IF_SAFER' and is passed to the gdalwarp BIGTIFF
#'    creation option. See \url{https://gdal.org/drivers/raster/gtiff.html.}
#'    TRUE and FALSE are mapped to 'YES' and 'NO' respectively. This argument
#'    is only relevant when creating new files.
#' @return `warpToReference` creates a new raster file at destination
#'   matching the extent and cellsize of reference but returns nothing.
#' @export
warpToReference <- function(source, destination, reference, clip,
                            method = "near", overwrite = TRUE,
                            overlay = FALSE, type, compression,
                            bigtiff = FALSE) {
  verbose <- rasterPrepOptions()$verbose
  usesf <- rasterPrepOptions()$usesf

  if (!missing(compression))
    stopifnot(compression %in% c("LZW", "DEFLATE"))

  reference <- normalizePath(reference, mustWork = TRUE, winslash = "/")
  source <- normalizePath(source, mustWork = TRUE, winslash = "/")
  destination <- normalizePath(destination, mustWork = FALSE, winslash = "/")

  if (!file.exists(reference))
    stop("reference file ", reference, " doesn't exist.")
  if (!file.exists(source))
    stop("source file ", source, " doesn't exist")

  # terra based code:
  src.info <- terra::rast(source)
  src.proj <- terra::crs(src.info)
  if (is.na(src.proj) || src.proj == "") {
    stop("Source file must have a defined projection")
  }

  if (!missing(clip)) {
    clip <- normalizePath(clip, mustWork = TRUE, winslash = "/")
    if (!file.exists(clip))
      stop("clip file ", clip, " is missing.")
  }

  ref.info <- terra::rast(reference)
  ref.proj <- terra::crs(ref.info)
  wkt.file <- tempfile(fileext = ".txt")
  cat(ref.proj,  file = wkt.file, append = FALSE)

  if (is.na(ref.proj) || ref.proj == "") {
    "Stop reference file must have a defined projection"
  }

  # Note gdalwarp allows setting the NA value and a new type
  # but it does NOT reclass existing NA values to the new NA value
  # If the user specifies a new type we are going to create a .vrt file
  # with gdalTranslate
  type.specified <- !missing(type)
  ref.ext <- terra::ext(ref.info)
  ref.xll <- as.numeric(ref.ext$xmin)
  ref.yll <- as.numeric(ref.ext$ymin)
  ref.resx <- terra::xres(ref.info)
  ref.resy <- terra::yres(ref.info)
  ref.xmax <- as.numeric(ref.ext$xmax)
  ref.ymax <- as.numeric(ref.ext$ymax)
  rm(ref.ext)

  # Note gdalwarp uses "near" while gdaladdo uses "nearest"
  # this allows this function to use either
  if (method == "nearest")
    method <- "near"
  valid.methods <- c("near", "bilinear", "cubic", "cubicspline", "lanczos",
                     "average", "mode", "max", "min",
                     "med", "q1", "q3")

  if (!method %in% valid.methods)
    stop("resampling method '", method, "' isn't recognized", "use one of '",
         paste(valid.methods, collapse = "' '"), "'", sep = "")

  # validate big tiff argument
  if (is.logical(bigtiff)) {
    if (!bigtiff %in% c(TRUE, FALSE)) {
      stop("bigtiff must be TRUE, FALSE, IF_NEEDED', 'IF_SAFER', 'YES', 'NO'")
    }
    bigtiff <- ifelse(bigtiff, "YES", "NO")
  }
  bigtiff <- toupper(bigtiff)
  stopifnot(bigtiff %in% c("IF_NEEDED", "IF_SAFER", "YES", "NO"))

  is.signed.byte <- FALSE
  if (type.specified) {
    a <- assessType(type)
    no.data.value <- a$noDataValue
    type <- a$type
    is.signed.byte <- a$isSignedByte
  }

  if (usesf) {

    opts <- character(0)
    if (!overlay) {

      # New file options - don't use if writing to existing dataset

      opts <- c(opts,
                "-co", "TFW=YES",
                "-t_srs", wkt.file)

      if (overwrite)
        opts <- c(opts, "-overwrite")

      opts <- c(opts,
                "-te", ref.xll,  ref.yll, ref.xmax, ref.ymax,
                "-tr", ref.resx, ref.resy)

      if (is.signed.byte)
        opts <- c(opts, "-co", "PIXELTYPE=SIGNEDBYTE")

      if (type.specified)
        opts <- c(opts, "-ot", type, "-dstnodata", no.data.value)

      if (!missing(compression))
        opts <- c(opts, "-co", paste0("compress=", compression))

      opts <- c(opts, "-co", paste0("BIGTIFF=", bigtiff))

    } # end  new file options:  if(!overlay)

    if (!missing(clip))
      opts <- c(opts, "-cutline", clip, " ")

    opts <- c(opts, "-r", method)

    args <- list(util = "warp",
                 source = source,
                 destination = destination,
                 options = opts)

    do.call(sf::gdal_utils, args = args)

  } else {

    # Assemble command for shell uage:

    command <- paste0(shQuote("gdalwarp"), " ")
    if (!overlay) {

      # These arguments only needed if not adding to an existing file

      command <- paste0(command,
                        "-co TFW=YES ",
                        "-t_srs ", shQuote(wkt.file), " ",
                        ifelse(overwrite, " -overwrite ", ""),
                        "-te ", ref.xll, " ", ref.yll, " ",
                        ref.xmax, " ", ref.ymax, " ",
                        "-tr ", ref.resx, " ", ref.resy, " ")
      if (is.signed.byte)
        command <- paste0(command,
                          "-co  PIXELTYPE=SIGNEDBYTE ")
      if (type.specified)
        command <- paste0(command,
                          "-ot ", type, " ",
                          "-dstnodata ", no.data.value, " ")
      if (!missing(compression))
        command <- paste0(command,
                          "-co compress=", compression, " ")

      command <- paste0(command, "-co BIGTIFF=", bigtiff, " ")

    }


    command <- paste0(command,
                      ifelse(missing(clip), "",
                             paste0("-cutline ", shQuote(clip), " ")),
                      "-r ", method, " ",
                      shQuote(source), " ", shQuote(destination))

    # Temporarily reset the PROJ_LIB environmental setting for system call
    # (if indicated by settings)
    oprojlib <- Sys.getenv("PROJ_LIB")
    ogdaldata <- Sys.getenv("GDAL_DATA")
    if (rasterPrepSettings$resetLibs) {
      Sys.setenv(PROJ_LIB = rasterPrepSettings$projLib)
      Sys.setenv(GDAL_DATA = rasterPrepSettings$gdalData)
      on.exit({
        Sys.setenv(PROJ_LIB = oprojlib)
        Sys.setenv(GDAL_DATA = ogdaldata)
      })
    }

    if (verbose)
      cat("Warping with system command:\n", command, "\n")
    a <- system(command = command, intern = TRUE, wait = TRUE)
    a <-  gsub("[[:blank:]]", " ", a)
    if (!grepl("- done.[[:blank:]]*$", a[length(a)])) {
      stop("An error might have occured.  The function returned: ", a)
    }
  } # end use shell commands   (!usesf)

  if (!file.exists(destination))
    stop("Output file", destination,
         "was not created. System call returned: ", a)

  if (terra::crs(terra::rast(destination)) == "")
    stop("Output was created but lacks a coordinate reference system")

  if (verbose)
    cat("Done warping output at:", destination, "\n")
  file.remove(wkt.file)

}
