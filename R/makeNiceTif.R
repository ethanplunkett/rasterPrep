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
#'  ## No Data values and changes in type
#'
#' `type` is currently passed to gdal_translate which will reset the
#'  type to the desired value but will **NOT** update no data values of cells.
#'  One of two things will happen. If the existing no data value is within the
#'  values supported by the output type they will retain that value. Otherwise
#'  they will be assigned the closest supported value in the output type.
#'
#'  Thus changing type may still work out if you (1) are not converting between
#'   signed and unsigned types and (2) are moving to a smaller bit depth.
#'  `makeNiceTif` will attempt to select a new no data value that works out.
#'   For example moving from an Int32 to and Int16 will probably work if the
#'   initial NA value is high because it will will be truncated to the highest
#'    value in the Int16 and which is what the new no data value will be set to.
#'    The default `noDataValue` of the output when using `type` and not
#'    specifying the `noDataValue` is determined by \code{\link{assessType}}.
#'
#'  If you use `type` and your output raster ends up with values cells that were
#'  originally no data you might be able to remedy this by setting
#'  `noDataValue` to that value.
#'
#'  If you want to change the type you can do so safely in a call to
#'  warpToReference prior to using this function as gdalwarp can reset the type
#'  and reset the values of the NA cell to the new noDataValue.
#'
#'
#' This is a wrapper to the `gdaltranslate` command line utility coupled with
#' [addOverviews()] and [addVat()].  For finer control over
#' translation see [gdalUtilities::gdal_translate()].
#'
#' If you intend to create a color table for a categorical raster first call
#' [addColorTable()] and then pass the .vrt file it creates to this function.
#
#' @param source (character) path to a raster file readable by gdal.
#' @param destination (character) path to a .tif file to be created for viewing
#'  with GIS software
#' @param type (character) If the `type` is used the output will be converted
#' to it. It should be one of `"Byte"`, `"UInt16"`, `"Int16"`,
#'   `"UInt32"`, `"Int32"`, `"Float32"`, `"Float64"` or for convenience you may
#' also use the raster package's designations: [raster::dataType()]. See
#' cautionary section below on no data problems while reassigning type.
#' @param overwrite (logical) if `TRUE` any existing file will be overwritten
#' @param buildOverviews (logical) if `TRUE` overviews (AKA pyramids) will be
#' built
#' @param overviewResample (character) one of  `"nearest"`,
#'  `"average"`, `"gauss"`, `"cubic"`, `"cubicspline"`,
#'  `"lanczos"`, `"average_mp"`, `"average_magphase"`, `"mode"` see
#'   [gdaladdo](https://www.gdal.org/gdaladdo.html) for details.
#'   For convenience `"near"` is silently updated to `"nearest"`
#' @param vat (logical) if `TRUE` an ESRI Value Attribute Table (VAT) sidecar
#'  file will be created containing all the unique values in the grid and
#'  their associated count of cells. This is only recommended for categorical
#'  data and can be slow but will speed up setting up symbology of that data
#'   in ArcGIS.
#' @param stats (logical) if `TRUE` than statistics are generated and saved;
#'    this helps GIS software transform continuous data
#'    (e.g. make a standard deviation color ramp)
#' @param noDataValue Only used if `type` is not missing. Setting this affects
#'   the no data value set in the output TIFF but does not reassign existing no
#'   data cell values to it. If the existing no data value is not in the range
#'   of values supported by `type` then they will end up with the value in range
#'   that is closest to the existing value. If this argument isn't supplied and
#'   `type` is used `noDataValue`` will be assigned based on the output of
#'   [assessType()] which often but not always picks an appropriate
#'   value.
#' @return This function creates a copy of the source raster at the destination
#' path that is formatted to facilitate viewing in GIS software. It does not
#' return anything.
#'
#' @export
makeNiceTif <- function(source, destination, type, overwrite = FALSE,
                        buildOverviews = TRUE, overviewResample = "nearest",
                        vat = FALSE, stats = TRUE, noDataValue) {

  verbose <- rasterPrepOptions()$verbose
  if (!file.exists(source)) stop("input file", source, "is missing.")
  if (file.exists(destination)) {
    if (overwrite) {
      deleteTif(destination)
    } else {
      stop("destination file already exists: ", destination)
    }
  }

  has.type <- FALSE
  is.signed.byte <- FALSE
  usesf <- rasterPrepOptions()$usesf

  if (!missing(type)) {
    if (verbose)
      cat("Type conversion will work in some cases but in others will ",
          "not properly conserve NA encoding. Use with caution.\n", sep = "")
    has.type <- TRUE
    a <- assessType(type)
    type <- a$type
    is.signed.byte <- a$isSignedByte
    if (missing(noDataValue)) {
      noDataValue <- a$noDataValue
    }
    stopifnot(is.numeric(noDataValue),
              !is.na(noDataValue),
              length(noDataValue) == 1)
  } else { # missing type
    if (!missing(noDataValue))
      warning("noDataValue will be ignored. Set type to use noDataValue.")
  }

  if (usesf) {

    opts <- character(0)
    opts <- c(opts,
              "-stats",
              "-co", "compress=LZW",
              "-co", "TFW=YES",
              "-co", "TILED=YES")
    if (has.type)
      opts <- c(opts,
                "-ot", type,
                " -a_nodata", noDataValue)
    if (is.signed.byte)
      opts <- c(opts, "-co", "PIXELTYPE=SIGNEDBYTE")
    if (stats)
      opts <- c(opts, "-stats")

    args <- list(util = "translate",
                 source = source,
                 destination = destination,
                 options = opts)

    if (verbose) {
      cat("Calling sf::gdal_utils with arguments:\n")
      print(utils::str(args))
    }

    do.call(sf::gdal_utils, args = args)

  } else {
    # Execute with shell commands

    qc <- '"'
    command <- paste0("gdal_translate -stats -co compress=LZW -co ",
                      "TFW=YES -co TILED=YES ",
                      qc, source, qc, " ", qc, destination, qc, " ")

    if (has.type)
      command <- paste0(command, " -ot ", type, " ",
                        " -a_nodata ", noDataValue, " ")

    if (is.signed.byte)
      command <- paste0(command,
                        " -co  PIXELTYPE=SIGNEDBYTE ")

    if (stats)
      command <- paste0(command, "-stats ")

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
      cat("Compressing with system command:\n", command, "\n")
    a <- system(command = command, intern = TRUE, wait = TRUE)

  } # end  use shell command (!usesf)

  if (!file.exists(destination))
    stop("Output file", destination,
         "was not created. System call returned: ", a)

  if (buildOverviews) {
    addOverviews(x = destination, method = overviewResample)
  }

  if (vat) {
    if (verbose)
      cat("building vat")
    addVat(destination)
  }

  if (stats) {
    if (usesf) {
      # Calling for side effect of updating stats and historgrams
      a <- sf::gdal_utils(util = "info", source = destination,
                          options = c("-stats", "-hist"),
                          quiet = TRUE)

    } else {
      # Make a call to gdalinfo to add both stats and a histogram
      # the histogram isn't added by gdal_translate even if -stats flag is set
      command <- paste0("gdalinfo -stats -hist ",
                        shQuote(destination))
      b <- system(command = command, intern = TRUE, wait = TRUE)
      rm(b)
    }
  }
}
