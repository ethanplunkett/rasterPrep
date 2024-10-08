#' Rasterize vector data
#'
#' This function rasterizes vector data into a file that coincides
#' (same extent, cellsize, projection, and cell alignment)
#' with a reference raster file.
#'
#' If the file at `destination` exists the new pixels will be overlaid on
#' existing data such that existing data will persist where it does not underlie
#' the vector data in `source`.
#'
#' Either the `burn` or `attribute` argument should be used to set the
#' value assigned to cells.  The default type (`"Byte"`) supports integers 0 to
#' 255.  If you are writing real numbers or outside
#' of that range be sure to set `type` to something appropriate.  Similarly,
#' if you are only writing a few values you may want to set `type` to something
#' that supports fewer values such as `"Byte"`.
#'
#' When creating a new file
#' the CRS of the source must match that of the reference and this used to be
#' checked with [raster::compareCRS()] but with updates to the PROJ library and
#' the switch to WKT representations I don't currently have a good way to check
#' if two projections are equivalent. Its now up to the user to make sure that
#' they are.
#'
#' In contrast if you are overlaying data on an existing file then the
#' underlying gdal_rasterize function will reproject while rasterizing.
#'
#' @param source (character) path to a vector GIS file (such as a shapefile)
#' that contains data to be rasterized.
#' @param destination (character) path to a `.tif` file that will either be
#' created or added to.
#' @param reference (character) path to a raster dataset
#' that defines the extent, cellsize, and projection of `destination`
#' if `destination` does not already exist.
#' @param burn (optional, numeric) value to be burnt into grid
#' (everywhere the `source` has data)
#' @param attribute (optional, character) the column name in the source to
#' extract values from
#' @param init (optional, numeric) if supplied this value will be written into
#' the grid as a background value prior to rasterizing.
#' `init` can only be used if the destination does not already exist.
#' @param type the type of grid to create.
#' Should be one of `"Byte"`, `"UInt16"`, `"Int16"`, `"UInt32"`, `"Int32"`,
#' `"Float32"`,`"Float64"`
#' or for convenience you may also use the **terra** package's
#' designations.
#' @param allTouched (optional, logical) defaults to `FALSE`.  If `TRUE`
#'   "all pixels touched by lines or polygons will be updated, not just those on
#'   the line render path, or whose center point is within the polygon"
#'   see "-at" flag description for
#'   [gdal_rasterize](http://www.gdal.org/gdal_rasterize.html)
#' @param sql (optional, character) An SQL statement to be applied to the data
#'   source.  This can be used to filter or sort the data prior to burning.
#' @return
#' This function creates a new raster or writes values to an existing raster
#'  at the destination.  It does not return anything.
#' @export
rasterizeToReference <- function(source, destination, reference, burn,
                                 attribute,  init, type = "Byte",
                                 allTouched = FALSE, sql) {

  verbose <- rasterPrepOptions()$verbose

  if (missing(burn) && missing(attribute))
    stop("You must specify either burn or attribute for rasterization to work.")

  if (!missing(burn) && !missing(attribute))
    stop("You should use burn or attribute, not both.")

  a <- assessType(type)
  type <- a$type
  no.data.value <- a$noDataValue
  if (type == "SignedByte")
    stop("SignedByte isn't currently supported by rasterizeToReference")

  if (!file.exists(source)) stop("source file ", source, " doesn't exist")

  if (grep("\\.shp$", source)) {
    if (!file.exists(gsub("\\.shp$", ".prj", source)))
      stop("Source file is missing projection information.")
  }

  dest.exists <- file.exists(destination)

  if (!dest.exists) {
    if (!file.exists(reference))
      stop("reference file ", reference, " doesn't exist.")

    ref.info <- terra::rast(reference)
    ref.proj <- terra::crs(ref.info)
    if (is.na(ref.proj) || ref.proj == "") {
      "Stop reference file must have a defined projection"
    }

    ref.ext <- terra::ext(ref.info)
    ref.xll <- as.numeric(ref.ext$xmin)
    ref.yll <- as.numeric(ref.ext$ymin)
    ref.resx <- terra::xres(ref.info)
    ref.resy <- terra::yres(ref.info)
    ref.xmax <- as.numeric(ref.ext$xmax)
    ref.ymax <- as.numeric(ref.ext$ymax)
    rm(ref.ext)

    stopifnot(grepl(".tif$", destination, ignore.case = TRUE))
  }

  usesf <- rasterPrepOptions()$usesf # use sf package for gdal utilities

  if (usesf) {
    args <- list(util = "rasterize",
                 source = source,
                 destination = destination,
                 options = character(0))

    if (!dest.exists) {
      # Note if the destination file exists this function will update it
      # (as long as overwrite = FALSE)
      args$options <- c(args$options,
                        "-te", ref.xll, ref.yll, ref.xmax, ref.ymax,
                        "-tr", ref.resx, ref.resy)
      if (!missing(type))
        args$options <- c(args$options, "-ot", type)

      args$options <- c(args$options, "-a_nodata", no.data.value)
    }

    if (!missing(burn))
      args$options <- c(args$options, "-burn", burn)

    if (!missing(attribute))
      args$options <- c(args$options, "-a", attribute)

    if (!missing(init)) {
      if (dest.exists)
        stop("The init argument can only be used when the destination file",
             "does not already exist")
      args$options <- c(args$options, "-init", init)
    }

    if (!missing(sql)) {
      args$options <- c(args$options, "-sql", sql)
    }

    if (allTouched)
      args$options <- c(args$options, "-at")

    if (verbose) {
      cat("Preparing to call sf::gdal_utils with arguments:\n")
      print(args)
    }

    do.call(sf::gdal_utils, args = args)


  } else { #  use shell commands

    command <- paste0(shQuote("gdal_rasterize"), " ")
    if (!dest.exists) {
      # Note if the destination file exists this function will update it
      # (as long as overwrite = FALSE)
      command <- paste0(command,
                        "-te ", ref.xll, " ", ref.yll, " ",
                        ref.xmax, " ", ref.ymax, " ",
                        "-tr ", ref.resx, " ", ref.resy, " ",
                        ifelse(missing(type), "", paste0("-ot ", type, " ")),
                        "-a_nodata ", no.data.value, " ")
    }
    if (!missing(burn))
      command <- paste0(command, "-burn ", burn, " ")

    if (!missing(attribute))
      command <- paste0(command, "-a ", shQuote(attribute), " ")

    if (!missing(init)) {
      if (dest.exists)
        stop("The init argument can only be used when the destination file ",
             "does not already exist")
      command <- paste0(command, "-init ", init, " ")
    }

    if (!missing(sql)) {
      command <- paste0(command, "-sql ", shQuote(sql), " ")
    }

    if (allTouched)
      command <- paste0(command, "-at ")

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
    command <- paste0(command, shQuote(source), " ")
    command <- paste0(command, shQuote(destination))

    if (verbose)
      cat("Rasterizing with:\n", command, "\n")
    a <- system(command = command, intern = TRUE, wait = TRUE)


    a <-  gsub("[[:blank:]]", " ", a)
    if (!grepl("- done.[[:blank:]]*$", a[length(a)])) {
      stop("An error might have occured.  The function returned: ", a)
    }
  }  # end use shell commands (!usesf)

  if (!file.exists(destination))
    stop("Output file", destination,
         "was not created. System call returned: ", a)

  if (terra::crs(terra::rast(destination)) == "")
    stop("Output was created but lacks a coordinate reference system")
}
