#' Function to create a .vrt file with a color table
#'
#' This allows adding a color table to a byte encoded categorical raster in
#' which each value is assigned a unique color and, optionally, a category
#' label.
#'
#' The `addColorTable` function creates a .vrt file alongside a raster
#' that references the raster and includes the color table.  .vrt is a GDAL
#' virtual format file that describes a dataset to be composed of other
#' datasets.  The .vrt can be read directly by ESRI GIS software or can be
#' passed to [makeNiceTif()] (or GDAL translate directly) in which
#' case the data and symbology will be united in an output tif.
#'
#' Note: The key will include all values between 0 and the maximum value in the
#' raster.  If your raster doesn't use some values in the sequence the missing
#' values will appear in the key with white as the associated color and an
#' empty string as the category label. Generally it works best if you recode
#' your raster to have values 0 to N-1, or 1 to N.
#
#' My workflow is generally:
#'
#'  \enumerate{
#'  \item `addColorTable()`   create .vrt alongside existing .tif
#'  \item [makeNiceTif()]  called directly on the .vrt to create a .tif
#'  with embedded color table, tiled data, stats, and overviews.
#'  \item optionally call [addVat()] to build a value attribute table.
#'  }
#'
#' @param x (character) the path to raster file containing categorical data,
#'   must be a single band byte encoded file.
#' @param table a  `data.frame`  with two or three columns:
#' \describe{
#'   \item{value}{(numeric) raster cell values.}
#'   \item{color}{(character) the color to assign each value. Typically a
#'      hexadecimal color in #rrggbb format. It will be passed to
#'      [grDevices::col2rgb()] so any format that function accepts will work.}
#'   \item{category}{(optional, character) text describing the class.  If
#'      present this will be used in the map legend.}
#' }
#' @return  this function creates a .vrt file but returns nothing.
#' @export
addColorTable <- function(x, table) {

  verbose <- rasterPrepOptions()$verbose

  stopifnot(c("value", "color") %in% names(table))
  add.categories <- "category" %in% names(table)

  # Check that input file is a single band
  r <- terra::rast(x)
  if (!dim(r)[3] == 1)
    stop("addColorTable only works with byte encoded single band files. ",
         x, " appears to have more than one band.")

  # And that the storage Type is byte
  d <- terra::describe(x)
  d <- d[grep("^Band 1", d)]
  d <- gsub("^.*Type=", "", d)
  d <- gsub("[^[:alpha:]].*$", "", d)
  if (tolower(d) != "byte")
    stop("addColorTable only works with byte encoded files.", x,
         " appears to have a diffent type.")

  # Identify the maximum value in attributes
  max.val <-  max(table$value)
  all.values <- 0:max.val

  # vrt file will be the same as the input tiff but with .vrt extension
  vrt.file <- gsub("\\.tif$", ".vrt", x,  ignore.case = TRUE)

  if (is.factor(table$color)) table$color <- as.character(table$color)

  #----------------------------------------------------------------------------#
  #  Create color table text for insertion in VRT file
  #----------------------------------------------------------------------------#

  # Make color table vrt text
  # First color corresponds to a value of 0
  # subsequent colors to values 1 to the maximum in the grid
  colors <- rep("#FFFFFFFF", length(all.values))  # start with all colors white
  colors[match(table$value, all.values)] <- table$color
  colors <- as.data.frame(t(grDevices::col2rgb(colors)))
  color.table <- paste0('  <Entry c1="', colors[, 1],
                        '" c2="', colors[, 2],
                        '" c3="', colors[, 3],
                        '" c4="255"/>', collapse = "\n  ")
  # Add additional xml
  color.table <-
    paste("<ColorInterp>Palette</ColorInterp>",
          "<ColorTable>",
          color.table,
          "</ColorTable>", collapse = "\n", sep = "\n")

  #----------------------------------------------------------------------------#
  #  Create catagories text for insertion in VRT file
  #----------------------------------------------------------------------------#
  if (add.categories) {
    classes <- rep("", length(all.values))
    classes[match(table$value, all.values)] <- as.character(table$category)

    categories <-
      paste("<CategoryNames>",
            paste("  <Category>", classes, "</Category>
          <!-- ", 0:(length(classes) - 1), " -->",  collapse = "\n", sep = ""),
            "</CategoryNames>", sep = "\n")
  }
  #----------------------------------------------------------------------------#
  # Create and modify the VRT file
  #----------------------------------------------------------------------------#

  # Temporarily reset the PROJ_LIB environmental setting for system
  # call (if indicated by settings)
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

  # Make vrt file to use as templace

  # Gdal translate while assigning projection
  command <- "gdal_translate"
  command <- paste0(command, " ",
                    shQuote(x), " ", shQuote(vrt.file),
                    " -of VRT")
  if (verbose)
    cat("Creating .vrt file with system command:\n", command, "\n")

  a <- system(command = command, intern = TRUE, wait = TRUE)
  a <-  gsub("[[:blank:]]", " ", a)
  if (!file.exists(vrt.file)) {
    print(a)
    stop("The vrt file was not created. The function returned: ", a)
  }

  # Read in template vrt file
  vrt <- readLines(vrt.file)
  vrt <- paste(vrt, collapse = "\n")


  if (add.categories) {
    new.xml <-  paste(color.table, categories, sep = "\n")
  } else {
    new.xml <- color.table
  }

  stopifnot(length(new.xml) == 1)

  # Replace <ColorInterp> tag with new xml code
  vrt <- gsub("<ColorInterp>.*</ColorInterp>", new.xml, vrt)

  # Write vrt file
  writeLines(vrt, vrt.file)

  return(invisible(vrt.file))

}
