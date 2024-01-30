test_that("addColorTable() works", {

  # Setup
  dir <- withr::local_tempdir("addo")

  # Paths
  original <- system.file("ex/elev.tif", package = "terra")
  classed <- file.path(dir, "elev_class.tif")
  vrt <-  file.path(dir, "elev_class.vrt") # virtual raster with color table

  r0 <- terra::rast(original)
  if (FALSE)
    terra::plot(r0)

  # Classify into 10 height groups
  r1 <- r0
  v <- terra::values(r1)
  class_names <- levels(cut(v, 10))
  class_values <- 1:10
  terra::values(r1) <- cut(v, 10,  labels = FALSE)
  names(r1) <- "class"
  colors <- rev(grDevices::terrain.colors(10))  # based on terra::plot col
  terra::writeRaster(r1, classed, datatype = "INT1U")
  if (FALSE)
    terra::plot(r1)

  # Make color table
  table <- data.frame(value = class_values,
                      color = colors,
                      category = class_names)

  # Add color table
  expect_no_error(addColorTable(classed, table)) # creates vrt
  r2 <- terra::rast(vrt)

  expect_equal(terra::coltab(r1), list(NULL)) # no color table in r1

  expect_snapshot(head(as.data.frame(r1), 4)) # integers class
  expect_snapshot(head(as.data.frame(r2), 4)) # labeled class

  expect_type(terra::coltab(r2), "list") # has color tab
  expect_snapshot(head(terra::coltab(r2)[[1]], 3)) # color values

})
