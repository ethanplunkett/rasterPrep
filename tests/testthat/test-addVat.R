test_that("addVat() works", {

  ###  Setup

  dir <- withr::local_tempdir("addVat")

  # Paths
  original <- system.file("ex/elev.tif", package = "terra")
  classed <- file.path(dir, "elev_class.tif")
  vat <- paste0(classed, ".vat.dbf")

  r0 <- terra::rast(original)
  if (FALSE)
    terra::plot(r0)

  # Classify into 10 height groups not this is now an integer raster
  # but it does not have an internal attribute table.  see cats() and levels()
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

  ### run

  # No table
  expect_no_error(addVat(classed))
  expect_true(file.exists(vat))
  v <- foreign::read.dbf(vat)
  expect_snapshot(v)

  # With attribute table
  at <- data.frame(VALUE = 1:10, a_col = letters[1:10], another = 21:30)

  file.remove(vat)
  expect_no_error(addVat(classed, attributes = at))
  expect_true(file.exists(vat))
  v2 <- foreign::read.dbf(vat, as.is = TRUE)

  expect_equal(cbind(v, at[, -1]), v2, ignore_attr = TRUE)

  # Only one new attribute column (previously it's name was lost)
  at2 <- at[, -3]
  file.remove(vat)
  expect_no_error(addVat(classed, attributes = at2))
  v3 <- foreign::read.dbf(vat, as.is = TRUE)
  expect_equal(names(v3), c("VALUE", "COUNT", names(at)[2]))

})



test_that("addVat() works with prexisting classes", {
  # Issue 11
  # addVat() fails with a TIFF with a pre-existing aux.xml with class names
  #


  dir <- withr::local_tempdir("addVat_classed")

  # Paths

  # A cropped version of the file Brad shared in issue 11 on github:
  original <- system.file("extdata/class_example.tif", package = "rasterPrep")
  tif <- file.path(dir, "class_example.tif")  # test copy path
  vat <- paste0(tif, ".vat.dbf")  # path to vat

  # Make copy in temp dir for testing
  copyTif(original, tif)

  # Test
  expect_no_error(addVat(tif))
  expect_true(file.exists(vat))

  v <- foreign::read.dbf(vat, as.is = TRUE)

  expect_equal(colnames(v), c("VALUE", "COUNT", "class"))
  expect_snapshot(tail(v, 3))

})



