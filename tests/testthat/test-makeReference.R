test_that("makeReference works", {

  dir <- withr::local_tempdir("makeReference")
  shp <- system.file("ex/lux.shp", package = "terra")
  ref <- file.path(dir, "ref.tif")
  expect_no_error(makeReference(shp, destination = ref, cellsize = 0.009))
  expect_true(file.exists(ref))
  info <- sf::gdal_utils(source = ref, quiet = TRUE)

  r <- terra::rast(ref)
  expect_snapshot(terra::ext(r))
  expect_equal(terra::xres(r), 0.009)
  expect_equal(terra::yres(r), 0.009)
  expect_equal(as.numeric(unique(terra::values(r))), c(NA, 1)) # only 1 and NA

  # This tests that the extent values are all integer multiples of the cellsize
  expect_equal(as.numeric(as.vector(terra::ext(r))) %% terra::xres(r),
               rep(0, 4))

  if(FALSE)
    terra::plot(r)
})
