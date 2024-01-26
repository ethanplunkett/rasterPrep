test_that("warpToReference works", {

  # setup
  dir <- withr::local_tempdir("warpToReference")
  shp <- system.file("ex/lux.shp", package = "terra")
  ref <- file.path(dir, "ref.tif")
  makeReference(shp, ref, cellsize = 0.009)
  source <- system.file("ex/elev.tif", package = "terra")
  elev <- file.path(dir, "elev.tif")
  elev2 <- file.path(dir, "elev2.tif")

  expect_no_error(warpToReference(source, elev, reference = ref))

  expect_true(file.exists(elev))
  rr <- terra::rast(ref)
  r1 <- terra::rast(elev)
  if(FALSE)
    terra::plot(r1)
  expect_equal(as.vector(terra::ext(rr)),as.vector(terra::ext(r1)))

  # With clip
  warpToReference(elev, elev2, reference = ref, clip = shp)
  r2 <- terra::rast(elev2)

  # verify that there are no values beyond the clip mask
  # as encoded in the reference raster
  all(is.na(terra::values(r2))[is.na(terra::values(rr))])



})

