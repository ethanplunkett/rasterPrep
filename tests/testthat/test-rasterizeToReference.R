test_that("rasterizeToReference  works", {

  # setup
  dir <- withr::local_tempdir("rasterizeToReference")
  shp <- system.file("ex/lux.shp", package = "terra")
  ref <- file.path(dir, "ref.tif")
  makeReference(shp, ref, cellsize = 0.009)

  regions <- file.path(dir, "regions.tif")
  regions2 <- file.path(dir, "regions2.tif")
  regions3 <- file.path(dir, "regions3.tif")

  # ID_2 is an integer column with unique values for each polygon
  #  (uniqueness isn't required)
  expect_no_error(
    rasterizeToReference(shp, regions, reference = ref, attribute = "ID_2")
  )

  expect_true(file.exists(regions))

  rr <- terra::rast(ref)
  r <- terra::rast(regions)

  if (FALSE) {
    terra::plot(r)
    p <- sf::st_read(shp, quiet = TRUE)
    plot(p[, 1], add = TRUE, col = NA)

  }

  expect_snapshot(table(terra::values(r)))

  # Matching extent and resolution
  expect_equal(as.vector(terra::ext(rr)), as.vector(terra::ext(r)))
  expect_equal(as.vector(terra::res(rr)), as.vector(terra::res(r)))

  # With burn all object receive same value
  expect_no_error(
    rasterizeToReference(shp, regions2, reference = ref,
                         burn = 1)
  )
  expect_true(file.exists(regions2))

  r2 <- terra::rast(regions2)
  if (FALSE)
    terra::plot(r2)
  # Raster has only one value
  expect_equal(as.numeric(table(terra::values(r2))), 3953)

  # Both output rasters have the same number of cells with values
  expect_equal(sum(table(terra::values(r))), sum(table(terra::values(r2))))

  # Meaningful error when both burn and attribute are used
  expect_error(rasterizeToReference(shp, regions2, reference = ref,
                                    burn = 1, attribute = "ID_2"),
               "use burn or attribute, not both")

  # Meaningful error when neither burn nor attribute is used
  expect_error(rasterizeToReference(shp, regions2, reference = ref),
               "specify either burn or attribute ")


  # With init
  expect_no_error(rasterizeToReference(shp, regions3, reference = ref,
                                       init = 0, burn = 1))
  with_init <- terra::rast(regions3)
  expect_snapshot(
    table(terra::values(with_init))
  )
})
