test_that("sf::gdal_addo() works", {

  # just experimenting

  dir <- withr::local_tempdir("addo")
  f <- system.file("ex/elev.tif", package="terra")
  f2 <- file.path(dir, "elev.tif")
  file.copy(f, f2)

  sf::gdal_addo(f2)

  i1 <- sf::gdal_utils(util = "info" , source = f, quiet = TRUE)
  i2 <- sf::gdal_utils(util = "info", source = f2, quiet = TRUE)
  expect_false(grepl("overviews", i1, ignore.case = TRUE))
  expect_true(grepl("overviews", i2, ignore.case = TRUE))
})

test_that("rasterPrep() addOverviews() works", {

  getRastInfo <- function(x, pattern = "overviews"){
    info <- sf::gdal_utils(source = x, quiet = TRUE) |> strsplit("\n")
    info <- info[[1]]
    sv <- grepl(pattern, info, ignore.case = TRUE)
    return(info[sv])
  }

  dir <- withr::local_tempdir("addo")
  f <- system.file("ex/elev.tif", package="terra")
  f2 <- file.path(dir, "elev.tif")
  ovf <- file.path(dir, "elev.tif.ovr")

  # Add overviews with default arguments
  file.copy(f, f2)
  addOverviews(f2)

  expect_length(getRastInfo(f),  0)
  expect_length(getRastInfo(f2), 1)
  expect_true(grepl("LZW", getRastInfo(ovf, "compression")))

  # Read in overview
  nearover <- terra::rast(ovf)
  nearsamp <- nearover[10, 10:18]

  if(interactive())
     terra::plot(nearover)
  expect_snapshot(nearover[10, 10:20])

  # remove overviews
  addOverviews(f2, clean = TRUE)
  expect_true(length(getRastInfo(f2)) == 0)

  # Alternate compression JPEG
  addOverviews(f2, compression = "JPEG")
  expect_length(getRastInfo(f2), 1)
  expect_true(grepl("JPEG", getRastInfo(ovf, "compression")))

  # remove overviews
  addOverviews(f2, clean = TRUE)
  expect_true(length(getRastInfo(f2)) == 0)

  # Alternate compression DEFLATE
  addOverviews(f2, compression = "DEFLATE")
  expect_length(getRastInfo(f2), 1)
  expect_true(grepl("DEFLATE", getRastInfo(ovf, "compression")))

  # remove overviews
  addOverviews(f2, clean = TRUE)
  expect_true(length(getRastInfo(f2)) == 0)

  # Alternate method "average"
  addOverviews(f2, method = "average")
  expect_length(getRastInfo(f2), 1)

  averageover <- terra::rast(ovf)
  avgrsamp <- nearover[10, 10:18]

   if(interactive())
    terra::plot(averageover)

  comparison <- cbind(nearsamp, avgrsamp)
  names(comparison) <- c("nearest", "average")
  expect_snapshot(comparison)

  sf::gdal_utils(source =ovf)

})
