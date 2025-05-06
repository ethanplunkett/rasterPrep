
test_that("rasterPrep() addOverviews() works", {
  ri <- function(x) sf::gdal_utils(source = x)

  # This returns lines from gdal_info that meet the pattern
  getRastInfo <- function(x, pattern = "overviews") {
    info <- sf::gdal_utils(source = x, quiet = TRUE) |> strsplit("\n")
    info <- info[[1]]
    sv <- grepl(pattern, info, ignore.case = TRUE)
    return(info[sv])
  }

  dir <- withr::local_tempdir("addo")
  f <- system.file("ex/elev.tif", package = "terra")
  f2 <- file.path(dir, "elev.tif")
  ovf <- file.path(dir, "elev.tif.ovr")

  # Add overviews with default arguments
  file.copy(f, f2)
  addOverviews(f2)

  expect_length(getRastInfo(f),  0)
  expect_length(getRastInfo(f2), 1)

  expect_true(grepl("LZW", getRastInfo(ovf, "compression")))

  # Read in overview
  expect_warning(nearover <- terra::rast(ovf))
  nearsamp <- nearover[10, 10:18]

  if (FALSE)
    terra::plot(nearover)

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

  expect_warning(averageover <- terra::rast(ovf))
  avgrsamp <- averageover[10, 10:18]

  if (FALSE)
    terra::plot(averageover)

  # Compare a selection of pixels from the nearest neighbor and average
  # overviews
  comparison <- cbind(nearsamp, avgrsamp)
  names(comparison) <- c("nearest", "average")
  expect_snapshot(comparison)

})
