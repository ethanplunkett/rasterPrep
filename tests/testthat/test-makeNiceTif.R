test_that("makeNiceTif works", {

  # setup
  dir <- withr::local_tempdir("makeNiceTif")
  source <- system.file("ex/elev.tif", package = "terra")
  tif <- file.path(dir, "elev.tif")
  ovr <- paste0(tif, ".ovr") # overview side car file
  aux <- paste0(tif, ".aux.xml") # overview side car file


  expect_no_error(makeNiceTif(source, destination = tif,
                              buildOverviews = TRUE,
                              overviewResample = "average",
                              vat = FALSE,
                              stats = TRUE))

  info <- sf::gdal_utils(source = tif, quiet = TRUE)
  info <- strsplit(info, "\n")[[1]]
  expect_true(any(grepl("overview", info, ignore.case = TRUE)))
  block <- info[grep("Block=", info, ignore.case = TRUE)]

  # Expect blocks to have two dimensions that are each at least two digits
  # (Tiled not striped)
  expect_true(grepl("[[:digit:]]{2,}x[[:digit:]]{2,}", block, perl = TRUE))
  expect_true(any(grepl("compression", info, ignore.case = TRUE)))

  # Overviews
  expect_true(file.exists(ovr))
  expect_true(grepl("compression", sf::gdal_utils(source = ovr, quiet = TRUE),
                    ignore.case = TRUE))

  if(FALSE){
     a <- sf::gdal_utils(util = "info", source = tif,
                    options = c("-stats", "-hist"), quiet = TRUE)
  }

  # aux.xml  (histogram)
  expect_true(file.exists(aux))
  l <- readLines(aux)
  expect_true(any(grepl("Histograms", l, ignore.case = TRUE)))

  # Make sure crs is retained
  expect_equal(sf::gdal_crs(tif), sf::gdal_crs(source))


})
