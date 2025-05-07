## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval = FALSE----------------------------------------------------
# if (!require("remotes"))
#   install.packages("remotes")
# library("remotes")
# remotes::install_github("ethanplunkett/rasterPrep")

## ----echo = TRUE, message = FALSE, warning = FALSE----------------------------
library(rasterPrep)
library(gdalUtilities)  # for ogr2ogr() used to reproject vector Shapefiles
library(terra)  # for crs() and rast()

## -----------------------------------------------------------------------------
input <- list(
  bound = system.file("extdata", "Amherst.shp", package = "rasterPrep"),
  slope = system.file("extdata", "slope.tif", package = "rasterPrep"),
  roads =  system.file("extdata", "roads.shp", package = "rasterPrep"),
  key = system.file("extdata", "roadClassKey.csv", package = "rasterPrep")
)

## -----------------------------------------------------------------------------
outDir <- file.path(tempdir(), "rasterPrepDemo")
dir.create(outDir, showWarnings = FALSE)

out <-
  list(ref = file.path(outDir, "reference.tif"),        # reference
       slopeTemp = file.path(outDir, "slopeTemp.tif"),  # Intermediate slope
       slope = file.path(outDir, "slope.tif"),          # Slope
       roadLines = file.path(outDir, "roadLines.shp"),  # Transformed roads
       roadsTemp = file.path(outDir, "roads1.tif"),     # Intermediate roads
       roads  = file.path(outDir, "roads.tif"),         # Binary Roads
       roadClass  = file.path(outDir, "roadClass.tif")) # Road class

## ----echo = FALSE, eval = TRUE------------------------------------------------
# Extra cleanup for vignette, to cleanup old output when rerunning.
# This deletes the entire contents of outDir!  Do not add to your workflow.
unlink(outDir, recursive = TRUE)
dir.create(outDir, showWarnings = FALSE)

## ----echo = TRUE, message = FALSE, warning = FALSE, results = FALSE-----------
makeReference(polyFile = input$bound, destination = out$ref, cellsize = 30)

## ----echo = TRUE, message = FALSE, warning = FALSE, results = FALSE-----------
warpToReference(input$slope, out$slopeTemp, reference = out$ref,
                method = "bilinear")

## ----clip slope, echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----
warpToReference(out$slopeTemp, out$slope, reference =  out$ref,
                clip = input$bound)


## ----delete slopeTemp---------------------------------------------------------
# Delete intermediate file
deleteTif(out$slopeTemp)

## ----visualize slope----------------------------------------------------------
slope <- rast(out$slope)
plot(slope)


## ----echo = TRUE, message = FALSE, warning = FALSE, results = FALSE-----------
refProj <- terra::crs(terra::rast(out$ref))  # well known text of projection
projFile <- file.path(outDir, "refproj.txt")
cat(refProj, file = projFile)   # well known text in file
gdalUtilities::ogr2ogr(input$roads, out$roadLines, t_srs = projFile)

## ----echo = TRUE, message = FALSE, warning = FALSE, results = FALSE-----------
rasterizeToReference(out$roadLines, destination = out$roadClass,
                     reference = out$ref, attribute = "ROADCLASS",
                     type = "byte")

## ----echo = TRUE, message = FALSE, warning = FALSE, results = FALSE-----------
rasterizeToReference(input$bound, out$roadsTemp, reference = out$ref,
                     burn = 0)

## -----------------------------------------------------------------------------
rasterizeToReference(out$roadLines, destination = out$roadsTemp,
                     allTouched = TRUE, burn = 1)

## -----------------------------------------------------------------------------
warpToReference(out$roadsTemp, out$roads, reference = out$ref,
                clip = input$bound)

## -----------------------------------------------------------------------------
deleteTif(out$roadsTemp)
roads <- rast(out$roads)
plot(roads)

## ----final setup--------------------------------------------------------------
finalDir <- file.path(outDir, "final")
dir.create(finalDir, showWarnings = FALSE)
final <- list()
final$slope <- file.path(finalDir,  "slope.tif")
final$roads <- file.path(finalDir,  "roads.tif")
final$roadclass <- file.path(finalDir, "roadclass.tif")

## ----results= FALSE-----------------------------------------------------------
makeNiceTif(out$slope, final$slope, overviewResample = "average")

## -----------------------------------------------------------------------------
makeNiceTif(source = out$roads, destination = final$roads,
            overwrite = TRUE, overviewResample = "near", stats = FALSE,
            vat = TRUE)

## -----------------------------------------------------------------------------
key <- read.csv(input$key, stringsAsFactors = FALSE)
print(key)

## ----results = FALSE----------------------------------------------------------
vrt.file <- addColorTable(out$roadClass, table = key) # returns path to file

## -----------------------------------------------------------------------------
makeNiceTif(source = vrt.file, destination = final$roadclass,
            overwrite = TRUE, stats = TRUE)

## -----------------------------------------------------------------------------
addVat(x = final$roadclass, attributes = key)

