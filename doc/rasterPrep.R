## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, message = FALSE, warning = FALSE---------------------------
# Load packages
library(rasterPrep)
library(gdalUtilities)  # for ogr2ogr() used to reproject vector Shapefiles
library(terra)  # for crs() and rast() 

# Note because I'm pulling data from one read-only directory and writing to a
# a second I'm defining complete paths here.  Often it's easier to set a working
# directory then use file names when calling the functions.

# Set paths for retrieving input data included with this package
inPaths <- list(
  bound = system.file("extdata","Amherst.shp" , package = "rasterPrep"),
  slope = system.file("extdata","slope.tif" , package = "rasterPrep"), 
  roads =  system.file("extdata","roads.shp" , package = "rasterPrep"),
  key = system.file("extdata","roadClassKey.csv" , package = "rasterPrep")
)

# Output directory: you might want to replace this with a path of your choosing:
outDir <- tempdir() 

# Set output paths 
outPaths <- list(
  ref = file.path(outDir, "reference.tif"),        # Our reference grid
  slopeTemp = file.path(outDir, "slopeTemp.tif"),  # Intermediate raster file
  slope = file.path(outDir, "slope.tif"),          # Slope raster
  roadLines = file.path(outDir, "roadLines.shp"), # Reprojected vector roads
  roadsTemp = file.path(outDir, "roads1.tif"),     # Intermediate raster file
  roads  = file.path(outDir, "roads.tif"),         # Binary Roads raster
  roadClass  = file.path(outDir, "roadClass.tif") )# Road class raster

# Delete output so rerunning doesn't cause problems
if(file.exists(outPaths$roadLines))
  file.remove(list.files(outDir, "roadLines", full.names = TRUE))

for(a in setdiff(names(outPaths), "roadLines")){
  if(file.exists(outPaths[[a]]))
    deleteTif(outPaths[[a]])
}



## ---- echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----------

makeReference(polyFile = inPaths$bound, destination = outPaths$ref, cellsize = 30)


## ---- echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----------

# Reproject to final extent, projection and alignment
warpToReference(inPaths$slope, outPaths$slopeTemp, reference = outPaths$ref, method = "bilinear")

# Assign NA outside of boundary
warpToReference(outPaths$slopeTemp, outPaths$slope, reference =  outPaths$ref, clip = inPaths$bound)

# Delete intermediate file
deleteTif(outPaths$slopeTemp)


## ---- echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----------

# Reproject vector to reference projection
refProj <- terra::crs(terra::rast(outPaths$ref))  #  well known text of projection
projFile <- file.path(outDir, "refproj.txt")
cat(refProj, file = projFile)   #well known text in file
gdalUtilities::ogr2ogr(inPaths$roads, outPaths$roadLines, t_srs = projFile)


## ---- echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----------
rasterizeToReference(outPaths$roadLines, destination = outPaths$roadClass, reference = outPaths$ref, attribute = "ROADCLASS", type = "byte")


## ---- echo = TRUE, message = FALSE, warning = FALSE, results = FALSE----------

# Burn in zero everywhere within the study area boundary creating a new raster file in the process
rasterizeToReference(inPaths$bound, outPaths$roadsTemp, 
                     reference = outPaths$ref, burn = 0, allTouched = TRUE)

# Burn in 1 where there are roads (on top of values from prior step)
rasterizeToReference(outPaths$roadLines, destination = outPaths$roadsTemp, 
                     burn = 1)

# Write to new file while clipping.  Without this step road cells outside of the 
#  study boundary will have a value of 1. This re-sets them to NA.
warpToReference(outPaths$roadsTemp, outPaths$roads, reference = outPaths$ref,
                clip = inPaths$bound)

# Delete intermediate file
deleteTif(outPaths$roadsTemp)

## ---- results= FALSE----------------------------------------------------------
cat("Files created here:\n\t",
    normalizePath(outPaths$ref), " our reference raster\n\t",
    normalizePath(outPaths$roadClass), " road class\n\t", 
    normalizePath(outPaths$roads), " roads 1, other study area cells 0\n\t", 
    normalizePath(outPaths$slope), " slope."
    )

# Confirm that projections information is there for all files
# Not required but changes in R's handling of spatial data have broken this in the past
refcrs <- terra::crs(terra::rast(outPaths$ref))  # wkt of refrence grid we created
if(refcrs == "") stop("Reference grid doesn't have projection information")
for(file in c("slope", "roads", "roadClass")){
  filecrs <- terra::crs(terra::rast(outPaths[[file]]))
  stopifnot(filecrs == refcrs)
}


## ---- results= FALSE----------------------------------------------------------
  final.dir <- file.path(outDir, "final")
 dir.create(final.dir, showWarnings = FALSE)
 makeNiceTif(outPaths$slope, file.path(final.dir,  "slope.tif") , overviewResample = "average")


## ----results= FALSE-----------------------------------------------------------
key <- read.csv(inPaths$key, stringsAsFactors = FALSE)  

# Note the table must have "value", and "color" columns and can optionally 
# have a "category" column will class labels
vrt.file <- addColorTable(outPaths$roadClass, table = key)


## -----------------------------------------------------------------------------
finalRoadClass <- file.path(final.dir,  "roadClass.tif")
makeNiceTif(source = vrt.file, destination = finalRoadClass, overwrite = TRUE, stats = FALSE )

## -----------------------------------------------------------------------------

addVat(x = finalRoadClass, attributes = key)


## -----------------------------------------------------------------------------
roadWithVAT <- file.path(final.dir,  "roadsWithVAT.tif")
makeNiceTif(source = outPaths$roadClass, destination = roadWithVAT, overwrite = TRUE, overviewResample = "near", stats = FALSE, vat = TRUE )

