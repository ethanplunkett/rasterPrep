# rasterPrep

The goal of rasterPrep is to prepare raster data for analysis or modeling 
within R; and (after modeling) to prepare output raster data for viewing in 
GIS software. 
  
To support modeling this package has tools for creating a series of aligned
tif files from raster and vector data.   

A separate set of tools converts raster files into tif files optimized 
for viewing with stand alone GIS software by adding overviews, tiling, 
compression, VATs, stats, and/or embedded color tables. These tools will be 
especially useful for folks working with large datasets that may load very
slowly or require additional processing in GIS software prior to viewing unless
these extra components are provided. 


## Installation

Caution, to date I have only tested this software on Windows machines. I am not
aware of any reason it will not run on other operating systems but I suspect 
there may be a few bumps to be sorted out.  If you are running on another OS and
run into trouble (or don't) I'd love to hear about it.

This package requires that GDAL Utilities be installed on your computer and the
directory that houses them be added to your PATH environmental variable.  On
Linux and Mac systems I recommend you use your package manager to install GDAL.
On Windows I recommend you use the Open Source Geo for Windows (OSGEO4w) installer
[https://trac.osgeo.org/osgeo4w/](https://trac.osgeo.org/osgeo4w/)  Use express web 
install and then select GDAL only.
You will need to add the path to the osgeo4win bin directory to your system's
PATH environmetnal variable.  On Windows it's likely something along the lines of
 C:\OSGeo4W64\bin but please verify on your system.

Once you've installed GDAL Utilities use the code below to install rasterPrep 
from [github](https://github.com/ethanplunkett/rasterPrep).
``` r
# Without vignette:
devtools::install_github("ethanplunkett/rasterPrep")

# With vignette -- This is slower and may throw cryptic errors 
#  if gdal utilities aren't installed and on the sytem path
devtools::install_github("ethanplunkett/rasterPrep",
                          build_opts = c("--no-resave-data", "--no-manual"))

vignette("rasterPrep")

```

## Example

See the [vignette](http://htmlpreview.github.io/?https://github.com/ethanplunkett/rasterPrep/blob/master/doc/rasterPrep.html) for example usage.  

## Change log

Oct 21, 2021 (v. 0.1.7)
 The two functions that use gdal_rasterize (rasterizeToRefence() and makeReference()) were failing to retain the CRS. I tracked this down to the GDAL_DATA envirnomental variable that rgdal is setting on load.  I was suprised to see that sf package also sets GDAL_DATA when it loads - to a yet different path. This problem was subtle as gdal_translate ran cleanly without error but silently dropped the projection. I now set both environmental variables (GDAL_DATA and PROJ_LIB) before system calls and then set both back to prior values right after each call.  This is all controlled thorugh rasterPrepOptions.

makeReference, warpToReference, rasterizeToReference, and makeNiceTif now all throw a errors if the destination file they create lacks a CRS.  

The vignette now uses terra and gdalUtilities instead of rgdal, raster, and gdalUtils. It also deletes any prexisting output files and
  checks to make sure there is consistent CRS information for the files created.

addColorTable now uses terra instead of rgdal and the package no longer depends on rgdal directly.

addColorTable still uses raster::freq() as in my tests terra::freq() threw errors but this is the last remnant of raster and sp that I'm aware of.
  

October 5, 2021 (v 0.1.6)
I replaced most calls to gdalUtils with system calls.  At one point I had thought about migrating all calls to gdalUtils but it's easier to manage the PROJ_LIB problem (see Sept 30 updates) with direct system calls. Consequently rasterPrep package no longer depends on the gdalUtils package. 

I streamlined rasterizeToReference which had some code that looks like it was added to correct the problem fixed in 0.1.5 before I fully understood the problem.

I've started eliminating all calls to rgdal, sp, and raster using terra and sf instead. Still pending:
  addVat uses raster::freq, need to replace with terra.
  the vignette still uses gdalUtils and rgdal 
  addColor table still uses rgdal::GDALinfo

September 30, 2021 (V 0.1.5)

rasterPrep now by default sets the system environment setting  PROJ_LIB to  "" before invoking system calls. This behavior can be controlled with a new function rasterPrepOptions(). The current options are resetLibs and projLib which determine whether PROJ_LIB is set and what it's set to.  In all cases after running the system call the PROJ_LIB enviroment value is reset to its original value. All of this is in response to a problem discussed here:  https://github.com/r-spatial/discuss/issues/31 where rgdal sets PROJ_LIB to point to its own installation within the package directory and consequently calls to gdal utilities installed on the system (perhaps a newer or older version) also use rgdal's proj library instead of the system's.  This mismatch sometimes results in clear errors but other times result in opaque errors or output that looks OK but is missing the spatial reference. 

The default value of "" is correct for my system. To determine what to use for your system restart R without any packages loaded and then execute: Sys.getenv("PROJ_LIB").  Note though if you see a path that ends in "/rgdal/proj" than you're looking at it after
rgdal has set it and you very likely do not want to use that path. 




