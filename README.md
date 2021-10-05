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
On Windows I recommend you download the latest "Stable release" from
http://download.gisinternals.com/; you want the "Generic installer for the GDAL
core components".   With considerably more effort you can install GDAL Utilities
on Windows with python bindings but it's not required for this package;  If
you want to anyway read [this](https://sandbox.idre.ucla.edu/sandbox/tutorials/installing-gdal-for-windows).

Once you've installed GDAL Utilities use the code below to install rasterPrep 
from [github](https://github.com/ethanplunkett/rasterPrep).
``` r
# Without vignette:
devtools::install_github("ethanplunkett/rasterPrep")

# With vignette -- This is slower and will throw cryptic errors 
#  if gdal utilities aren't installed and on the sytem path
devtools::install_github("ethanplunkett/rasterPrep",
                          build_opts = c("--no-resave-data", "--no-manual"))

vignette("rasterPrep")

```

## Example

See the [vignette](http://htmlpreview.github.io/?https://github.com/ethanplunkett/rasterPrep/blob/master/doc/rasterPrep.html) for example usage.  

## Change log

October 5, 2021 (v 0.1.6)
I replaced the few calls to gdalUtils with system calls.  At one point I had thought about migrating all calls to gdalUtils but it's easier to manage the PROJ_LIB problem (see Sept 30 updates) with direct system calls. Consequently rasterPrep package no longer depends on the gdalUtils package. 

I streamlined rasterizeToReference which had some code that looks like it was added to correct the problem fixed in 0.1.5 before I fully understood the problem.

I've started eliminating all calls to rgdal, sp, and raster using terra and sf instead. Still pending:
  addVat uses raster::freq, need to replace with terra.
  the vignette still uses gdalUtils and rgdal 
  addColor table still uses rgdal::GDALinfo

September 30, 2021 (V 0.1.5)

rasterPrep now by default sets the system environment setting  PROJ_LIB to  "" before invoking system calls. This behavior can be controlled with a new function rasterPrepOptions(). The current options are setProjLib and projLib which determine whether PROJ_LIB is set and what it's set to.  In all cases after running the system call the PROJ_LIB enviroment value is reset to its original value. All of this is in response to a problem discussed here:  https://github.com/r-spatial/discuss/issues/31 where rgdal sets PROJ_LIB to point to its own installation within the package directory and consequently calls to gdal utilities installed on the system (perhaps a newer or older version) also use rgdal's proj library instead of the system's.  This mismatch sometimes results in clear errors but other times result in opaque errors or output that looks OK but is missing the spatial reference. 

The default value of "" is correct for my system. To determine what to use for your system restart R without any packages loaded and then execute: Sys.getenv("PROJ_LIB").  Note though if you see a path that ends in "/rgdal/proj" than you're looking at it after
rgdal has set it and you very likely do not want to use that path. 




