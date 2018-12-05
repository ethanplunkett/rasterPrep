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
