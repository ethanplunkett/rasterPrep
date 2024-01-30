# rasterPrep

**rasterPrep** prepares raster data for analysis or modeling 
within R; and (after modeling) prepares output raster data for viewing in 
GIS software. 
  
To support modeling this package has tools for creating a series of aligned
TIFF files from raster and vector data.   

A separate set of tools converts raster files into TIFF files optimized 
for viewing with stand alone GIS software by adding overviews, tiling, 
compression, VATs, stats, histograms, and/or embedded color tables. 
These tools will be especially useful when working with large datasets 
that may load very slowly or require additional processing in GIS software
prior to viewing unless these extra components are provided. 

Note, this package was recently updated to use **sf** to call gdal 
utilities instead of relying on them being installed on the local system.
However, `addOverviews` still executes a shell command, this will be updated
after the next release of *sf* which will include a bug fix to `sf::gdal_addo`
https://github.com/r-spatial/sf/pull/2323.  In the meantime either have the
utilities installed on your system or avoid that call. 

## Installation

Use the code below to install rasterPrep from 
[github](https://github.com/ethanplunkett/rasterPrep).
``` r
# Without vignette:
devtools::install_github("ethanplunkett/rasterPrep")
```

## Example

See the 
[vignette](http://htmlpreview.github.io/?https://github.com/ethanplunkett/rasterPrep/blob/master/doc/rasterPrep.html) 
for example usage.  
