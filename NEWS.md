# rasterPrep 0.1.1.9001

* rasterInfo(), a new function migrated from gridio returns a nested list with
parsed output from gdalinfo via [sf::sf::gdal_utils()] 
(same as [terra::describe()]) it returns a rasterInfo object.

* print.rasterInfo() a new function (migrated from gridio) prints a rasterInfo
object nicely (or at least better than the default list printing).

* Added a `NEWS.md` file to track changes to the packag and moved old items from
the change log to NEWS.md

## July 8, 2022 (v.0.1.10) 

assessType now returns values consistent with the raster package's default no data types for Int16 and Int32. Previously these returned smaller values that were theoretically the smallest value an integer could hold, however, in practice it seemed to trip up some GIS software.

## Jan 6. 2021 (v. 0.1.9) 
* makeNiceTif now calls gdalinfo with -stats -hist flags when the stats argument 
is TRUE. This forces calculation of a histogram as well as the statistics. 
Previously I used the -stats flag in gdal_translate which only did the stats.

## Oct 28, 2021 (v. 0.1.8)

* addOverviews now defaults to clean = FALSE. With clean = TRUE it just deleted overviews.

## Oct 21, 2021 (v. 0.1.7)

* The two functions that use gdal_rasterize (rasterizeToRefence() and makeReference()) were failing to retain the CRS. I tracked this down to the GDAL_DATA envirnomental variable that rgdal is setting on load.  I was suprised to see that sf package also sets GDAL_DATA when it loads - to a yet different path. This problem was subtle as gdal_translate ran cleanly without error but silently dropped the projection. I now set both environmental variables (GDAL_DATA and PROJ_LIB) before system calls and then set both back to prior values right after each call.  This is all controlled thorugh rasterPrepOptions.  Note when I later tried to create a reproducible minimal example demonstrating side-effects from GDAL_DATA I couldn't; so I no longer feel certain that GDAL_DATA is a problem but I'm leaving code to reset it in just in case as I feel like the GDAL_DATA should be set to the directory on the system not those in the package installations of GDAL while executing system commands.

* makeReference, warpToReference, rasterizeToReference, and makeNiceTif now all
throw a errors if the destination file they create lacks a CRS.  

* The vignette now uses terra and gdalUtilities instead of rgdal, raster, and 
gdalUtils. It also deletes any prexisting output files and checks to make sure
there is consistent CRS information for the files created.

* addColorTable now uses terra instead of rgdal and the package no longer 
depends on rgdal directly.

* addColorTable still uses raster::freq() as in my tests terra::freq() threw
errors but this is the last remnant of raster and sp that I'm aware of.
  

## October 5, 2021 (v 0.1.6)

* I replaced most calls to gdalUtils with system calls.  At one point I had 
thought about migrating all calls to gdalUtils but it's easier to manage the PROJ_LIB problem (see Sept 30 updates) with direct system calls. Consequently rasterPrep package no longer depends on the gdalUtils package. 

* I streamlined rasterizeToReference which had some code that looks like it was
added to correct the problem fixed in 0.1.5 before I fully understood the problem.

* I've started eliminating all calls to rgdal, sp, and raster using terra and sf instead. Still pending:
  addVat uses raster::freq, need to replace with terra.
  the vignette still uses gdalUtils and rgdal 
  addColor table still uses rgdal::GDALinfo

## September 30, 2021 (V 0.1.5)

* rasterPrep now by default sets the system environment setting  PROJ_LIB to  "" before invoking system calls. This behavior can be controlled with a new function rasterPrepOptions(). The current options are resetLibs and projLib which determine whether PROJ_LIB is set and what it's set to.  In all cases after running the system call the PROJ_LIB enviroment value is reset to its original value. All of this is in response to a problem discussed here:  https://github.com/r-spatial/discuss/issues/31 where rgdal sets PROJ_LIB to point to its own installation within the package directory and consequently calls to gdal utilities installed on the system (perhaps a newer or older version) also use rgdal's proj library instead of the system's.  This mismatch sometimes results in clear errors but other times result in opaque errors or output that looks OK but is missing the spatial reference. The default value of "" is correct for my system. To determine what to use for your system restart R without any packages loaded and then execute: Sys.getenv("PROJ_LIB").  Note though if you see a path that ends in "/rgdal/proj" than you're looking at it after
rgdal has set it and you very likely do not want to use that path. 



