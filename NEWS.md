
## rasterPrep 0.2.0.9002

  * Use sf's gdal for overviews.
  * update documentation


# rasterPrep 0.2.0.9001

### Breaking Changes

  * Fix bug in `addVat` that caused it to replace the second 
  attribute column name with "attributes" when there were only two columns.
  For example if the input names where  "VALUE", and "class" the output 
  previously would have had columns  "VALUE", "COUNT", and "attributes", 
  but will now have "VALUE", "COUNT", and "class". Although this fixes a bug it
  may break existing code that expect the old behavior.

### Updates
  * Drop **raster** package in favor of **terra** this was 95% done in prior 
  version.  Replacing `raster::freq()` that with `terra::freq()` within
  `addOverviews()` was the last step.
  
  * Add unit testing for all functions that call gdal utilities.
  
  * Switch from using shell commands to calling `sf::gdal_utils`, 
  which uses gdal distributed with the **sf**
  package. So the user does not need to install thier own gdal for the 
  command line; and issues arising from weird GDAL configurations should be
  reduced and opefully will be fully the responsibility of the **sf** team.
    * This is complete with the exception of `addOverviews`. An 
    [accepted PR](https://github.com/r-spatial/sf/pull/2323)
    
    fixes an issue with `sf::gdal_addo` that blocked the use of 
    configure options - making it impossible to create compressed overviews. 
    With the next update to **sf** on CRAN I'll switch `addOverviews` over to
    **sf** too.
    * Setting `rasterPrepOptions(usesf = FALSE)` will switch back to the old 
    behavior of exclusively using shell commands.
  
  * Overhaul vignette.
  * Update readme.
  * Check spelling
  * Lint

## rasterPrep 0.1.12.9004

* On load rasterPrep now looks for an environmental variable `RASTERPREP_PROJ` 
  and if it exists calls `rasterPrepOptions(projLib = value)` with the result.

  This allows setting the environmental variable projlib in a consistent way 
  regardless of whether sp or rgdal is loaded in the session 
  (they change `PROJ_LIB`).  

  Ultimately, I'd like to switch to using the wrappers to gdal utilities that
  are in **sf** so that we don't need to mess with system variables.

* `makeNiceTif()` gains new argument `noDataValue` that allows controlling the
  no data value assigned to the output grid. Cell values are not updated.

# rasterPrep 0.1.12.9003

* Fixed spelling in addOverviews. Fixes #5.

# rasterPrep 0.1.12.9002

* Added verbose setting that defaults to FALSE and can be turned on with `rasterPrepOptions(verbose = TRUE)`.  This means that the default behavior is 
not to print any messages to the screen.

* All cat() and print() statements are now conditional on verbose setting.

# rasterPrep 0.1.12.9001

* rasterInfo(), a new function migrated from gridio returns a nested list with
parsed output from gdalinfo via [sf::sf::gdal_utils()] 
(same as [terra::describe()]) it returns a rasterInfo object.

* print.rasterInfo() a new function (migrated from gridio) prints a rasterInfo
object nicely (or at least better than the default list printing).

* Added a `NEWS.md` file to track changes to the package and moved old items from
the change log to NEWS.md

* Converted to roxygen with markdown support

* Used roxygen2md to update documentation (from old .Rd formatting to markdown)


## Nov 17, 2022 (v.0.1.12) 
`warpToReference()` now has `BigTIFF` argument to request the BigTIFF extension
of the geoTIFF format in output.
BigTIFF is an extension of TIFF that allows files 
over 5 GiB. When writing compressed TIFFs, the new default since v.0.1.11, 
GDAL no longer automatically generates BigTIFF as needed. The new argument 
allows the user to force the output to be BigTIFF. If you get an error along 
the lines of:

    `TIFFFetchDirectory:  path/file.tif: Can not read TIFF directory count (GDAL error 1)`, 
    `TIFFReadDirectory:Failed to read directory at offset 4294967234 (GDAL error 1)'`, or
    `TIFFAppendToStrip:Maximum TIFF file size exceeded` 

Than you may need to use BigTIFF = TRUE

## Oct 8, 2022 (v.0.1.11) 

* added `compression` argument to warpToReference which allows specifying LZW 
or DEFLATE compression.

## July 8, 2022 (v.0.1.10) 

* assessType now returns values consistent with the raster package's default no data types for Int16 and Int32. Previously these returned smaller values that were theoretically the smallest value an integer could hold, however, in practice it seemed to trip up some GIS software.

## Jan 6. 2021 (v. 0.1.9) 
* makeNiceTif now calls gdalinfo with -stats -hist flags when the stats argument 
is TRUE. This forces calculation of a histogram as well as the statistics. 
Previously I used the -stats flag in gdal_translate which only did the stats.

## Oct 28, 2021 (v. 0.1.8)

* addOverviews now defaults to clean = FALSE. With clean = TRUE it just deleted overviews.

## Oct 21, 2021 (v. 0.1.7)

* The two functions that use gdal_rasterize (`rasterizeToRefence()` and `makeReference()`) were failing to retain the CRS. I tracked this down to the GDAL_DATA environment variable that rgdal is setting on load.  I was surprised to see that **sf** package also sets GDAL_DATA when it loads - to a yet different path. This problem was subtle as gdal_translate ran cleanly without error but silently dropped the projection. I now set both environmental variables (GDAL_DATA and PROJ_LIB) before system calls and then set both back to prior values right after each call.  This is all controlled through `rasterPrepOptions()`.  Note when I later tried to create a reproducible minimal example demonstrating side-effects from GDAL_DATA I couldn't; so I no longer feel certain that GDAL_DATA is a problem but I'm leaving code to reset it in just in case as I feel like the GDAL_DATA should be set to the directory on the system not those in the package installations of GDAL while executing system commands.

* makeReference, warpToReference, rasterizeToReference, and makeNiceTif now all
throw a errors if the destination file they create lacks a CRS.  

* The vignette now uses terra and gdalUtilities instead of rgdal, raster, and 
gdalUtils. It also deletes any pre-existing output files and checks to make sure
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

* I've started eliminating all calls to rgdal, sp, and raster using terra and 
sf instead. Still pending:
  `addVat` uses `raster::freq`, need to replace with **terra**.
  the vignette still uses **gdalUtils** and **rgdal** 
  `addColorTable` still uses `rgdal::GDALinfo`

## September 30, 2021 (V 0.1.5)

* rasterPrep now by default sets the system environment setting  PROJ_LIB to  "" before invoking system calls. This behavior can be controlled with a new function rasterPrepOptions(). The current options are resetLibs and projLib which determine whether PROJ_LIB is set and what it's set to.  In all cases after running the system call the PROJ_LIB environment variable is reset to its original value. All of this is in response to a problem discussed here:  https://github.com/r-spatial/discuss/issues/31 where rgdal sets PROJ_LIB to point to its own installation within the package directory and consequently calls to gdal utilities installed on the system (perhaps a newer or older version) also use rgdal's proj library instead of the system's.  This mismatch sometimes results in clear errors but other times result in opaque errors or output that looks OK but is missing the spatial reference. The default value of "" is correct for my system. To determine what to use for your system restart R without any packages loaded and then execute: Sys.getenv("PROJ_LIB").  Note though if you see a path that ends in "/rgdal/proj" than you're looking at it after
rgdal has set it and you very likely do not want to use that path. 



