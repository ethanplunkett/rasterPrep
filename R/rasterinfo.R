#' get detailed information about raster files
#'
#' This is a wrapper for the gdalinfo utility (accessed through either
#' [sf::gdal_utils] or [terra::describe]) that returns parsed output in a nested
#' list.
#'
#' This is a work in progress. The information returned by gdalinfo depends on
#' the file type described and I've developed this while working primarily with
#' single band ArcInfo Grids and geoTiff files so this function is unlikely to
#' parse all available information for all file types. Use `print(rasterInfo(x),
#' raw = TRUE)`,  `x$raw` if `x` is a rasterInfo object, to see the unparsed
#' text.
#'
#' Some return values aren't standard for gdal but are calculated here from
#' other values (e.g. `cellsize`, `type`, `extent`).

#' @rdname rasterInfo
#' @param x  For raster info the path to a raster file.
#' For the print method a rasterInfo object as returned by `rasterInfo`.
#' @param all if `FALSE` (the default) some potentially lengthly components of
#' `x` are omitted.  Use `print(x, all=TRUE)` to print everything.
#' @param raw if `TRUE` Then the raw text from the gdalinfo call is printed.
#'  Use this if you think the something is being lost when the text is parsed
#'  into the list.
#' @param ... required by generic method.
#' @return  A nested list of class `rasterInfo` with most of the
#' information returned by gdalinfo ([terra::describe()]) and some additional
#' derived elements.
#'  \item{path}{The path to the raster file, same as `x`}
#'  \item{driver}{The driver used to read the file which usually matches the file's format}
#'  \item{type}{Calculated from `bands` will be "missing" if the raster file is missing or unreadable, "mixed" if there are multiple bands and they have varying types, otherwise the type associated with the band(s).}
#'  \item{res}{the resolution of  the cells,  as reported gdal (two dimensions, possible negative)}
#'  \item{cellsize}{calculated from `res` `NA` if the cells aren't square, otherwise the (positive) dimension of the cell}
#'  \item{na.value}{`NA` if bands have inconsitent no data values. Otherewise the no data value used by the band(s)}
#'  \item{dim}{The dimensions of the raster in cells (rows, then cols).  This is from a raw line that reads  "Size is" and reports cols then rows (reverse of order used here). }
#'  \item{rows}{the number of rows in the file  (copied from `dim`)}
#'  \item{cols}{the number of columns in the file (copied from `dim`)}
#'  \item{origin}{the coordinates of the origin of the file (this seems to be the upper left corner). Note, gdal
#'    may report more digits than R can represent. Look at raw output to see the text as reported by gdal}
#'  \item{approx.coords}{the approximate coordinates of the corners and center. This is reported by `gdalinfo` but appears to be rounded, thus the "approx"}
#'  \item{extent}{A list with items `xll`, `yll`, `nrow`, `ncol`, `cellsize`. If cells are square these are calculated from  `origin`, `rows`, `cols` and `cellsize`. This format matches the way grid extents are defined in \pkg{gridio} so can be used with functions such as [coincide()]  or [checkcellalignment()].}
#'  \item{nbands}{The number of bands in the file.}
#'  \item{bands}{A list  of lists (one per band); inner list has items \describe{
#'    \item{id}{assigned, 1:`nbands`}
#'    \item{block}{the block dimensions as reported by gdal (colums, rows)}
#'    \item{type}{the storage type of the band}
#'    \item{colorinterp}{reported by gdal}
#'    \item{stats}{list of min, max, mean, sd}
#'    \item{na.value}{the No Data value used for the band. NA if there is no No Data value or if gdalinfo reports 'nan'}
#'    \item{metadata}{any band specific metadata}
#'    \item{imagestructuremetadata}{any band specific image structure metadata}
#'  }}
#'  \item{metadata}{any file metadata}
#'  \item{imagestructuremetadata}{any file image structure metadata}
#'  \item{crs}{The coordinate reference system in well known text format}
#'  \item{files}{a vector of all the files associated with the raster dataset}
#'  \item{attributetable}{the raster attribute table   (AKA Value Attribute Table, VAT)}
#'  \item{raw}{The raw text returned by gdalinfo that was parsed to create all other items in this list}
#'
#'
#'
#'
#' @export
#'
#' @examples
#'  tif <- system.file("extdata","slope.tif" , package = "rasterPrep")
#'  rasterInfo(tif)
#'
#' @note  The `print` method for rasterInfo will, by default, skip some
#'   potentially large components. Use `print(x, all = TRUE)` to print
#'   everything but the raw text; or `print(x, raw = TRUE)` to print the
#'   raw text only. If the raster file is missing, corrupt, or otherwise
#'   unreadable by gdal `type` will be `"missing"`, `path` will
#'   be the input path, and all other items in the returned list will be
#'   `NA`.
#'
#' @references  https://gdal.org/programs/gdalinfo.html
rasterInfo <- function(x){

  use <- NA
  s <- search()
  if("package:terra" %in% s)
    use <- "terra"
  if(is.na(use) && "package:sf" %in% s)
    use <- "sf"
  if(is.na(use))
    use <- "sf"

  # Get raw text from gdal_info
  if(use == "sf"){
    l <- sf::gdal_utils(source =  x, quiet = TRUE)
    l<- unlist(strsplit(l, "\n"))
  }
  if(use == "terra"){
    l <- terra::describe(x)
  }


  raw <- l
  # cat(l, sep ="\n")
  result <- list(path = x,
                 driver = NA,
                 type = NA,
                 res = NA,
                 cellsize = NA,
                 na.value = NA,
                 dim = NA,
                 rows = NA,
                 cols = NA,
                 origin = NA,
                 approx.coords = NA,
                 extent = NA,
                 nbands = NA,

                 bands = NA,
                 metadata = NA,
                 imagestructuremetadata = NA,
                 crs = NA,
                 files = NA,
                 attributetable = NA)


  if(length(l) == 0){
    result$type <- "missing"
    return(result)
  }


  #------------------------------------------------------------------------------------------------------------------------#
  # Separate out the Raster Attribute Table (mostly because it's long and annoying)
  #------------------------------------------------------------------------------------------------------------------------#
  start <- grep("<GDALRasterAttributeTable", l, ignore.case = TRUE)
  end <- grep("</GDALRasterAttributeTable>", l, ignore.case = TRUE)
  if(length(end) > 1 || length(start) > 1) stop("More than one GDALRasterAttributeTable. This is unexpected.")
  if(length(start) != length(end)) stop("There should be either 1 start and 1 end of the attribute table or no start and end")

  if(length(start) == 1){
    result$attributetable <- l[start:end]  # move to result
    l <- l[setdiff(1:length(l), start:end)] # cut out from l
  }
  rm(start, end)
  #------------------------------------------------------------------------------------------------------------------------#

  #------------------------------------------------------------------------------------------------------------------------#
  # Driver
  #------------------------------------------------------------------------------------------------------------------------#
  ln <- grep("^Driver:", l, ignore.case = TRUE)
  result$driver = .extract.tag("^Driver:", l[ln])
  l <- l[-ln]

  #------------------------------------------------------------------------------------------------------------------------#
  # Files:  (start at Files: end at first subsequent line that doesn't start with blank space)
  #------------------------------------------------------------------------------------------------------------------------#
  start <- grep("^Files:", l, ignore.case = TRUE)
  stopifnot(length(start) == 1)
  end <- start
  while(TRUE){
    if(!grepl("^[[:blank:]]", l[end +1 ])) break
    end <- end + 1
    if(end == length(l)) break
  }
  files <- l[start:end]
  l <- l[-(start:end)]

  files[1] <- gsub("^Files:[[:blank:]]*", "", files[1], ignore.case = TRUE) # drop files tag from first files line
  files <- trimws(files)
  files <- normalizePath(files, winslash = "/")
  result$files <- files


  #------------------------------------------------------------------------------------------------------------------------#
  # CRS (Coordinate Reference System, AKA projection):
  #    start at "Coordinate System is:"
  #     Allow one line without leading blank and then stop at first line without a leading blank
  #------------------------------------------------------------------------------------------------------------------------#
  start <- grep("^Coordinate System is:", l, ignore.case = TRUE)
  if(length(start) > 1) stop("More than one Coordinate System is: line\n")
  if(length(start) == 1){
    end <- start + 1
    while(TRUE){
      if(!grepl("^[[:blank:]]", l[end +1 ])) break
      end <- end + 1
      if(end >= length(l)) break
    }
    crs <- l[start:end]
    l <- l[-(start:end)]

    crs <-  crs[-1] # drop header line
    result$crs <- paste0(crs, collapse = "\n")
    rm(start, end, crs)
  }

  #------------------------------------------------------------------------------------------------------------------------#
  # Size: assume single line with two values cols, rows
  #------------------------------------------------------------------------------------------------------------------------#
  ln <- grep("^Size is", l, ignore.case = TRUE)
  if(length(ln) != 1)
    stop("Expected one and only one Size is line")

  size <- l[ln]
  size <- gsub("Size is[[:blank:]]*", "",  size)
  size <- strsplit(size, "[[:blank:]]*,[[:blank:]]*")[[1]]
  if(length(size) != 2) stop("Expected only two values for Size (cols, rows) found", length(size))
  if(any(grepl("[^[:digit:].]", size)))
    stop("There appears to be non numeric values after Size is. I'm finding:", paste(size, collapse =", "))
  size <- as.numeric(size)
  result$dim <- size[c(2, 1)]  # switch to rows, cols
  result$rows <- size[2]
  result$cols <- size[1]
  l <- l[-ln]
  rm(size)



  #------------------------------------------------------------------------------------------------------------------------#
  # Pixel Size:  assigned to "res" and if both are equal also assigned to cellsize
  #------------------------------------------------------------------------------------------------------------------------#
  ln <- grep("^Pixel size =", l, ignore.case = TRUE)
  if(length(ln) != 1){
    # Previously I threw an error here as of Nov 2022 I'm assuming if there's no pixel size line that the target isn't
    # a raster GIS file. And returning a type = "missing" as per documentation.
    # This is known to happen if you call rasterInfo on a standard (non geo) TIFF
    result$type <- "missing"
    return(result)
  }

  res <- l[ln]
  res <- gsub("^Pixel Size = [[:blank:]]*\\(|\\)[[:blank:]]*$", "",  res)
  res <- strsplit(res, "[[:blank:]]*,[[:blank:]]*")[[1]]
  if(length(res) != 2) stop("Expected only two values for pixel size (xdim, ydim) found", length(res))
  if(any(grepl("[^[:digit:]\\.-]", res)))
    stop("There appears to be non numeric values after Size is. I'm finding:", paste(res, collapse =", "))
  res <- as.numeric(res)
  result$res <- res
  if(abs(res[1]) == abs(res[2]))
    result$cellsize = abs(res[1])
  l <- l[-ln]

  #------------------------------------------------------------------------------------------------------------------------#
  # metadata and image structure metadata
  #  note these tags can exist for the entire file  or for individual bands
  #   just processing entire file here - with no leading spaces
  #------------------------------------------------------------------------------------------------------------------------#

  # Extract metadata as lines of of text
  # This will be a Metadata: line without leading white space
  start <- grep("^*Metadata:", l, ignore.case = TRUE)
  if(length(start) > 1 )  stop("Found more than one non-indented Metadata:  tag. Expected at most one.")
  if(length(start) == 0){
    result$metadata = NA
  }else{
    ostart <- start
    start <- ostart + 1 # skip first line which is just the metadata heading
    end <- start
    indent <- gsub("(^[[:blank:]]*)(.*$)", "\\1", l[start], perl = TRUE) # extract whitespace from beginning first line after start
    while(TRUE){
      if(!grepl(paste0("^", indent), l[end +1 ])) break
      end <- end + 1
      if(end == length(l)) break
    }
    result$metadata <- trimws(l[start:end])
    l  <- l[-(ostart:end)]  #  drop from l
  }

  # extract image  structure metadata
  start <- grep("^*Image Structure Metadata:", l, ignore.case = TRUE)
  if(length(start) > 1 )  stop("Found more than one non-indented Metadata:  tag. Expected at most one.")
  if(length(start) == 0){
    result$imagestructuremetadata = NA
  }else{
    ostart <- start
    start <- ostart + 1 # skip first line which is just the metadata heading
    end <- start
    indent <- gsub("(^[[:blank:]]*)(.*$)", "\\1", l[start], perl = TRUE) # extract whitespace from beginning first line after start
    while(TRUE){
      if(!grepl(paste0("^", indent), l[end +1 ])) break
      end <- end + 1
      if(end == length(l)) break
    }
    result$imagestructuremetadata <- trimws(l[start:end])
    l  <- l[-(ostart:end)]  #  drop from l (including header tag)
  }




  #------------------------------------------------------------------------------------------------------------------------#
  # Band info
  #------------------------------------------------------------------------------------------------------------------------#
  bands <- list()
  i <- 1
  while(any(grepl("^Band ", l))){
    start <- grep("Band ", l)[1]
    end <- start
    while(TRUE){
      if(!grepl("^[[:blank:]]", l[end +1 ])) break
      end <- end + 1
      if(end == length(l)) break
    }
    # Pull out this bands lines into b
    b <- l[start:end]
    l <- l[-(start:end)]
    band <- list()

    # Parse band, block , type, colorInterpe
    ln <- grep("^Band", b, ignore.case = TRUE)
    if(length(ln) != 1) stop("Expected one and only one Band= for this band"
    )
    band$id <- .extract.tag("Band", b[ln], numeric = TRUE)
    block<-  .extract.tag("Block=", b[ln], numeric = FALSE)
    block <- as.numeric(unlist(strsplit(block, "x")))
    band$block <- block
    band$type <- .extract.tag("Type=", b[ln])
    band$colorinterp <- .extract.tag("ColorInterp=", b[ln])

    # Parse Statistics
    has.full.stats <- FALSE
    band$stats <- list(min = NA, max=NA, mean=NA, sd=NA)

    # Full statistics line
    ln <- grep("Minimum.*Maximum.*Mean.*StdDev", b, ignore.case = TRUE)
    if(length(ln) == 1){
      has.full.stats <-  TRUE
      a <-  b[ln]
      band$stats$min <- .extract.tag("Minimum=", a, numeric = TRUE)
      band$stats$max <- .extract.tag("Maximum=", a, numeric = TRUE)
      band$stats$mean <- .extract.tag("Mean=",a , numeric = TRUE)
      band$stats$sd <- .extract.tag("StdDev=", a, numeric = TRUE)
    }
    if(!has.full.stats){
      #There seems to be an alternate min and max value on a line without other statistics
      # I'm not sure if always present but if there aren't full stats and this line exists I'm going to grab it
      ln <- grep("Min=.*Max=", b)
      band$stats$min <- .extract.tag("Min=", b[ln], TRUE)
      band$stats$max <- .extract.tag("Max=", b[ln], TRUE)
    }

    band$na.value <- .extract.tag("NoData Value=", b, numeric = FALSE)
    if(is.na(band$na.value) || band$na.value == "nan"){
      band$na.value <- NA
    } else {
      band$na.value <- as.numeric(band$na.value)
    }



    # Extract band metadata as a block of text
    # Note this will be a Metadata: line  with leading white space
    start <- grep("^[[:blank:]]*Metadata:", b)
    if(length(start) == 0){
      band$metadata = NA
    }else{
      start <- start + 1 # skip first line which is just the metadata heading
      end <- start
      indent <- gsub("(^[[:blank:]]*)(.*$)", "\\1", b[end], perl = TRUE) # extract whitespace from beginning first line after start
      while(TRUE){
        if(!grepl(paste0("^", indent), b[end +1 ])) break
        end <- end + 1
        if(end == length(b)) break
      }
      band$metadata <- trimws(b[start:end])
      b  <- b[-( (start-1):end)]
    }

    # Extract band image structure metadata as a block of text
    # Note this will be a Image Structure Metadata: line  with leading white space
    start <- grep("^[[:blank:]]*Image Structure Metadata:", b)
    if(length(start) == 0){
      band$imagestructuremetadata = NA
    }else{
      start <- start + 1 # skip first line which is just the metadata heading
      end <- start
      indent <- gsub("(^[[:blank:]]*)(.*$)", "\\1", b[end], perl = TRUE) # extract whitespace from beginning first line after start
      while(TRUE){
        if(!grepl(paste0("^", indent), b[end +1 ])) break
        end <- end + 1
        if(end == length(b)) break
      }
      band$imagestructuremetadata <- trimws(b[start:end])
      b  <- b[-( (start-1):end)]
    }


    bands[[i]] <- band
    names(bands)[i] <- paste("band", band$id)
    i <- i + 1
  }
  result$bands <- bands

  # Extract origin and corner coordinates
  extract.coords <- function(tag, x){
    a <- grep(tag, x, ignore.case = TRUE)
    if(length(a) > 1) stop("Duplicated tags")
    x <- x[a]
    x <- gsub("^[^(]*\\(|\\).*$", "", x = x)  #  drop everything up until first "(" and everything  after first ")"

    x <- unlist(strsplit(x, ","))
    x <- gsub("[[:blank:]]", "", x)
    x <- as.numeric(x)
    return(x)
  }

  origin <- extract.coords("Origin = ", l)

  #  Extract extent
  # Note: the corner coordinates are heavily rounded
  approx.coords<- matrix(NA, nrow = 5, ncol = 2,
                         dimnames = list(c("Upper Left", "Lower Left",
                                           "Upper Right", "Lower Right", "Center"),
                                         c("1", "2")) )

  approx.coords[1, ]  <- extract.coords("Upper Left", l)
  approx.coords[2, ]  <- extract.coords("Lower Left", l)
  approx.coords[3, ]  <- extract.coords("Upper Right", l)
  approx.coords[4, ]  <- extract.coords("Lower right", l)
  approx.coords[5, ]  <- extract.coords("Center", l)

  if(!all(  abs(origin - approx.coords[1, ])  < abs(result$res) / 100))
    stop("I'm assuming that the origin is the upper left but that doesn't appear to be true.")

  result$origin <- origin
  result$approx.coords <- approx.coords



  # add convenience tags nbands and type
  #  derved from info in bands
  result$nbands <- length(result$bands)
  if(result$nbands == 1){
    result$type <- result$bands[[1]]$type
    result$na.value <- result$bands[[1]]$na.value
  }
  if(result$nbands > 1){
    types <- lapply(result$bands, function(x) x$type)
    names(types) <- NULL
    if(isTRUE(do.call(all.equal, types))){
      result$type <- result$bands[[1]]$type
    } else {
      result$type <- "mixed"
    }
    na.values <- lapply(result$bands, function(x) x$na.value)
    names(na.values) <- NULL
    if(isTRUE(do.call(all.equal, na.values))){
      result$na.value <- result$bands[[1]]$na.value
    } else {
      result$na.value <- NA
    }

  }
  if(!is.na(result$cellsize)){
    xll <- origin[1]
    yll <- origin[2] - ( result$cellsize * result$rows)
    if(!all(  abs(c(xll, yll) - approx.coords[2, ])  < abs(result$res) / 100))
      stop("xll and yll don't correspond to lower left corner. This file isn't structured the way  I assumed it was.")
    result$extent <- list(xll= xll, yll= yll, nrow = result$rows, ncol = result$cols, cellsize = result$cellsize)
  }



  result$raw <- raw

  class(result) <- c("rasterInfo", class(result))

  return(result)
} # end function



printnestedlist <- function(x, indent = 0, name){
  if(!is.list(x)){
    if(is.vector(x)){
      if(is.numeric(x)){
        spacer <- ", "
      } else {
        spacer <- paste0("\n", paste(rep("  ", indent-1), collapse = "", sep =""), paste(rep(" ", nchar(name)), collapse ="", sep =""), "   ")
      }
      cat(" = ", paste(x, collapse = spacer), "\n", sep="")
      #cat("[", nchar(spacer), "]\n")
    } else {
      cat(":\n")
      print(x)
    }
  } else {  # if it's a list
    if(!indent == 0) cat(":")
    cat("\n", sep ="")
    for(i in seq_along(x)){
      cat(paste(rep("  ", indent), collapse = ""), names(x)[i], sep ="")
      printnestedlist(x[[i]], indent = indent + 1, name = names(x)[i])
    }
  }
}




#' @rdname rasterInfo
#' @export
print.rasterInfo <- function(x, all = FALSE, raw = FALSE, ...){
  if(raw){
    cat(x$raw, sep = "\n")
    return()
  }


  x$raw  <- "[not printed]"

  if(!all){
    if(!is.na(x$crs))
      x$crs <- "[Not printed]"
    if(!is.na(x$files[1]))
      x$files <- "[Not printed]"
    if(!is.na(x$attributetable[1]))
      x$attributetable <- "[Not printed]"

  }

  printnestedlist(x)
  if(!all)
    cat("[Some items may have been skipped use print(x, all=TRUE) to print everything.]")
}



