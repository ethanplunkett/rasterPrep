#' Copy a TIFF and all its associated files
#'
#' @param from (character) path to a TIFF to be copied
#' @param to (character) path where copy will be created
#' @param includeVrt (logical) if `TRUE` also copy an associated `.vrt` file
#' with same base name.
#' @param overwrite (logical) if `TRUE` overwrite existing file if `FALSE`
#' don't.
#' @return Nothing is returned.
#' @export

copyTif <- function(from, to, includeVrt = TRUE, overwrite = FALSE) {
  # Rename a tif file (and it's associated files)
  stopifnot(grepl(".tif$", from, ignore.case = TRUE))
  targets <- findTifFiles(from, includeVrt = includeVrt)
  a <- gsub(".tif$", "", from, ignore.case = TRUE)
  b <- gsub(".tif$", "", to, ignore.case = TRUE)
  new <- gsub(a, b, targets, ignore.case = TRUE)
  if (rasterPrepOptions()$verbose)
    print(data.frame(from = targets, to = new))
  file.copy(from = targets, to = new, overwrite = overwrite)
}
