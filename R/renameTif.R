#' Function to rename or move a .tif and any associated sidecar files
#'
#' @param from (character) path to a tif to be renamed
#' @param to (character) path to renamed tif
#' @param includeVrt (logical) if TRUE also rename an associated .vrt file with
#'   same base name.
#' @return this function returns nothing
#' @export
renameTif <- function(from, to, includeVrt = TRUE) {
  # Rename a tif file (and it's associated files)
  stopifnot(grepl(".tif$", from, ignore.case = TRUE))
  targets <- findTifFiles(from, includeVrt = TRUE)
  a <- gsub(".tif$", "", from, ignore.case = TRUE)
  b <- gsub(".tif$", "", to, ignore.case = TRUE)
  new <- gsub(a, b, targets)
  if (rasterPrepOptions()$verbose)
    print(data.frame(from = targets, to = new))
  file.rename(targets, new)
}
