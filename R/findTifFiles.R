#' Find all the files associated with a TIFF file.
#'
#' This is primarily intended for internal use and is called by
#' [copyTif()],[deleteTif()], and [renameTif()].
#'
#' Given the path to a TIFF file (*.tif* extension) return the path to all
#' the associated files, including the original **.tif**.  Any file that
#'  includes the full name (with **.tif** extension and some additional
#' extension  e.g. *.tif.ovr*) a world
#' file which has the same base name but *.twf* instead of *.tif* and if
#' `includeVrt` is TRUE a *.vrt* file with the same name.
#'
#' @param x (character) the path to a single TIFF file.
#' @param includeVrt (logical) if TRUE consider a *.vrt* with the same base name
#'   to be associated.
#' @return a vector of files associated with `x`
#' @export
#' @keywords internal
findTifFiles <- function(x, includeVrt = TRUE) {
  name <- basename(x)
  dir <- dirname(x)

  matches <- list.files(dir, pattern = paste0("^", name), full.names = TRUE,
                        ignore.case = TRUE)


  alternativeExtensions <- ".tfw"
  if (includeVrt)
    alternativeExtensions <- c(alternativeExtensions, ".vrt")

  for (ext in alternativeExtensions) {
    a <- gsub(".tif$", ext, x, ignore.case = TRUE)
    if (file.exists(a))
      matches <- c(matches, a)
  }
  return(matches)
}
