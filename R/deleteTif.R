


#' Function to delete one or more .tif files and all associated files
#'
#' This will delete the files listed in `x` and all associated files,
#' specifically: \itemize{
#'   \item any file that includes the full name with .tif extension and an
#'    additional extension  e.g. .../name.tif.ovr.
#'   \item a world file which has the same base name but a ".twf" extension
#'   instead of ".tif"
#'   \item if `includeVrt` is TRUE a .vrt file with the same name.
#' }
#' @param x (character) the path to one or more .tif files.
#' @param includeVrt if TRUE (the default) .vrt files with the same base name
#' will also be deleted.
#' @return this function returns nothing
#' @export
deleteTif <- function(x, includeVrt = TRUE) {
  # Delete the target files and any associated files (same name with
  # extensions tacked on) or same base name with ".tfw" instead of ".tif"
  stopifnot(all(grepl("\\.tif$", x, ignore.case = TRUE)))
  for (i in seq_along(x)) {
    targets <- findTifFiles(x[i], includeVrt = includeVrt)
    if (rasterPrepOptions()$verbose)
      cat("Deleting:\n\t\t", paste(targets, collapse = "\n\t\t"), "\n",
          sep = "")
    file.remove(targets)
  }
}
