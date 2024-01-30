#' Function to snap a coordinate to the edge of a cell.
#'
#' This function is primarily for internal use by [makeReference()]
#'
#' If offset is 0 it will snap to cells aligned with the origin, otherwise
#' offset should be the positive offset from the origin to the first cell
#' boundary.
#'
#' @param x a vector of coordinates (either x or y component) to be snapped to
#' a cell boundary
#' @param cellsize the cellsize of the grid being snapped.
#' @param up if `TRUE` then the number is snapped up to the nearest cell
#' boundary if `FALSE` it is snapped down.
#' @param offset the distance up from the origin to the first cell boundary.
#'
#' @return a vector of number close to `x` that fall on cell boundaries
#' if `up` is `TRUE` they will be equal to or higher
#' than corresponding values in `x`
#' if `FALSE` they will be the equal or lower.
#' @export
#' @keywords internal
snapToEdge <- function(x, cellsize, up = TRUE, offset = 0) {
  x2 <- ((x - offset) %/% cellsize) * cellsize  + offset
  if (up) {
    sv <- x2 < x
    x2[sv] <- x2[sv] + cellsize
  }
  x2
}
