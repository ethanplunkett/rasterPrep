.onLoad <- function(lib, pkg) {
  rpProj <- Sys.getenv("RASTERPREP_PROJ")
  if (!is.na(rpProj) && !rpProj == "") {
    rasterPrepOptions(projLib = rpProj)
  }
}
