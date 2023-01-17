#' non-exported objects and functions from other packages
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @rdname imports
messageDF <- utils::getFromNamespace("messageDF", "Require")

#' @rdname imports
messageVerbose <- utils::getFromNamespace("messageVerbose", "Require")

#' @rdname imports
downloadRepo <- utils::getFromNamespace("downloadRepo", "Require")

#' @rdname imports
.spatialPkgs <- tryCatch({
  .spatialPkgs <- utils::getFromNamespace(".spatialPkgs", "Require")
}, error = function(e) {
  c("lwgeom", "raster", "rgdal", "rgeos", "s2", "sf", "sp", "terra",
    "units")
}) ## in case using older version of Require w/o .spatialPkgs defined
.spatialPkgs[-which(.spatialPkgs %in% c("units"))]
