#' non-exported objects and functions from other packages
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @rdname imports
splitGitRepo <- utils::getFromNamespace("splitGitRepo", "Require")

#' @rdname imports
getGitHubFile <- utils::getFromNamespace("getGitHubFile", "Require")

#' @rdname imports
isWindows <- utils::getFromNamespace("isWindows", "Require")

#' @rdname imports
messageDF <- utils::getFromNamespace("messageDF", "Require")

#' @rdname imports
.spatialPkgs <- tryCatch({
  .spatialPkgs <- utils::getFromNamespace(".spatialPkgs", "Require")
}, error = function(e) {
  c("lwgeom", "raster", "rgdal", "rgeos", "s2", "sf", "sp", "terra",
    "units")
}) ## in case using older version of Require w/o .spatialPkgs defined
.spatialPkgs[-which(.spatialPkgs %in% c("units"))]
