utils::globalVariables(c(
  "..apCachedCols", ".N", "N", "Package", "VersionOK", "filenameFromFunction",
  "i.VersionOnRepos", "inequality", "keepBasedOnRedundantInequalities"
))


#' non-exported objects and functions from other packages
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @rdname imports
messageDF <- utils::getFromNamespace("messageDF", "Require")

#' @rdname imports
yellow <- utils::getFromNamespace("yellow", "Require")

#' @rdname imports
blue <- utils::getFromNamespace("blue", "Require")

#' @rdname imports
green <- utils::getFromNamespace("green", "Require")

#' @rdname imports
whereInStack <- utils::getFromNamespace("whereInStack", "Require")

#' @rdname imports
messageVerbose <- utils::getFromNamespace("messageVerbose", "Require")

#' @rdname imports
compareVersion2 <- utils::getFromNamespace("compareVersion2", "Require")

#' @rdname imports
downloadRepo <- utils::getFromNamespace("downloadRepo", "Require")

#' @rdname imports
splitGitRepo <- utils::getFromNamespace("splitGitRepo", "Require")

#' @rdname imports
isGitHub <- utils::getFromNamespace("isGitHub", "Require")

#' @rdname imports
fileRenameOrMove <- utils::getFromNamespace("fileRenameOrMove", "Require")

#' @rdname imports
linkOrCopy <- getFromNamespace("linkOrCopy", ns = "Require")

#' @rdname imports
.basePkgs <- getFromNamespace(".basePkgs", ns = "Require")

#' @rdname imports
DESCRIPTIONFileDeps <- getFromNamespace("DESCRIPTIONFileDeps", ns = "Require")

#' @rdname imports
getVersionOnRepos <- getFromNamespace("getVersionOnRepos", ns = "Require")

#' @rdname imports
substitutePackages <- getFromNamespace("substitutePackages", ns = "Require")

#' @rdname imports
toPkgDTFull <- getFromNamespace("toPkgDTFull", ns = "Require")

#' @rdname imports
#' @importFrom Require tempfile2 .downloadFileMasterMainAuth
trimRedundancies <- getFromNamespace("trimRedundancies", ns = "Require")

#' @rdname imports
DESCRIPTIONFileVersionV <- getFromNamespace("DESCRIPTIONFileVersionV", ns = "Require")



#' @rdname imports
.spatialPkgs <- tryCatch({
  .spatialPkgs <- utils::getFromNamespace(".spatialPkgs", "Require")
}, error = function(e) {
  c("lwgeom", "raster", "rgdal", "rgeos", "s2", "sf", "sp", "terra",
    "units")
}) ## in case using older version of Require w/o .spatialPkgs defined
.spatialPkgs[-which(.spatialPkgs %in% c("units"))]
