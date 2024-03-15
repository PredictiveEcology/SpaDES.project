#' Set the package directory for a project
#'
#' This function will create a sub-folder of the `lib.loc` directory that
#' is based on the R version and the platform, as per the standard R package directory
#' naming convention
#' @param lib.loc The folder for installing packages inside of
#' @inheritParams Require::Require
#' @export
setProjPkgDir <- function(lib.loc = "packages",
                          verbose = getOption("Require.verbose", 1L)) {
  pkgDir <- Sys.getenv("PRJ_PKG_DIR")
  if (!nzchar(pkgDir)) {
    pkgDir <- lib.loc ## default: use subdir within project directory
  }
  pkgDir <- normalizePath(
    file.path(pkgDir, version$platform, paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }

  .libPaths(pkgDir)
  messageVerbose("Using libPaths:\n", paste(.libPaths(), collapse = "\n"),
                 verbose = verbose)
}
