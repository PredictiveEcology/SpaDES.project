#' An alternative to `pkgload::load_all` with caching
#'
#' `pkgload::load_all` does not automatically deal with dependency chains: the
#' user must manually load the dependency chain in order with separate calls to
#' `pkgload::load_all`. Also, it does not use caching. This function allows
#' nested caching for a sequence of packages that depend on one another. For
#' example, if a user has 3 packages that have dependency chain:
#' A is a dependency of B which is a dependency of C.
#' If a change happens in C, then pkgload::load_all will only be called on C.
#' If a change happens in A, then pkgload::load_all will be called on A, then B, then C.
#' @param depsPaths A character vector of paths to packages that need loading, or list
#'   of these. Each vector should be the load order sequence, based on the package
#'   dependencies, i.e., the first element in the vector should be a dependency of the
#'   second element in the vector etc. For packages that do not depend on each
#'   other, use separate list elements.
#' @param envir An environment where an object called .prevDigs that will be placed
#'   and used as a cache comparison.
#' @export
#' @importFrom utils tar
#' @return
#' This is called for its side effects, which are 2: pkgload::load_all on the
#' packages that need it, and an object, `.prevDigs` that is assigned to `envir`.
#'
pkgload2 <- function(depsPaths = file.path("~/GitHub", c("reproducible", "SpaDES.core", "LandR")),
                     envir = parent.frame()) {
  pasts <- get0(".prevDigs", envir = envir)
  if (is.null(pasts)) {
    pasts <- character()
    #pasts <- lapply(seq_along(depsPaths), function(x) "")
    #names(pasts) <- basename(depsPaths)
  }
  if (!is.list(depsPaths)) {
    depsPaths <- list(depsPaths)
  }
  depsAll <- depsPaths
  if (!requireNamespace("digest")) stop("Please install digest")

  for (dpOuter in depsAll) {
    depsPaths <- dpOuter
    for (dp in depsPaths) {
      dpBase <- basename(dp)
      left <- depsPaths[match(dp, depsPaths):length(depsPaths)]
      digRepro <- Map(n = basename(left), l = left, function(l, n) {
        tf <- tempfile(fileext = ".tar")
        zf <- tar(tarfile = tf, dir(file.path(l, "R"), full.names = TRUE, pattern = "\\.R$"))
        dig <- digest::digest(file = tf)
        unlink(tf)
        dig
      })
      if (tryCatch(!identical(pasts[[dpBase]], digRepro[[dpBase]]), error = function(x) TRUE)) {
        for (p in left) {
          if (!requireNamespace("pkgload")) stop("Please install pkgload")
          pkgload::load_all(p)
          pasts[basename(p)] <- digRepro[basename(p)]
        }
        break
      }
    }
  }
  assign(".prevDigs", pasts, envir = envir)
}
