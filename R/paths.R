###########################################################
# These functions are from Require, but not exported here
###########################################################

#' Normalize filepath
#'
#' Checks the specified filepath for formatting consistencies:
#'  1) use slash instead of backslash;
#'  2) do tilde etc. expansion;
#'  3) remove trailing slash.
#'
#' @param path A character vector of filepaths.
#'
#' @return Character vector of cleaned up filepaths.
#'
#' @rdname normPath
#'
#'
setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

#' @rdname normPath
setMethod("normPath",
          signature(path = "character"),
          definition = function(path) {
            if (length(path) > 0) {
              nas <- is.na(path)
              if (any(!nas)) {
                path[!nas] <- normalizePath(path[!nas], winslash = "/", mustWork = FALSE)
              }
              if (any(nas)) {
                path[nas] <- NA_character_
              }

              # path <- lapply(path, function(x) {
              #   if (is.na(x)) {
              #     NA_character_
              #   } else {
              #     normalizePath(x, winslash = "/", mustWork = FALSE)
              #   }
              # })
              # Eliot changed this Sept 24, 2019 because weird failures with getwd()
              # in non-interactive testing
              path <- unlist(path)
              if (!is.null(path)) {
                hasDotStart <- startsWith(path, "./")
                if (isTRUE(any(hasDotStart)))
                  path[hasDotStart] <- gsub("^[.]/", paste0(getwd(), "/"), path[hasDotStart])
                path <- gsub("\\\\", "//", path)
                path <- gsub("//", "/", path)
                path <- gsub("/$", "", path) # nolint
              }
            }
            return(path)
})

#' @rdname normPath
setMethod("normPath",
          signature(path = "list"),
          definition = function(path) {
            return(normPath(unlist(path)))
})

#' @rdname normPath
setMethod("normPath",
          signature(path = "NULL"),
          definition = function(path) {
            return(character(0))
})

#' @rdname normPath
setMethod("normPath",
          signature(path = "missing"),
          definition = function() {
            return(character(0))
})

################################################################################
#' Check directory path
#'
#' Checks the specified path to a directory for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @note This will not work for paths to files.
#' To check for existence of files, use \code{\link{file.exists}}.
#' To normalize a path to a file, use \code{\link{normPath}} or \code{\link{normalizePath}}.
#'
#' @param path A character string corresponding to a directory path.
#'
#' @param create A logical indicating whether the path should
#' be created if it does not exist. Default is \code{FALSE}.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso \code{\link{file.exists}}, \code{\link{dir.create}}.
#'
#' @rdname checkPath
#'
#'
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "logical"),
  definition = function(path, create) {
    # if (length(path) != 1) {
    #   stop("path must be a character vector of length 1.")
    # } else {
    if (isTRUE(all(is.na(path)))) {
      stop("Invalid path: cannot be NA.")
    } else {
      path <- normPath(path) # this is necessary to cover Windows double slash used on non-Windows
      dirsThatExist <- dir.exists(path)
      if (any(!dirsThatExist)) {
        isExistingFile <- file.exists(path)
        if (all(isExistingFile)) {
          message("That path is an existing file(s)")
        } else {
          if (create == TRUE) {
            lapply(path[!dirsThatExist[!isExistingFile]], function(pth) {
              dir.create(file.path(pth), recursive = TRUE, showWarnings = FALSE)
            })
          } else {
            stop(paste("Specified path", normPath(path), "does not exist.",
                       "Create it and try again."))
          }
        }
      }
      if (Sys.info()[["sysname"]] == "Darwin")
        path <- normPath(path) # ensure path re-normalized after creation

      return(path)
    }
    #}
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path = "character", create = "missing"),
          definition = function(path) {
            return(checkPath(path, create = FALSE))
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path = "NULL", create = "ANY"),
          definition = function(path) {
            stop("Invalid path: cannot be NULL.")
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path = "missing", create = "ANY"),
          definition = function() {
            stop("Invalid path: no path specified.")
})


#' Set the package directory for a project
#'
#' This function will create a sub-folder of the `lib.loc` directory that
#' is based on the R version and the platform, as per the standard R package directory
#' naming convention
#' @param lib.loc The folder for installing packages inside of
#' @export
setProjPkgDir <- function(lib.loc = "packages") {
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
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))
}
