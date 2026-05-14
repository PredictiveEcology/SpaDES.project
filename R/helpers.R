#' Find the project root directory
#'
#' Searches from current working directory for and Rstudio project file
#' or git repository, falling back on using the current working directory.
#'
#' @return `findProjectPath` returns an absolute path;
#'         `findProjectName` returns the basename of the path.
#'
#' @export
#' @importFrom rprojroot find_root from_wd is_git_root is_rstudio_project
#' @rdname findProject
findProjectPath <- function() {
  find_root(is_rstudio_project | is_git_root | from_wd, path = getwd())
}

#' @export
#' @rdname findProject
findProjectName <- function() {
  basename(findProjectPath())
}

isAbsolutePath <- function(pathnames) {
  keep <- is.character(pathnames)
  if (isFALSE(keep))
    stop("pathnames must be character")
  nams <- names(pathnames)
  ret <- fs::is_absolute_path(pathnames)
  if (!is.null(nams))
    names(ret) <- nams
  return(ret)
  # origPn <- pathnames
  # nPathnames <- length(pathnames)
  # if (nPathnames == 0L)
  #   return(logical(0L))
  # if (nPathnames > 1L) {
  #   res <- sapply(pathnames, FUN = isAbsolutePath)
  #   return(res)
  # }
  # if (is.na(pathnames))
  #   return(FALSE)
  # if (regexpr("^~", pathnames) != -1L)
  #   return(TRUE)
  # if (regexpr("^.:(/|\\\\)", pathnames) != -1L)
  #   return(TRUE)
  # components <- strsplit(pathnames, split = "[/\\]")[[1L]]
  # if (length(components) == 0L)
  #   return(FALSE)
  # (components[1L] == "")
}


#' Helpers to develop easier to understand code.
#'
#' A set of lightweight helpers that are often not strictly necessary, but they
#' make code easier to read.
#'
#' @export
#' @rdname helpers
#' @param username A character string of a username.
#' @return if `username` is non-NULL, returns a logical indicating whether
#' the current user matches the supplied `username`.
#' Otherwise returns a character string with the value of the current user.
user <- function(username = NULL) {
  if (is.null(username)) {
    Sys.info()[["user"]]
  } else {
    identical(username, Sys.info()[["user"]])
  }
}

#' @param machinename A character string, which will be used as a partial match via
#' `grep`, so the entire machine name is not necessary. A user can use `regex` if
#' needed, e.g., `"^machine1"` will match `"machine15"` and `"machine12"`, but not
#' `"thisIs_machine1"`.
#' @export
#' @rdname helpers
#' @return
#' `machine` returns a logical indicating whether the current machine name
#' `Sys.info()[["nodename"]]` is matched by `machinename`.
machine <- function(machinename = NULL) {
  if (is.null(machinename)) {
    Sys.info()[["nodename"]]
  } else {
    grepl(machinename, Sys.info()[["nodename"]])
  }
}

#' @export
#' @rdname helpers
#' @details
#' `node` is an alias for `machine`
node <- machine

#' Tear down a project created by `setupProject()`
#'
#' Reverse the side-effects of [setupProject()]:
#'
#' 1. remove the project library directory created by `setupProject()`,
#' 2. unlink the project paths returned by `setupProject()`,
#' 3. restore the `.libPaths()` value that was in effect before
#'    `setupProject()` was called.
#'
#' The previous `.libPaths()` is stored on the `setupProject()` output as
#' `out$paths$.previousLibPaths` (and on `attr(out$paths, "extraPaths")`),
#' so `teardownProject(out)` is enough -- no need to remember
#' `origLibPaths` separately.
#'
#' @param x Either the list returned by [setupProject()], or a character
#'   vector of paths to remove (back-compat with the previous
#'   `.teardownProject(prjPaths, origLibPaths)` signature).
#' @param origLibPaths Optional. The `.libPaths()` to restore. Defaults to
#'   `x$paths$.previousLibPaths` when `x` is a `setupProject()` output, so
#'   most callers will not need to supply this.
#'
#' @return `NULL`, invisibly. Called for its side effects.
#'
#' @seealso [setupProject()] for what is being torn down.
#'
#' @export
#' @importFrom fs path_has_parent path_rel path_split
#' @importFrom Require normPath
#' @importFrom tools R_user_dir
#' @importFrom utils head
teardownProject <- function(x, origLibPaths) {
  if (is.list(x) && !is.null(x[["paths"]])) {
    prjPaths <- x[["paths"]]
    if (missing(origLibPaths)) {
      origLibPaths <- prjPaths[[".previousLibPaths"]]
      if (is.null(origLibPaths))
        origLibPaths <- attr(prjPaths, "extraPaths")[[".previousLibPaths"]]
    }
  } else {
    prjPaths <- x
  }
  if (missing(origLibPaths) || is.null(origLibPaths))
    stop("`origLibPaths` not found; pass it explicitly or supply a setupProject() output that carries `.previousLibPaths`.",
         call. = FALSE)

  curlibpath <- utils::head(.libPaths(), 1) |> Require::normPath()
  donttouch <- c(
    "~/R-dev",
    Sys.getenv("R_LIBS_USER"),
    Sys.getenv("R_LIBS_SITE") |> strsplit(":") |> unlist(),
    Sys.getenv("R_LIBS")
  )
  donttouch <- donttouch[nzchar(donttouch)] |>
    normPath() |>
    unique()

  if (!curlibpath %in% donttouch) {
    userDirRoot <- normPath(R_user_dir(""))
    if (path_has_parent(curlibpath, userDirRoot)) {
      prjLibPathRoot <- path_rel(curlibpath, userDirRoot) |>
        path_split() |>
        unlist() |>
        head(1)
      try(unlink(file.path(userDirRoot, prjLibPathRoot), recursive = TRUE))
    } else {
      try(unlink(curlibpath, recursive = TRUE))
    }
  }

  try(unlink(unlist(prjPaths), recursive = TRUE))

  .libPaths(origLibPaths)

  invisible(NULL)
}

#' @rdname teardownProject
#' @export
.teardownProject <- function(prjPaths, origLibPaths) {
  teardownProject(prjPaths, origLibPaths)
}
