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

#' Helpers for cleanup of global state in examples and tests
#'
#' 1. remove project library directory created using `setupProject()`;
#' 2. remove project paths created using `setupProject`;
#' 3. restore original library paths.
#'
#' Detect an interactive SSH PTY session
#'
#' Returns `TRUE` when R is running inside an SSH session that allocated a
#' pseudo-terminal (i.e., started with `ssh -t`).  The SSH server sets the
#' `SSH_TTY` environment variable to the slave PTY device path only in this
#' case; it is absent in plain `ssh` (no PTY), RStudio, local tmux, etc.
#'
#' This matters because `parallelly::makeClusterPSOCK()` spawns Rscript worker
#' processes that inherit the PTY slave file descriptors.  R resets its SIGHUP
#' handler during startup (overriding any inherited SIG_IGN), so if the PTY
#' reference count reaches zero after workers exit, the resulting SIGHUP kills
#' R silently.  Use this function to guard `makeClusterPSOCK` calls:
#'
#' ```r
#' if (!inSshPty()) {
#'   cl <- parallelly::makeClusterPSOCK(cores, ...)
#' } else {
#'   message("SSH PTY session: skipping makeClusterPSOCK (run sequentially)")
#'   cl <- NULL
#' }
#' ```
#'
#' @return Logical scalar: `TRUE` if `SSH_TTY` is set (non-empty).
#' @export
inSshPty <- function() nzchar(Sys.getenv("SSH_TTY"))

#' @note not intended to be called by users
#'
#' @param prjPaths character vector of paths to be removed
#'
#' @param origLibPaths character string giving the original library path to be restored
#'
#' @return NULL. Invoked for its side effects.
#'
#' @export
#' @importFrom fs path_has_parent path_rel path_split
#' @importFrom Require normPath
#' @importFrom tools R_user_dir
#' @importFrom utils head
#' @rdname test-helpers
.teardownProject <- function(prjPaths, origLibPaths) {
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

  return(NULL)
}
