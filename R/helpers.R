#' Find the project root directory
#'
#' Searches from current working directory for and Rstudio project file
#' or git repository.
#'
#'
#' @return `findProjectPath` returns an absolute path;
#'         `findProjectName` returns the basename of the path.
#'
#' @export
#' @importFrom rprojroot find_root is_git_root is_rstudio_project
#' @rdname findProject
findProjectPath <- function() {
  find_root(is_rstudio_project | is_git_root, path = getwd())
}

#' @export
#' @rdname findProject
findProjectName <- function() {
  basename(findProjectPath())
}


isAbsolutePath <- function (pathnames)
{
  keep <- is.character(pathnames)
  if (isFALSE(keep))
    stop("pathnames must be character")
  origPn <- pathnames
  nPathnames <- length(pathnames)
  if (nPathnames == 0L)
    return(logical(0L))
  if (nPathnames > 1L) {
    res <- sapply(pathnames, FUN = isAbsolutePath)
    return(res)
  }
  if (is.na(pathnames))
    return(FALSE)
  if (regexpr("^~", pathnames) != -1L)
    return(TRUE)
  if (regexpr("^.:(/|\\\\)", pathnames) != -1L)
    return(TRUE)
  components <- strsplit(pathnames, split = "[/\\]")[[1L]]
  if (length(components) == 0L)
    return(FALSE)
  (components[1L] == "")
}

.rndstr <- function (n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <- paste0(sample(c(0:9, letters, LETTERS), size = len,
                       replace = TRUE), collapse = "")
  }))
}


#' Helpers to develop easier to understand code.
#'
#' A set of lightweight helpers that are often not strictly necessary, but they
#' make code easier to read.
#'
#' @export
#' @rdname helpers
#' @param username A character string of a username.
#' @return
#' `user` returns a logical indicating whether the current user matches
#' the supplied `username`.
#'
user <- function (username = NULL) {
  if (is.null(username)) {
    Sys.info()[["user"]]
  }
  else {
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
#' Sys.info()[["nodename"]] is matched by `machinename`.
machine <- function(machinename = NULL) {
  if (is.null(machinename)) {
    Sys.info()[["nodename"]]
  }
  else {
    grepl(machinename, Sys.info()[["nodename"]])
  }
}


#' @export
#' @rdname helpers
#' @details
#' `node` is an alias for `machine`
node <- machine
