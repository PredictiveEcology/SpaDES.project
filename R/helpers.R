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
