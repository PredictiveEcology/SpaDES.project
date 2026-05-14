#' Partially or Fully Run `setupProject`
#'
#' \code{preRunSetupProject} parses an R script (default: \code{"global.R"}) and
#' evaluates its contents up to the \code{setupProject()} call, either fully or
#' partially based on the \code{upTo} argument. This is useful for initializing
#' only certain parts of a project without executing the entire setup.
#'
#' @param file Character string. Path to the R script containing the setup code.
#'   Defaults to \code{"global.R"}.
#' @param upTo Character or logical. If \code{TRUE}, evaluates all code up to and
#'   including the first \code{setupProject()} call within `file`.
#'   If a character string, only evaluates the code up to the `setupProject` plus
#'   the arguments up to the `upTo` named argument. Defaults to \code{"paths"} so
#'   that `paths` will be evaluated and availble to use.
#' @param envir The environment where the function should be finding objects. Defaults
#'   to `parent.frame()` so it can find them in the calling frame.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Parses the specified file using \code{parse()}.
#'   \item Identifies the line where \code{setupProject()} is called.
#'   \item Evaluates all code before the \code{setupProject()} call.
#'   \item Depending on \code{upTo}, evaluates either the full call or a subset
#'         of its arguments.
#' }
#'
#' This allows selective initialization of project components for debugging or
#' partial setup in large projects.
#'
#' @return The evaluated result of the executed portion of \code{setupProject()}.
#'   i.e., a `list` returned by \code{setupProject()}.
#'
#' @examples
#' \dontrun{
#' # Run file up to and including the setupProject, but only to the 'paths' argument
#' result <- preRunSetupProject(file = "global.R", upTo = "paths")
#'
#' # Run file up to and including full setupProject()
#' result <- preRunSetupProject(file = "global.R", upTo = TRUE)
#' }
#'
#' @seealso \code{\link{setupProject}}
#'
#' @export
preRunSetupProject <- function(file = "global.R", upTo = TRUE, envir = parent.frame()) {
  pp <- parse(file)
  whSetupProject <- grep("setupProject", pp)

  eval(pp[1:c(whSetupProject - 1)], envir = envir)
  if (isTRUE(upTo) || is.null(upTo) || identical(upTo, "")) {
    outs <-   eval(pp[whSetupProject], envir = envir)
  } else {
    upToNum <- grep(upTo, names(pp[[whSetupProject]][[3]]))
    outs <- eval(pp[[whSetupProject]][[3]][1:upToNum], envir = envir)
  }
  outs
}
