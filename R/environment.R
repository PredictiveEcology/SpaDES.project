#' The `SpaDES.project` package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
#'
.pkgEnv <- new.env(parent = emptyenv())

#' Inspect the call stack from the most recent worker error
#'
#' Worker panes started by `experimentTmux()` / `tmuxRunNextWorker()` evaluate
#' the user's `global.R` inside a fresh scenario environment (not `.GlobalEnv`).
#' When the source call errors, the package captures `sys.calls()` and stashes
#' it on that scenario env so a post-mortem traceback is still possible without
#' polluting the user's global state. Use this accessor to retrieve it.
#'
#' @return A list of calls (as from `sys.calls()`) suitable for passing to
#'   [base::traceback()]; `NULL` if no error has been captured in the current
#'   session.
#' @examples
#' \dontrun{
#'   # After a worker pane errors:
#'   traceback(SpaDES.project::lastTraceback())
#' }
#' @export
lastTraceback <- function() {
  scn <- .pkgEnv$lastScn
  if (is.null(scn)) return(NULL)
  scn$.spades_tb
}
