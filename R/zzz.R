.onAttach <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.spadesproj <- list(
    SpaDES.project.gitignore = TRUE,
    SpaDES.project.messageWarnStop = "message",
    SpaDES.project.Restart = FALSE,
    SpaDES.project.updateSelf = NULL
  )
  toset <- !(names(opts.spadesproj) %in% names(opts))
  if (any(toset)) options(opts.spadesproj[toset])

  NULL
}
