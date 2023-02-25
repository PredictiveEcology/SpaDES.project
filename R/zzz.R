.onAttach <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.spadesproj <- list(
    # Require.updateRprofile = FALSE, ## TODO: this is used, with default FALSE
    # SpaDES.project.Restart = FALSE, ## TODO: this is used, with default FALSE
    SpaDES.project.updateGitIgnore = TRUE
  )
  toset <- !(names(opts.spadesproj) %in% names(opts))
  if (any(toset)) options(opts.spadesproj[toset])

  # module template path
  .pkgEnv[["templatePath"]] <- system.file("templates", package = "SpaDES.project")
}
