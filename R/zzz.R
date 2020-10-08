.onAttach <- function(libname, pkgname) {
  # module template path
  .pkgEnv[["templatePath"]] <- system.file("templates", package = "SpaDES.project")
}
