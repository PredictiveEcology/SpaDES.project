#' `SpaDES.project` options
#'
#' These demonstrate default values for some options that can be set in
#' SpaDES.project.
#' To see defaults, run `spadesProjectOptions()`.
#' See Details below.
#'
#' @export
#' @return named list of the *default* options currently available.
#'
#' @details
#'
#' Below are options that can be set with `options("spades.xxx" = newValue)`,
#' where `xxx` is one of the values below, and `newValue` is a new value to
#' give the option. Sometimes these options can be placed in the user's `.Rprofile`
#' file so they persist between sessions.
#'
#' The following options are used, and can mostly be specified in the various `setup*`
#' functions also.
#' \tabular{lcl}{
#'   *OPTION* \tab *DEFAULT VALUE* \tab *DESCRIPTION* \cr
#'   `reproducible.cachePath`
#'      \tab NOTE: uses `reproducible`. Defaults is within projectPath, with subfolder "cache"  \cr
#'
#'   `spades.inputPath`
#'      \tab Default is within projectPath, with subfolder "inputs"  \cr
#'
#'   `spades.modulePath`
#'      \tab Default is within projectPath, with subfolder "modules"  \cr
#'
#'   `spades.outputPath`
#'      \tab Default is within projectPath, with subfolder "outputs"  \cr
#'
#'   `spades.packagePath`
#'      \tab Default to `.libPathDefault(<projectPath>)`  \cr
#'
#'   `spades.projectPath`
#'      \tab Default "."  \cr
#'
#'   `spades.scratchPath`
#'      \tab Default is within `tempdir()`, with subfolder <projectPath>  \cr
#'
#'   `SpaDES.project.Restart`
#'      \tab Default is FALSE. Passed to `Restart` argument in `setupProject`  \cr
#'
#'   `SpaDES.project.useGit`
#'      \tab Default is FALSE. Passed to `useGit` argument in `setupProject`  \cr
#'
#'
#' }
#'
#' `SpaDES.project.ask` is currently only used when offering to clone a remote
#' github repository. Setting this to `FALSE` will prevent asking and just "do it".
#'
spadesProjectOptions <- function() {
  pp <- getOption("spades.projectPath")
  if (is.null(pp))
    options("spades.projectPath" = ".")

  ppBasename <- normalizePath(basename(getOption("spades.projectPath")),
                              mustWork = FALSE, winslash = "/")
  defaults <- list( # nolint
    reproducible.cachePath = file.path(ppBasename, "cache"),
    spades.projectPath = ppBasename,
    spades.packagePath = .libPathDefault(ppBasename),
    spades.inputPath = file.path(ppBasename, "inputs"),
    spades.modulePath = file.path(ppBasename, "modules"),
    spades.outputPath = file.path(ppBasename, "outputs"),
    spades.scratchPath = file.path(tempdir(), basename(ppBasename))
    )
  defaults2 <- list(
    SpaDES.project.Restart = FALSE,
    SpaDES.project.useGit = FALSE,
    SpaDES.project.ask = TRUE,
    SpaDES.project.gitignore = TRUE,
    SpaDES.project.setLinuxBinaryRepo = TRUE,
    SpaDES.project.standAlone = TRUE,
    SpaDES.project.updateRprofile = TRUE,
    SpaDES.project.overwrite = FALSE,
    SpaDES.project.fast = FALSE
  )
  defaults <- Map(def = defaults, function(def) normPath(def))
  defaults <- append(defaults, defaults2)
  defaults
}
