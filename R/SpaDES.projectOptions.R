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
#' functions also. The *DEFAULT VALUE* column lists the value returned by
#' `spadesProjectOptions()`, which is also the fallback used by `setupProject()`
#' (and the inner `setup*` functions) when the option is not otherwise set.
#' \tabular{lll}{
#'   *OPTION* \tab *DEFAULT VALUE* \tab *DESCRIPTION* \cr
#'   `reproducible.cachePath` \tab `<projectPath>/cache` \tab
#'      NOTE: uses `reproducible`. Within `projectPath`, with subfolder "cache" \cr
#'
#'   `spades.inputPath` \tab `<projectPath>/inputs` \tab
#'      Within `projectPath`, with subfolder "inputs" \cr
#'
#'   `spades.modulePath` \tab `<projectPath>/modules` \tab
#'      Within `projectPath`, with subfolder "modules" \cr
#'
#'   `spades.outputPath` \tab `<projectPath>/outputs` \tab
#'      Within `projectPath`, with subfolder "outputs" \cr
#'
#'   `spades.packagePath` \tab `.libPathDefault(<projectPath>)` \tab
#'      The project-level package library \cr
#'
#'   `spades.projectPath` \tab `"."` \tab
#'      The project root (current directory by default) \cr
#'
#'   `spades.scratchPath` \tab `<tempdir()>/<projectPath>` \tab
#'      Within `tempdir()`, with subfolder `basename(projectPath)` \cr
#'
#'   `SpaDES.project.Restart` \tab `FALSE` \tab
#'      Passed to the `Restart` argument in `setupProject` \cr
#'
#'   `SpaDES.project.useGit` \tab `FALSE` \tab
#'      Passed to the `useGit` argument in `setupProject` \cr
#'
#'   `SpaDES.project.setLinuxBinaryRepo` \tab `TRUE` \tab
#'      Passed to the `setLinuxBinaryRepo` argument in `setupProject` \cr
#'
#'   `SpaDES.project.standAlone` \tab `TRUE` \tab
#'      Passed to the `standAlone` argument in `setupProject` \cr
#'
#'   `SpaDES.project.updateRprofile` \tab `TRUE` \tab
#'      Passed to the `updateRprofile` argument in `setupProject` \cr
#'
#'   `SpaDES.project.overwrite` \tab `FALSE` \tab
#'      Passed to the `overwrite` argument in `setupProject` \cr
#'
#'   `SpaDES.project.ask` \tab `TRUE` \tab
#'      If `FALSE`, do not prompt before cloning a remote GitHub repository \cr
#'
#'   `SpaDES.project.gitignore` \tab `TRUE` \tab
#'      Whether `setupProject` manages a `.gitignore` for the project \cr
#'
#'   `SpaDES.project.fast` \tab `FALSE` \tab
#'      If `TRUE`, use `fastOptions()` and skip package/git checks (see `setupProject`) \cr
#'
#' }
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
