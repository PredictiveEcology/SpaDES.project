#' `SpaDES.project` options
#'
#' These provide top-level, powerful settings for a comprehensive SpaDES workflow.
#' To see defaults, run `spadesProjectOptions()`.
#' See Details below.
#'
#' @export
#' @return named list of the *default* package options.
#'
#' @details
#'
#' Below are options that can be set with `options("spadesProject.xxx" = newValue)`,
#' where `xxx` is one of the values below, and `newValue` is a new value to
#' give the option. Sometimes these options can be placed in the user's `.Rprofile`
#' file so they persist between sessions.
#'
#' The following options are used, using the prefix: `spades`
#' \tabular{lcl}{
#'   *OPTION* \tab *DEFAULT VALUE* \tab *DESCRIPTION* \cr
#'   `spades.logPath`
#'      \tab Defaults to a subdirectory (`logs/`) of the simulation output directory.
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   `spades.inputPath`
#'      \tab Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/inputs`)
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   `spades.modulePath` \tab `file.path(tempdir(), "SpaDES", "modules")`)
#'     \tab The default local directory where modules and data will be downloaded and stored.
#'     Default is a temporary directory  \cr
#'
#'   `spades.outputPath`
#'     \tab `file.path(tempdir(), "SpaDES", "outputs")`
#'     \tab The default local directory in which to save simulation outputs.\cr
#'
#'   `spades.useRequire` \tab `!tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false"`
#'     \tab : The default for that environment variable is unset, so this returns
#'     `TRUE`. If this is `TRUE`, then during the `simInit` call, when packages are
#'     identified as being required, these will be installed if missing, only if
#'     `spades.useRequire` option is `TRUE`, otherwise, `simInit` will fail because
#'     packages are not available.\cr
#'
#' }
#'
spadesProjectOptions <- function() {
  pp <- getOption("spades.projectPath")
  if (is.null(pp))
    options("spades.projectPath" = ".")

  ppBasename <- normalizePath(basename(getOption("spades.projectPath")), mustWork = FALSE)
  list( # nolint
    spades.projectPath = getOption("spades.projectPath"),
    spades.packagePath = .libPathDefault(ppBasename),
    spades.inputPath = file.path(getOption("spades.projectPath"), "inputs"),
    spades.modulePath = file.path(getOption("spades.projectPath"), "modules"),
    spades.outputPath = file.path(getOption("spades.projectPath"), "outputs"),
    spades.scratchPath = file.path(getOption("spades.projectPath"), "scratch")
  )
}
