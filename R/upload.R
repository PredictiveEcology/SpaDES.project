#' Save a SpaDES simulation to an RDS file
#'
#' @description
#' Saves a `simList` to an RDS file via [SpaDES.core::saveSimList()].
#' Heavy ancillary data (inputs, outputs, cache, files) are excluded so the
#' file contains only the simulation state; pair with [outTar()] to bundle the
#' output files separately.
#'
#' @param sim A `simList` object.
#' @param runName Character scalar. Used to construct the default filename via
#'   [SpaDES.core::simFile()].
#' @param simFilename Character scalar. Full path for the `.rds` file.
#'   Defaults to `SpaDES.core::simFile(name = runName, path = outputPath(sim),
#'   time = end(sim), ext = "rds")`.
#'
#' @return Invisibly returns `simFilename`.
#' @seealso [outTar()], [outUpload()], [outSaveTarUpload()]
#' @export
outSave <- function(sim, runName, simFilename = NULL) {
  if (is.null(simFilename))
    simFilename <- SpaDES.core::simFile(
      name = runName,
      path = SpaDES.core::outputPath(sim),
      time = SpaDES.core::end(sim),
      ext  = "rds"   # do not use qs
    )
  dir.create(dirname(simFilename), showWarnings = FALSE, recursive = TRUE)
  elapsed <- system.time(SpaDES.core::saveSimList(
    sim      = sim,
    filename = simFilename,
    inputs   = FALSE,
    outputs  = FALSE,
    cache    = FALSE,
    files    = FALSE
  ))
  message("Saved sim to ", simFilename,
          " (", round(elapsed[["elapsed"]], 1), " s)")
  invisible(simFilename)
}


#' Bundle a sim file and output files into a tar.gz archive
#'
#' @description
#' Creates a `.tar.gz` archive containing `simFilename` and any additional
#' `outputFiles`.  Files that do not exist are silently skipped so a partially
#' completed simulation can still be archived.
#'
#' @param simFilename Character scalar. Path to the saved sim RDS file
#'   (typically the return value of [outSave()]).
#' @param outputFiles Character vector of additional files to include (e.g.
#'   `SpaDES.core::outputs(sim)$file`).  Non-existent paths are dropped.
#'   Default `character(0)`.
#' @param runName Character scalar. Base name for the tarball
#'   (`<runName>.tar.gz`).
#' @param tarDir Character scalar. Directory in which to create the tarball.
#'   Defaults to `dirname(simFilename)`.
#' @param verbose Logical. Pass `-v` to `tar` for file-by-file progress.
#'   Default `TRUE`.
#'
#' @return Invisibly returns the path to the created tarball.
#' @seealso [outSave()], [outUpload()], [outSaveTarUpload()]
#' @export
outTar <- function(simFilename, outputFiles = character(0), runName,
                   tarDir = dirname(simFilename), verbose = TRUE) {
  outputFiles <- outputFiles[nzchar(outputFiles) & file.exists(outputFiles)]
  allFiles    <- unique(c(simFilename, outputFiles))
  tarball     <- file.path(tarDir, paste0(runName, ".tar.gz"))
  tar(tarball, files = allFiles,
      extra_flags = if (isTRUE(verbose)) "-v" else "")
  message("Created tarball: ", tarball)
  invisible(tarball)
}


#' Upload a file to Google Drive
#'
#' @description
#' Uploads a local file (typically a tarball produced by [outTar()]) to a
#' Google Drive folder via [googledrive::drive_upload()].
#'
#' @param tarball Character scalar. Path to the local file to upload.
#' @param gFolder A Google Drive folder identifier accepted by
#'   [googledrive::drive_upload()] — a `dribble`, a Drive URL, or a bare
#'   folder ID from [googledrive::as_id()].
#' @param overwrite Logical. Overwrite an existing file of the same name in
#'   the Drive folder.  Default `TRUE`.
#' @param cleanup Logical. Delete the local tarball after a successful upload.
#'   Default `FALSE`.
#'
#' @return Invisibly returns the `dribble` returned by
#'   [googledrive::drive_upload()].
#' @seealso [outSave()], [outTar()], [outSaveTarUpload()]
#' @export
outUpload <- function(tarball, gFolder, overwrite = TRUE, cleanup = FALSE) {
  if (is.null(gFolder))
    stop("gFolder must be supplied for Google Drive upload.", call. = FALSE)
  result <- googledrive::drive_upload(
    tarball,
    path      = gFolder,
    name      = basename(tarball),
    overwrite = overwrite
  )
  message("Uploaded ", basename(tarball), " to Google Drive")
  if (isTRUE(cleanup)) {
    unlink(tarball)
    message("Deleted local tarball: ", tarball)
  }
  invisible(result)
}


#' Save, tar, and upload a SpaDES simulation to Google Drive
#'
#' @description
#' Convenience wrapper that calls [outSave()], [outTar()], and [outUpload()]
#' in sequence.  The sim is saved to an RDS file, bundled with its output
#' files into a `.tar.gz` archive, and the archive is uploaded to a Google
#' Drive folder.
#'
#' @param runName Character scalar. Label used as the base name for the saved
#'   sim file and tarball.
#' @param sim A `simList` object.
#' @param gFolder A Google Drive folder identifier; passed to [outUpload()].
#'   Required.
#' @param simFilename Character scalar. Passed to [outSave()]; see that
#'   function for the default.
#' @param tarDir Character scalar. Directory for the tarball; defaults to
#'   `dirname(simFilename)`.  Passed to [outTar()].
#' @param overwrite Logical. Passed to [outUpload()].  Default `TRUE`.
#' @param cleanup Logical. Delete the local tarball after upload.  Passed to
#'   [outUpload()].  Default `FALSE`.
#' @param verbose Logical. Verbose tar output.  Passed to [outTar()].
#'   Default `TRUE`.
#'
#' @return Invisibly returns the `dribble` from [googledrive::drive_upload()].
#' @seealso [outSave()], [outTar()], [outUpload()]
#' @export
outSaveTarUpload <- function(runName, sim, gFolder = NULL, simFilename = NULL,
                              tarDir = NULL, overwrite = TRUE, cleanup = FALSE,
                              verbose = TRUE) {
  simFilename <- outSave(sim, runName, simFilename)
  if (is.null(tarDir))
    tarDir <- dirname(simFilename)
  tarball <- outTar(simFilename,
                    outputFiles = SpaDES.core::outputs(sim)$file,
                    runName     = runName,
                    tarDir      = tarDir,
                    verbose     = verbose)
  outUpload(tarball, gFolder, overwrite = overwrite, cleanup = cleanup)
}
