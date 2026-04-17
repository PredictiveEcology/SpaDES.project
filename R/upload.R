#' Save a SpaDES simulation and upload it with its outputs to Google Drive
#'
#' @description
#' Saves a `simList` to an RDS file via [SpaDES.core::saveSimList()], bundles
#' it together with the simulation output files into a `.tar.gz` archive, and
#' uploads the archive to a Google Drive folder.
#'
#' @param runName Character scalar. Label used as the base name for both the
#'   saved simulation file and the resulting tarball.
#' @param sim A `simList` object (the completed SpaDES simulation).
#' @param gFolder A Google Drive folder identifier accepted by
#'   [googledrive::drive_upload()] — e.g. a `dribble`, a Drive URL, or a bare
#'   folder ID created with [googledrive::as_id()].  Required; the function
#'   stops if `NULL`.
#' @param simFilename Character scalar. Full path for the saved `.rds` file.
#'   Defaults to the value returned by [SpaDES.core::simFile()] using
#'   `runName`, `SpaDES.core::outputPath(sim)`, `SpaDES.core::end(sim)`, and
#'   extension `"rds"`.
#'
#' @return Invisibly returns the [googledrive::drive_upload()] result (a
#'   `dribble` describing the uploaded file).
#'
#' @seealso [SpaDES.core::saveSimList()], [googledrive::drive_upload()]
#' @export
uploadSimAndOutputs <- function(runName, sim, gFolder = NULL, simFilename = NULL) {
  if (is.null(gFolder))
    stop("This function is currently for uploading to a GoogleDrive folder; gFolder must be supplied",
         call. = FALSE)

  if (is.null(simFilename))
    simFilename <- SpaDES.core::simFile(
      name = runName,
      path = SpaDES.core::outputPath(sim),
      time = SpaDES.core::end(sim),
      ext  = "rds"   # do not use qs
    )

  dir.create(dirname(simFilename), showWarnings = FALSE, recursive = TRUE)

  system.time(SpaDES.core::saveSimList(
    sim      = sim,
    filename = simFilename,
    inputs   = FALSE,
    outputs  = FALSE,
    cache    = FALSE,
    files    = FALSE
  ))

  tarball <- paste0(runName, ".tar.gz")
  tar(tarball,
      files       = c(simFilename, SpaDES.core::outputs(sim)$file),
      extra_flags = "-v")

  result <- googledrive::drive_upload(tarball,
                                      path      = gFolder,
                                      name      = tarball,
                                      overwrite = TRUE)
  invisible(result)
}
