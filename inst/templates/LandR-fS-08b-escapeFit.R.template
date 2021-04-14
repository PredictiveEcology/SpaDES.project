do.call(setPaths, escapeFitPaths)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["escapeOut"]] == ""

escapeFitParams <- list(
  fireSense_EscapeFit = list(
    fireSense_escapeFormula = fSsimDataPrep$fireSense_escapeFormula
  )
)

escapeFitObjects <- list(
  fireSense_escapeCovariates = fSsimDataPrep$fireSense_escapeCovariates
)

fescapeOut <- file.path(Paths$outputPath, paste0("escapeOut_", studyAreaName, ".qs"))
if (isTRUE(usePrerun)) {
  if (!file.exists(fescapeOut)) {
    googledrive::drive_download(file = as_id(gdriveSims[["escapeOut"]]), path = fescapeOut)
  }
  escapeOut <- loadSimList(fescapeOut)
} else {
  escapeOut <- simInitAndSpades(
    times = list(start = 0, end = 1),
    # ignitionSim <- simInit(times = list(start = 0, end = 1),
    params = escapeFitParams,
    modules = "fireSense_EscapeFit",
    paths = escapeFitPaths,
    objects = escapeFitObjects
  )
  saveSimList(
    sim = escapeOut,
    filename = fescapeOut,
    #filebackedDir = descapeOut,
    fileBackend = 2
  )
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fescapeOut, path = gdriveURL, name = basename(fescapeOut), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["escapeOut"]]), media = fescapeOut)
  }
}
