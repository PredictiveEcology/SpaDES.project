do.call(setPaths, escapeFitPaths)

gid_escapeOut <- gdriveSims[studyArea == studyAreaName & simObject == "escapeOut", gid]
upload_escapeOut <- reupload | length(gid_escapeOut) == 0

escapeFitParams <- list(
  fireSense_EscapeFit = list(
    fireSense_escapeFormula = fSsimDataPrep$fireSense_escapeFormula
  )
)

escapeFitObjects <- list(
  fireSense_escapeCovariates = fSsimDataPrep$fireSense_escapeCovariates
)

fescapeOut <- file.path(Paths$outputPath, paste0("escapeOut_", studyAreaName, ".qs"))
if (isTRUE(usePrerun) & isFALSE(upload_preamble)) {
  if (!file.exists(fescapeOut)) {
    googledrive::drive_download(file = as_id(gid_escapeOut), path = fescapeOut)
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
  saveSimList(sim = escapeOut, filename = fescapeOut, fileBackend = 2)

  if (isTRUE(upload_escapeOut)) {
    fdf <- googledrive::drive_put(media = fescapeOut, path = gdriveURL, name = basename(fescapeOut))
    gid_escapeOut <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "escapeOut",  runID = NA,
                 gcm = NA, ssp = NA, gid = gid_escapeOut),
      gdriveSims
    )
  }
}

