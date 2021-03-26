do.call(setPaths, preamblePaths)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["simOutPreamble"]] == ""

preambleObjects <- list()

preambleParams <- list(
  YOUR_MODULE_NAME = list( ## TODO: use your module name
    ".useCache" = TRUE,
    "historicalFireYears" = 1991:2019,
    "studyAreaName" = studyAreaName
  )
)

fsimOutPreamble <- file.path(Paths$outputPath, paste0("simOutPreamble_", studyAreaName, ".qs"))
if (isTRUE(usePrerun)) {
  if (!file.exists(fsimOutPreamble)) {
    googledrive::drive_download(file = as_id(gdriveSims[["simOutPreamble"]]), path = fsimOutPreamble)
  }
  simOutPreamble <- loadSimList(fsimOutPreamble)
} else {
  simOutPreamble <- Cache(simInitAndSpades,
                          times = list(start = 0, end = 1),
                          params = preambleParams,
                          modules = c("YOUR_MODULE_NAME"), ## TODO: use your module name
                          objects = preambleObjects,
                          paths = preamblePaths,
                          #useCloud = useCloudCache,
                          #cloudFolderID = cloudCacheFolderID,
                          userTags = c("YOUR_MODULE_NAME", studyAreaName) ## TODO: use your module name
  )
  saveSimList(
    sim = simOutPreamble,
    filename = fsimOutPreamble,
    #filebackedDir = dsimOutPreamble,
    fileBackend = 2 ## 0 = no change; 1 = copy rasters to fileBackedDir; 2 = rasters to memory
  )
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fsimOutPreamble, path = gdriveURL, name = basename(fsimOutPreamble), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["simOutPreamble"]]), media = fsimOutPreamble)
  }
}
