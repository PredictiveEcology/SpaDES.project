## NOTE: 07a-dataPrep_2001.R needs to be run before this script

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["biomassMaps2011"]] == ""

dataPrepParams2011 <- dataPrepParams2001
dataPrepParams2011$Biomass_speciesData$types <- "KNN2011"
dataPrepParams2011$Biomass_speciesData$.studyAreaName <- paste0(studyAreaName, 2011)
dataPrepParams2011$Biomass_borealDataPrep$.studyAreaName <- paste0(studyAreaName, 2011)

dataPrepOutputs2011 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = 2011,
  file = c("cohortData2011_fireSense.rds",
           "pixelGroupMap2011_fireSense.rds",
           "speciesLayers2011_fireSense.rds",
           "standAgeMap2011_borealDataPrep.rds",
           "rawBiomassMap2011_borealDataPrep.rds") # Currently not needed
)

dataPrepObjects2011 <- dataPrepObjects
dataPrepObjects2011$standAgeMap <- simOutPreamble$standAgeMap2011

fbiomassMaps2011 <- file.path(Paths$outputPath, paste0("biomassMaps2011_", studyAreaName, ".qs"))
if (isTRUE(usePrerun)) {
  if (!file.exists(fbiomassMaps2011)) {
    googledrive::drive_download(file = as_id(gdriveSims[["biomassMaps2011"]]), path = fbiomassMaps2011)
  }
  biomassMaps2011 <- loadSimList(fbiomassMaps2011)
} else {
  biomassMaps2011 <- Cache(
    simInitAndSpades,
    times = list(start = 2011, end = 2011),
    params = dataPrepParams2011,
    modules = list("Biomass_speciesData", "Biomass_borealDataPrep"),
    objects = dataPrepObjects2011,
    paths = getPaths(),
    loadOrder = c("Biomass_speciesData", "Biomass_borealDataPrep"),
    clearSimEnv = TRUE,
    # outputs = dataPrepOutputs2011,
    .plots = "png",
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2011", studyAreaName)
  )
  saveSimList(
    sim = biomassMaps2011,
    filename = fbiomassMaps2011,
    #filebackedDir = dbiomassMaps2011,
    fileBackend = 2
  )
  #archive::archive_write_dir(archive = abiomassMaps2011, dir = dbiomassMaps2011)
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fbiomassMaps2011, path = gdriveURL, name = basename(fbiomassMaps2011), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["biomassMaps2011"]]), media = fbiomassMaps2011)
  }
}

rm(dataPrepOutputs2001, dataPrepParams2001, dataPrepOutputs2011, dataPrepParams2011)
