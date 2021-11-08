## NOTE: 07a-dataPrep_2001.R needs to be run before this script

do.call(setPaths, dataPrepPaths)

gid_biomassMaps2011 <- gdriveSims[studyArea == studyAreaName & simObject == "biomassMaps2011", gid]
upload_biomassMaps2011 <- reupload | length(gid_biomassMaps2011) == 0

year <- 2011

dataPrepParams2011 <- dataPrepParams2001
dataPrepParams2011$Biomass_speciesData$types <- "KNN"
dataPrepParams2011$Biomass_speciesData$dataYear <- year
dataPrepParams2011$Biomass_speciesData$.studyAreaName <- paste0(studyAreaName, year)
dataPrepParams2011$Biomass_borealDataPrep$dataYear <- year
dataPrepParams2011$Biomass_borealDataPrep$.studyAreaName <- paste0(studyAreaName, year)

dataPrepOutputs2011 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(studyAreaName, "_",
                c("cohortData2011_fireSense.rds",
                  "pixelGroupMap2011_fireSense.rds",
                  "speciesLayers2011_fireSense.rds",
                  "standAgeMap2011_borealDataPrep.rds",
                  "rawBiomassMap2011_borealDataPrep.rds"))
)

fbiomassMaps2011 <- file.path(Paths$outputPath, paste0("biomassMaps2011_", studyAreaName, ".qs"))
if (isTRUE(usePrerun) & isFALSE(upload_biomassMaps2011)) {
  if (!file.exists(fbiomassMaps2011)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2011), path = fbiomassMaps2011)
  }
  biomassMaps2011 <- loadSimList(fbiomassMaps2011)
} else {
  biomassMaps2011 <- Cache(
    simInitAndSpades,
    times = list(start = 2011, end = 2011),
    params = dataPrepParams2011,
    modules = dataPrepModules,
    objects = dataPrepObjects,
    paths = getPaths(),
    loadOrder = unlist(dataPrepModules),
    clearSimEnv = TRUE,
    # outputs = dataPrepOutputs2011,
    .plots = "png",
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2011", studyAreaName)
  )
  saveSimList(biomassMaps2011, fbiomassMaps2011, fileBackend = 2)

  if (isTRUE(upload_biomassMaps2011)) {
    fdf <- googledrive::drive_put(media = fbiomassMaps2011, path = gdriveURL, name = basename(fbiomassMaps2011))
    gid_biomassMaps2011 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "biomassMaps2011", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2011),
      gdriveSims
    )
  }
}

rm(dataPrepOutputs2001, dataPrepParams2001, dataPrepOutputs2011, dataPrepParams2011)
