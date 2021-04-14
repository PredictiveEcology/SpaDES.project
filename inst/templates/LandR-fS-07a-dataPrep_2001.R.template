#this script will run Biomass_borealDataPrep + Biomass_speciesData twice, to generate some objects for fitting
do.call(setPaths, dataPrepPaths)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["biomassMaps2001"]] == ""

dataPrep <- list(
  subsetDataBiomassModel = 50,
  pixelGroupAgeClass = 20,
  successionTimeStep = 10,
  useCache = TRUE
)

dataPrepParams2001 <- list(
  Biomass_borealDataPrep = list(
    # "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup))),
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover | ecoregionGroup))),
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "exportModels" = "all",
    "forestedLCCClasses" = c(1:15, 20, 32, 34:36),
    "LCCClassesToReplaceNN" = 34:35,
    "pixelGroupAgeClass" = dataPrep$pixelGroupAgeClass,
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "sppEquivCol" = simOutPreamble$sppEquivCol,
    "subsetDataBiomassModel" = dataPrep$subsetDataBiomassModel,
    "useCloudCacheForStats" = useCloudCache,
    ".studyAreaName" = paste0(studyAreaName, 2001),
    ".useCache" = c(".inputObjects", "init")
  ),
  Biomass_speciesData = list(
    "sppEquivCol" = simOutPreamble$sppEquivCol,
    ".studyAreaName" = paste0(studyAreaName, 2001)
  )
)

dataPrepOutputs2001 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = 2001,
  file = paste0(studyAreaName, "_",
                c("cohortData2001_fireSense.rds",
                  "pixelGroupMap2001_fireSense.rds",
                  "speciesLayers2001_fireSense.rds",
                  "standAgeMap2001_borealDataPrep.rds",
                  "rawBiomassMap2001_borealDataPrep.rds"))
)

dataPrepObjects <- list("rasterToMatch" = simOutPreamble$rasterToMatch,
                        "rasterToMatchLarge" = simOutPreamble$rasterToMatchLarge,
                        "sppColorVect" = simOutPreamble$sppColorVect,
                        "sppEquiv" = simOutPreamble$sppEquiv,
                        "studyArea" = simOutPreamble$studyArea,
                        "studyAreaLarge" = simOutPreamble$studyAreaLarge,
                        "studyAreaReporting" = simOutPreamble$studyAreaReporting)

fbiomassMaps2001 <- file.path(Paths$outputPath, paste0("biomassMaps2001_", studyAreaName, ".qs"))
if (isTRUE(usePrerun)) {
  if (!file.exists(fbiomassMaps2001)) {
    googledrive::drive_download(file = as_id(gdriveSims[["biomassMaps2001"]]), path = fbiomassMaps2001)
  }
  biomassMaps2001 <- loadSimList(fbiomassMaps2001)
} else {
  biomassMaps2001 <- Cache(
    simInitAndSpades,
    times = list(start = 2001, end = 2001),
    params = dataPrepParams2001,
    modules = list("Biomass_speciesData", "Biomass_borealDataPrep"), ## TODO: separate to use different caches
    objects = dataPrepObjects,
    paths = getPaths(),
    loadOrder = c("Biomass_speciesData", "Biomass_borealDataPrep"),
    # outputs = dataPrepOutputs2001,
    .plots = NA,
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2001", studyAreaName)
  )
  saveSimList(
    sim = biomassMaps2001,
    filename = fbiomassMaps2001,
    #filebackedDir = dbiomassMaps2001,
    fileBackend = 2
  )
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fbiomassMaps2001, path = gdriveURL, name = basename(fbiomassMaps2001), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["biomassMaps2001"]]), media = fbiomassMaps2001)
  }
}
