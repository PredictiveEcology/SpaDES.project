a <- 1
mode <- "development"

paramsDefault <- list(
  .globals = list(
    fireTimestep = a,
    initialB = NA,
    reps = 1L:10L,
    sppEquivCol = "LandR",
    successionTimestep = 10,
    .plotInitialTime = times$start,
    .plots = c("object", "png", "raw", "screen"),
    .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
    # .studyAreaName = self$context$studyAreaName,
    .useParallel = 2 ## doesn't benefit from more DT threads
  ),
  Biomass_borealDataPrep = list(
    biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                      (logAge + cover | ecoregionGroup))),
    ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
    exportModels = "all",
    fixModelBiomass = TRUE,
    forestedLCCClasses = 1:6, ## LCC2010 default
    LCCClassesToReplaceNN = numeric(0), ## LCC2010 default
    pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
    pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
    speciesTableAreas = c("BSW", "BP", "MC", "PM"), ## western boreal defaults
    speciesUpdateFunction = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    subsetDataAgeModel = 100,
    subsetDataBiomassModel = 100,
    useCloudCacheForStats = FALSE, ## TODO: re-enable once errors in species levels resolved
    .plotInitialTime = times$start, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_core = list(
    # growthAndMortalityDrivers = ifelse(isTRUE(self$args[["useLandR.CS"]]), "LandR.CS", "LandR"),
    growthInitialTime = times$start, ## start(sim)
    # vegLeadingProportion = 0, ## apparently `sppColorVect` has no mixed colour
    .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
    .plotInitialTime = times$start, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  CBM_defaults = list(
    fireInitialTime = times$start + 1, ## start(sim, "year") + 1
    .plotInitialTime = times$start, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_speciesData = list(
    dataYear = times$start,
    types = "KNN",
    .plotInitialTime = times$start, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_speciesFactorial = list(
    factorialSize = "small" ## TODO: use medium?
  ),
  Biomass_speciesParameters = list(
    constrainGrowthCurve = c(0, 1),
    constrainMaxANPP = c(3.0, 3.5),
    constrainMortalityShape = c(10, 25),
    GAMMiterations = 2,
    GAMMknots = 3,
    minimumPlotsPerGamm = 65,
    quantileAgeSubset = 98,
    speciesFittingApproach = "focal"
  ),
  Biomass_summary = list(
    ## TODO
  ),
  canClimateData = list(
    # climateGCM = self$context$climateGCM,
    # climateSSP = self$context$climateSSP,
    historicalFireYears = 1991:2020,
    studyAreaName = NA_character_,
    # runName = self$context$runName,
    .useCache = ".inputObjects"
  ),
  fireSense = list(
    plotIgnitions = FALSE,
    whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
    .plotInterval = NA
  ),
  fireSense_dataPrepFit = list(
    fireYears = 2001:2020,
    # igAggFactor = 10000 / self$context$pixelSize,
    useCentroids = TRUE,
    useFireRaster = TRUE,
    usePCA = FALSE,
    whichModulesToPrepare = c("fireSense_IgnitionFit", "fireSense_EscapeFit", "fireSense_SpreadFit"),
    # .studyAreaName = self$context$studyAreaName,
    .useCache = ".inputObjects"
  ),
  fireSense_dataPrepPredict = list(
    nonForestCanBeYoungAge = TRUE,
    whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict")
  ),
  fireSense_EscapeFit = list(),
  fireSense_EscapePredict = list(),
  fireSense_IgnitionFit = list(
    family = quote(MASS::negative.binomial(theta = 1, link = "identity")),
    iterDEoptim = 300
  ),
  fireSense_IgnitionPredict = list(
    ##
  ),
  fireSense_SpreadFit = list(
    # cloudFolderID_DE = self$args$cloud$cacheDir,
    DEoptimTests = c("adTest", "snll_fs"),
    doObjFunAssertions = FALSE,
    iterDEoptim = 150L,
    iterStep = 150L,
    iterThresh = 396L,
    libPathDEoptim = paths$packagePath, #file.path(tools::R_user_dir(basename(paths$projectPath), "data"), "packages",
                               #version$platform, getRversion()[, 1:2]),
    mode = c("fit", "visualize"), ## combo of "debug", "fit", "visualize"
    mutuallyExclusive = list("youngAge" = c("class", "nf_")),
    objFunCoresInternal = 1L,
    objfunFireReps = 100,
    rescaleAll = TRUE,
    trace = 1,
    SNLL_FS_thresh = NULL, # NULL means 'autocalibrate' to find suitable threshold value
    useCache_DE = FALSE,
    # useCloud_DE = self$args$cloud$useCloud,
    verbose = TRUE,
    visualizeDEoptim = FALSE,
    .plot = FALSE, # TRUE,
    .plotSize = list(height = 1600, width = 2000)
  ),
  fireSense_SpreadPredict = list(
    ## TODO
  ),
  fireSense_summary = list(
    ## TODO
  ),
  gmcsDataPrep = list(
    doPlotting = TRUE,
    yearOfFirstClimateImpact = times$start ## sim(start)
  )
)

params <- list(
  CBM_defaults = list(
    .plotInitialTime = 0
  )
)

list(
  CBM_defaults = list(
    .saveInitialTime = 1
  )
)

if (machine("W-VIC"))
  list(CBM_defaults = list(.useCache = mode))


list(
  .globals = list(
    fireTimestep = 1L,
    initialB = if (.version == 2) NA_real_ else 10,
    sppEquivCol = "LandWeb",
    successionTimestep = 10,
    summaryInterval = 100,
    summaryPeriod = c(700, 1000),
    vegLeadingProportion = 0.8,
    .plotInitialTime = 0,
    .plots = c("object", "png", "raw", "screen"),
    .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
    .studyAreaName = "random",
    .useParallel = 2 ## doesn't benefit from more DT threads
  ),
  Biomass_borealDataPrep = list(
    biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                      (logAge + cover | ecoregionGroup))),
    ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
    forestedLCCClasses = c(1:15, 20, 32, 34:36), ## should match preamble's treeClassesLCC
    LCCClassesToReplaceNN = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
    pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
    subsetDataAgeModel = 100,
    subsetDataBiomassModel = 100,
    speciesTableAreas = c("BSW", "BP", "MC"), ## TODO: should we remove BP? MC?
    speciesUpdateFunction = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    useCloudCacheForStats = FALSE, ## TODO: re-enable once errors in species levels resolved
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_core = list(
    growthInitialTime = 0, ## start(sim)
    initialBiomassSource = "cohortData",
    seedingAlgorithm = "wardDispersal",
    .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_regeneration = list(
    fireInitialTime = 1, ## start(sim, "year") + 1
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  Biomass_speciesData = list(
    types = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    .useCache = c(".inputObjects", "init")
  ),
  HSI_Caribou_MB = list(
    ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
    ageClassCutOffs = c(0, 40, 80, 120),                  ## LandWebUtils:::.ageClassCutOffs
    ageClassMaxAge = 400L, ## was `maxAge` previously
    reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
    simOutputPath = paths[["outputPath"]],
    summaryInterval = 100,        ## also in .globals
    summaryPeriod = c(700, 1000), ## also in .globals
    upload = FALSE,
    uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
    version = .version,
    .makeTiles = FALSE, ## no tiles until parallel tile creation resolved (ropensci/tiler#18)
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "postprocess"), ## don't cache 'init'
    .useParallel = options[["map.maxNumCores"]]
  ),
  LandMine = list(
    biggestPossibleFireSizeHa = 5e5,
    burnInitialTime = 1L, ## start(sim, "year") + 1; same as fireInitialTime
    maxReburns = 20L,
    maxRetriesPerID = 4L,
    minPropBurn = 0.90,
    ROSother = 30L,
    useSeed = NULL, ## NULL to avoid setting a seed
    .plotInitialTime = 1, ## sim(start) + 1
    .plotInterval = 1,
    .unitTest = TRUE,
    .useCache = c(".inputObjects", "init")
  ),
  LandWeb_output = list(
    summaryInterval = 100, ## also set in .globals
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "init")
  ),
  LandWeb_preamble = list(
    bufferDist = 20000,        ## 20 km buffer
    bufferDistLarge = 50000,   ## 50 km buffer
    dispersalType = "default",
    friMultiple = 1L,
    pixelSize = 250,
    minFRI = 25L,
    ROStype = "default",
    treeClassesLCC = c(1:15, 20, 32, 34:36), ## should match B_bDP's forestedLCCClasses
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects") ## faster without caching for "init"
  ),
  LandWeb_summary = list(
    ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
    ageClassCutOffs = c(0, 40, 80, 120),                  ## LandWebUtils:::.ageClassCutOffs
    ageClassMaxAge = 400L, ## was `maxAge` previously
    reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
    simOutputPath = paths[["outputPath"]],
    summaryInterval = 100,        ## also in .globals
    summaryPeriod = c(700, 1000), ## also in .globals
    timeSeriesTimes = 601:650,
    upload = FALSE,
    uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
    version = .version,
    .makeTiles = FALSE, ## no tiles until parallel tile creation resolved (ropensci/tiler#18)
    .plotInitialTime = 0, ## sim(start)
    .useCache = c(".inputObjects", "animation", "postprocess"), ## don't cache 'init'
    .useParallel = options[["map.maxNumCores"]]
  )

)
list(
  timeSinceFire = list(
    startTime = paramsDefault$.globals$fireTimestep,
    .useCache = c(".inputObjects") ## faster without caching for "init"
  )
)
