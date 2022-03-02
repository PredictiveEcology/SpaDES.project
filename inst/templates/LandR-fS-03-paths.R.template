################################################################################
## Set paths for each part of the simulation
################################################################################

defaultPaths <- list(
  cachePath = cacheDir,
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", studyAreaName),
  scratchPath = scratchDir
)

scratchDir <- checkPath(file.path(scratchDir, studyAreaName), create = TRUE) ## basedir set in config

preamblePaths <- defaultPaths
preamblePaths[["cachePath"]] <- file.path(cacheDir, "cache_preamble", studyAreaName)

dataPrepPaths <- defaultPaths
dataPrepPaths[["cachePath"]] <- file.path(cacheDir, "cache_dataPrep")

ignitionFitPaths <- defaultPaths
ignitionFitPaths[["cachePath"]] <- file.path(cacheDir, "cache_ignitionFit")

escapeFitPaths <- defaultPaths
escapeFitPaths[["cachePath"]] <- file.path(cacheDir, "cache_escapeFit")

spreadFitPaths <- defaultPaths
spreadFitPaths[["cachePath"]] <- file.path(cacheDir, "cache_spreadFit", runName)

## main (dynamic) simulation
dynamicPaths <- defaultPaths
dynamicPaths$cachePath <- file.path(cacheDir, "cache_sim")
dynamicPaths$outputPath <- file.path("outputs", runName)

## postprocessing paths
posthocPaths <- defaultPaths
posthocPaths[["cachePath"]] <- file.path(cacheDir, "cache_posthoc")
posthocPaths[["outputPath"]] <- dirname(defaultPaths[["outputPath"]])
