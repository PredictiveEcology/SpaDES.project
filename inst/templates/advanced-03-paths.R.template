## set paths for each part of the simulation

## studyArea
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "studyArea"),
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = file.path(outputDir, runName)
)

## dataPrep
paths2 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrep"),
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = file.path(outputDir, runName)
)

## main simulation
paths3 <- list(
  ## NOTE: use separate cachePath for each dynamic simulation
  cachePath = file.path(cacheDir, runName),
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = file.path(outputDir, runName)
)
