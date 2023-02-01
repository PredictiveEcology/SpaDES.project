maxMemory <- 5e+9 # if (grepl("LandWeb", runName)) 5e+12 else 5e+9

if (.mode == "development") {
  list(test = 2)
}

if (.mode == "batch") {
  list(test = 1)
}

if (nodes == "batch") {
  list(test = 3)
}

# rasterOptions(default = TRUE)
opts <- list(
  "fftempdir" = paths$scratchPath,
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = normPath(paths$inputPath), # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = paths$tilePath,
  "map.maxNumCores" = 2,
  # "map.useParallel" = mapParallel,
  "rasterMaxMemory" = maxMemory,
  # "rasterTmpDir" = scratchDir,
  "reproducible.destinationPath" = normPath(paths$inputPath),
  "reproducible.futurePlan" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
  # "reproducible.useMemoise" = ifelse(isTRUE(batchMode), FALSE, if (user("emcintir")) FALSE else TRUE),
  "reproducible.useGDAL" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "spades.inputPath" = normPath(paths$outputPath),
  "spades.moduleCodeChecks" = FALSE,
  "spades.recoveryMode" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

opts <- list(
  "map.overwrite" = FALSE
)

if (user("emcintir"))
  list(
    "reproducible.showSimilar" = FALSE
  )

if (user("emcintir")) {
  list(
  "reproducible.inputPaths" = Require::checkPath("~/data", create = TRUE),
  "reproducible.devMode" = TRUE
  )
}

googledrive::drive_auth(email = "eliotmcintire@gmail.com", cache = "~/.secret")

if (machine("A127")) {
  list(Ncpus = 2)
}

# library(googledrive)

httr::set_config(httr::config(http_version = 0))

# token <- if (dir.exists(computeCanadaScratch)) {
#   file.path(activeDir, "landweb-82e0f9f29fbc.json")
# } else if (Sys.info()['nodename'] == "landweb") {
#   file.path(activeDir, "landweb-e3147f3110bf.json")
# } else {
#   NA_character_
# } %>%
#   normPath(.)

if (FALSE) {
  if (is.na(token) || !file.exists(token))
    message(crayon::red("no Google service token found"))

  if (pemisc::user("achubaty")) {
    if (utils::packageVersion("googledrive") < "1.0.0") {
      #drive_auth(service_token = token)
      drive_auth(email = "alex.chubaty@gmail.com")
    } else {
      #drive_auth(path = token)
      drive_auth(email = "alex.chubaty@gmail.com")
    }
  } else if (pemisc::user("emcintir")) {
    drive_auth(email = "eliotmcintire@gmail.com")
  } else {
    drive_auth(use_oob = quickPlot::isRstudioServer())
  }

  message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
}
