defaultBr <- "development"
defaultAcct <- "PredictiveEcology"

modules <- c(
  ## NOTE: user needs to provide their own preamble module per project, and add it to the config
  "Biomass_borealDataPrep",
  "Biomass_core",
  "Biomass_regeneration",
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_speciesParameters",
  # "Biomass_summary", ## post-processing
  "canClimateData",
  "fireSense",
  "fireSense_dataPrepFit@HEAD",
  "fireSense_dataPrepPredict",
  "fireSense_EscapeFit@HEAD",
  "fireSense_EscapePredict@HEAD",
  "fireSense_IgnitionFit",
  "fireSense_IgnitionPredict",
  "fireSense_SpreadFit",
  "fireSense_SpreadPredict",
  # "fireSense_summary", ## post-processing
  "ianmseddy/gmcsDataPrep"
)

if (.mode == "postprocess") {
  modules <- c("Biomass_summary@HEAD", "fireSense_summary@HEAD")
}

hasAcct <- mapply(m = modules, function(m) tryCatch(Require:::splitGitRepo(m, default = "")$acct,
                                                    error = function(e) ""))
hasAcct <- nzchar(hasAcct)

if (any(!hasAcct))
  modules[!hasAcct] <- file.path(defaultAcct, modules[!hasAcct])


hasBranch <- grepl("\\@", modules)
if (any(!hasBranch))
  modules[!hasBranch] <- paste0(modules[!hasBranch], "@", defaultBr)

modules
