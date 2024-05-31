defaultBr <- "development"
defaultAcct <- "PredictiveEcology"

modules <- c(
  "Biomass_borealDataPrep",
  "Biomass_core",
  "Biomass_regeneration",
  "Biomass_speciesData",
  #HSI_Caribou_MB = "HSI_Caribou_MB", ## used for postprocess in MB, not devel nor production
  "LandMine",
  "LandWeb_output",
  "LandWeb_preamble",
  #LandWeb_summary = "LandWeb_summary", ## used for postprocess, not devel nor production
  "timeSinceFire"
)


hasAcct <- mapply(m = modules, function(m) tryCatch(splitGitRepo(m, default = "")$acct,
                                                    error = function(e) ""))
hasAcct <- nzchar(hasAcct)

if (any(!hasAcct))
  modules[!hasAcct] <- file.path(defaultAcct, modules[!hasAcct])


hasBranch <- grepl("\\@", modules)
if (any(!hasBranch))
  modules[!hasBranch] <- paste0(modules[!hasBranch], "@", defaultBr)

modules
