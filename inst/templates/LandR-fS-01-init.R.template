.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

Require::Require("config")

cacheDir <- config::get("paths")[["cachedir"]]
cacheFormat <- config::get("cacheformat")
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
codeChecks <- config::get("codechecks")
firstRunSpreadFit <- FALSE
messagingNumCharsModule <- config::get("messagingNumCharsModule")
newGoogleIDs <- FALSE ## gets rechecked/updated for each script (06, 07x, 08x) based on script 05
reproducibleAlgorithm <- config::get("reproduciblealgorithm")
scratchDir <- config::get("paths")[["scratchdir"]]
studyAreaName <- config::get("studyarea")
useCloudCache <- config::get("cloud")[["usecloud"]]
useMemoise <- config::get("usememoise")
usePlot <- config::get("plot")
userInputPaths <- config::get("inputpaths")
usePrerun <- config::get("useprerun")
useRequire <- config::get("userequire")
.plotInitialTime <- if (isTRUE(usePlot)) 2011 else NA
