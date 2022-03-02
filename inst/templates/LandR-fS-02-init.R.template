if (getDTthreads() > 4) {
  data.table::setDTthreads(4)
}

switch(Sys.info()[["user"]],
       "user1" = Sys.setenv(R_CONFIG_ACTIVE = "user1"),
       "user2" = Sys.setenv(R_CONFIG_ACTIVE = "user2"),
       "user3" = Sys.setenv(R_CONFIG_ACTIVE = "user3"),
       Sys.setenv(R_CONFIG_ACTIVE = "test")
)
#Sys.getenv("R_CONFIG_ACTIVE") ## verify

cacheDir <- config::get("paths")[["cachedir"]]
cacheFormat <- config::get("cacheformat")
climateGCM <- config::get("climategcm")
climateSSP <- as.numeric(config::get("climatessp"))
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
codeChecks <- config::get("codechecks")
delayStart <- config::get("delaystart")
fitUsing <- if (grepl("for-cast[.]ca", Sys.info()[["nodename"]])) 3 else 0
libPathDEoptim <- file.path(config::get("paths")[["libpathdeoptim"]], version$platform,
                            paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
messagingNumCharsModule <- config::get("messagingNumCharsModule")
newGoogleIDs <- FALSE ## gets rechecked/updated for each script (06, 07x, 08x) based on script 05
nReps <- config::get("nreps")
reproducibleAlgorithm <- config::get("reproduciblealgorithm")
reupload <- config::get("reupload")
run <- config::get("run")
scratchDir <- config::get("paths")[["scratchdir"]]
simFileFormat <- config::get()[["simfileformat"]]
studyAreaName <- config::get("studyarea")
useCloudCache <- config::get("cloud")[["usecloud"]]
useLandR.CS <- config::get("uselandrcs")
useMemoise <- config::get("usememoise")
usePlot <- config::get("plot")
userInputPaths <- config::get("paths")[["inputpaths"]]
usePrerun <- config::get("useprerun")
useRequire <- config::get("userequire")
useTerra <- config::get("useterra")
.plotInitialTime <- if (isTRUE(usePlot)) 2011 else NA

if (!exists("runName")) {
  runName <- sprintf("%s_%s_SSP%03d_run%02d", studyAreaName, climateGCM, climateSSP, run)
} else {
  chunks <- strsplit(runName, "_")[[1]]
  climateSSP <- substr(chunks[length(chunks) - 1], 4, 6)
  climateGCM <- if (grepl("ensemble", runName)) paste0(chunks[2], "_", chunks[3]) else chunks[2]
  studyAreaName <- chunks[1]
  run <- as.numeric(substr(chunks[length(chunks)], 4, 5))
}

firstRunMDCplots <- if (run == 1 && reupload) TRUE else FALSE
firstRunIgnitionFit <- if (run == 1) TRUE else FALSE
