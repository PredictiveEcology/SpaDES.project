if (!exists("pkgDir")) {
  pkgDir <- file.path("packages", version$platform, paste0(version$major, ".",
                                                           strsplit(version$minor, "[.]")[[1]][1]))

  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }
  .libPaths(pkgDir)
}

if (!suppressWarnings(require("Require"))) {
  install.packages(c("config", "Require"))
  library(Require)
}

if (FALSE) {
  Require::Require("PredictiveEcology/SpaDES.install (>= 0.0.2)")
  out <- makeSureAllPackagesInstalled(modulePath = "modules")
}

switch(Sys.info()[["user"]],
       "username1" = Sys.setenv(R_CONFIG_ACTIVE = "user1"),
       "username2" = Sys.setenv(R_CONFIG_ACTIVE = "user2"),
       "username3" = Sys.setenv(R_CONFIG_ACTIVE = "user3"),
       Sys.setenv(R_CONFIG_ACTIVE = "test")
)
#Sys.getenv("R_CONFIG_ACTIVE") ## verify

source("01-init.R")
source("02-paths.R")
source("03-packages.R")
source("04-options.R")
#source("05-prerun.R")   ## gets sourced at top of each script 06, 07x, 08x

source("06-studyArea.R") ## you will need to customize this script

source("07a-dataPrep_2001.R")
source("07b-dataPrep_2011.R")
source("07c-dataPrep_fS.R")

source("08a-ignitionFit.R")
source("08b-escapeFit.R")
source("08c-spreadFit.R")

source("09-main-sim.R")
