Require("data.table")

if (FALSE) {
  Require::Require("PredictiveEcology/SpaDES.install (>= 0.0.4)")
  out <- makeSureAllPackagesInstalled(modulePath = "modules")
}

## ensure plyr loaded before dplyr or there will be problems
Require(c("plyr", "PredictiveEcology/SpaDES.core@development (>=1.0.6.9004)"),
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core

Require("jimhester/archive")
Require("slackr")
