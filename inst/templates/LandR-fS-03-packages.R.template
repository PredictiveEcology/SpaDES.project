Require(c("data.table", "plyr", "pryr")) ## ensure plyr loaded before dplyr or there will be problems
Require("PredictiveEcology/SpaDES.install (>= 0.0.4)")
Require("PredictiveEcology/SpaDES.core@development (>= 1.0.6.9023)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core

Require("achubaty/amc (>= 0.2.0)", require = FALSE, which = c("Suggests", "Imports", "Depends"))
Require(c("jimhester/archive", "slackr"), upgrade = FALSE)

out <- makeSureAllPackagesInstalled(modulePath = "modules")
