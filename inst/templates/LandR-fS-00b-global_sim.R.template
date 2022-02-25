source("01-packages.R")

source("02-init.R")
source("03-paths.R")
source("04-options.R")
source("05-google-ids.R")

if (delayStart > 0 & run == 1) {
  message(crayon::green("\nStaggered job start: delaying", runName, "by", delayStart/10, "minutes."))
  Sys.sleep(delayStart*6)
}

usePrerun = TRUE; reupload = FALSE
source("06-studyArea.R")
source("07a-dataPrep_2001.R")
source("07b-dataPrep_2011.R")
source("07c-dataPrep_fS.R")

source("08a-ignitionFit.R")
source("08b-escapeFit.R")
source("08c-spreadFit.R")

source("09-main-sim.R")

message(crayon::red("Simulation", runName, "complete"))
