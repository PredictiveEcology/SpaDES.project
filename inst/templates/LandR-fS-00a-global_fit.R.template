source("01-packages.R")

source("02-init.R")
source("03-paths.R")
source("04-options.R")
source("05-google-ids.R")

if (delayStart > 0) {
  message(crayon::green("\nStaggered job start: delaying", runName, "by", delayStart, "minutes."))
  Sys.sleep(delayStart*60)
}

source("06-studyArea.R")

source("07a-dataPrep_2001.R")
source("07b-dataPrep_2011.R")
source("07c-dataPrep_fS.R")

message(crayon::red("Data prep", runName, "complete"))

source("08a-ignitionFit.R")
source("08b-escapeFit.R")

for (i in 1:nReps) {
  run <- i
  runName <- gsub("run[0-9][0-9]", sprintf("run%02d", run), runName)

  ## prerun all spreadfits, for use with main sim runs on another machine

  if (file.exists("Rplots.pdf")) {
    unlink("Rplots.pdf")
  }
  source("08c-spreadFit.R")
  if (file.exists("Rplots.pdf")) {
    file.rename("Rplots.pdf", file.path(Paths$outputPath, "figures", sprintf("spreadFit_plots_%s.pdf", runName)))
  }
}
