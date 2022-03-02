Require::Require("reproducible")
Require::Require("googledrive")

source("05-google-ids.R")
dir.create(tempdir()) ## TODO: figure out why this gets deleted; required for upload

try(file.move(
  file.path("outputs", studyAreaName, "figures", "spreadFit_coeffs.png"),
  file.path("outputs", studyAreaName, "figures", sprintf("spreadFit_coeffs_%s_run_%02d.png", studyAreaName, run))
))

filesToUpload <- c(
  file.path("outputs", studyAreaName, "figures", sprintf("spreadFit_coeffs_%s_run_%02d.png", studyAreaName, run))
)

gid_results <- gdriveSims[studyArea == studyAreaName & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  retry(quote(drive_put(f, unique(as_id(gid_results), basename(f)))), retries = 5, exponentialDecayBase = 2)
})
