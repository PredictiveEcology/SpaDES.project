Require::Require("reproducible")
Require::Require("googledrive")

source("05-google-ids.R")

filesToUpload <- c(
  paste0("fireSense_SpreadFit_veg_coeffs_", studyAreaName, ".txt")
)

gid_results <- gdriveSims[studyArea == studyAreaName & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  if (file.exists(f))
    retry(quote(drive_put(file.path("outputs", studyAreaName, f), unique(as_id(gid_results)))),
          retries = 5, exponentialDecayBase = 2)
})
