#source("01-packages.R")

library(Require)
library(reproducible)
library(googledrive)

source("05-google-ids.R")

files2upload <- c(
  list.files("outputs", "(CanESM5|CNRM-ESM2-1).*[.]tar.gz$", full.names = TRUE)
)

m <- which(basename(files2upload) == "STUDYAREANAME_CanESM5_SSP370_run01.tar.gz") ## TODO
lapply(files2upload[-c(1:m)], function(tarball) {
  studyAreaName <- substr(basename(tarball), 1, X) ## TODO
  gid_results <- gdriveSims[studyArea == studyAreaName & simObject == "results", gid]
  retry(quote(drive_put(media = tarball, path = unique(as_id(gid_results)), name = basename(tarball))),
        retries = 5, exponentialDecayBase = 2)
})
