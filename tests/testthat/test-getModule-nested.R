## A module can live in a subfolder of its repo, spelled
## `Acct/Repo@branch/subFolder`. downloadGHRepoOuter() used to pass
## `subFolder = NA` and name the destination after the *repo*, so a nested
## module landed at modulePath/<repo>/<module>/ -- where neither
## getModule()'s localExists check nor SpaDES.core looks. See #141.
##
## The download itself needs network (and, for the original report, a private
## repo), so these assert the arguments handed to Require::downloadRepo()
## rather than the result.

.captureDownloadRepoArgs <- function(spec, modulePath) {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    downloadRepo = function(gitRepo, subFolder, destDir, ...) {
      seen$gitRepo   <- gitRepo
      seen$names     <- names(gitRepo)
      seen$subFolder <- subFolder
      character(0)          # nothing extracted; caller reports "could not be downloaded"
    }
  )
  suppressWarnings(suppressMessages(
    SpaDES.project:::downloadGHRepoOuter(modToDL = spec, verbose = -1,
                                         overwrite = TRUE, modulePath = modulePath)
  ))
  seen
}

test_that("a nested module spec passes its subFolder through to downloadRepo", {
  skip_if_not_installed("Require")
  mp <- withr::local_tempdir()
  seen <- .captureDownloadRepoArgs("PredictiveEcology/testNestedModule@main/nestedModule", mp)

  ## the subfolder, not NA -- this is what reaches into the repo
  expect_identical(seen$subFolder, "nestedModule")
  ## downloadRepo() names the extracted folder after names(gitRepo); without a
  ## name it falls back to the repo, which is the bug
  expect_identical(seen$names, "nestedModule")
})

test_that("a non-nested module still passes subFolder = NA", {
  skip_if_not_installed("Require")
  mp <- withr::local_tempdir()
  seen <- .captureDownloadRepoArgs("PredictiveEcology/Biomass_core@development", mp)

  expect_true(is.na(seen$subFolder))
  expect_identical(seen$names, "Biomass_core")
})

test_that("moduleSubFolder returns the FULL path within the repo", {
  ## The regression that broke test-setupProject.R: scfm keeps its modules in
  ## `modules/<mod>`, so the subFolder is two segments. Using only the last one
  ## found nothing and the download silently produced an empty modulePath.
  expect_identical(SpaDES.project:::moduleSubFolder(
    "PredictiveEcology/scfm@development/modules/scfmRegime"), "modules/scfmRegime")
  ## single segment, both spellings
  expect_identical(SpaDES.project:::moduleSubFolder(
    "PredictiveEcology/testNestedModule@main/nestedModule"), "nestedModule")
  expect_identical(SpaDES.project:::moduleSubFolder(
    "PredictiveEcology/testNestedModule/nestedModule@main"), "nestedModule")
  ## not nested -> NA, so downloadRepo() behaves exactly as before
  expect_true(is.na(SpaDES.project:::moduleSubFolder(
    "PredictiveEcology/Biomass_core@development")))
  expect_true(is.na(SpaDES.project:::moduleSubFolder("PredictiveEcology/Biomass_core")))
})

test_that("a multi-segment nested spec passes the whole path to downloadRepo", {
  skip_if_not_installed("Require")
  mp <- withr::local_tempdir()
  seen <- .captureDownloadRepoArgs("PredictiveEcology/scfm@development/modules/scfmRegime", mp)
  expect_identical(seen$subFolder, "modules/scfmRegime")
  expect_identical(seen$names, "scfmRegime")
})

test_that("extractModName distinguishes a nested module from its repo", {
  expect_identical(SpaDES.project:::extractModName(
    "PredictiveEcology/testNestedModule@main/nestedModule"), "nestedModule")
  expect_identical(SpaDES.project:::extractModName(
    "PredictiveEcology/Biomass_core@development"), "Biomass_core")
})
