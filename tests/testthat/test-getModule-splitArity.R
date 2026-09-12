## getModule() used to drop `versionSpec` from splitGitRepo()'s output and
## assign whatever remained to three data.table columns. That silently depended
## on splitGitRepo() returning exactly four elements. When Require gained a
## fifth (`subFolder`), every setupProject test that downloads a module died
## with "Supplied 3 columns to be assigned 4 items".
##
## These pin the contract from both sides so the coupling cannot rot again.

test_that("getModule takes acct/repo/br by name, not by whatever is left over", {
  skip_if_not_installed("Require")
  g <- Require:::splitGitRepo("PredictiveEcology/Biomass_core@development")
  ## the three fields getModule needs must exist by name ...
  expect_true(all(c("acct", "repo", "br") %in% names(g)))
  ## ... and selecting them must give exactly three, whatever else Require adds
  expect_length(g[c("acct", "repo", "br")], 3L)
})

test_that("extra elements in splitGitRepo's return do not change the selection", {
  ## simulate Require growing further: the selection must stay at three
  fake <- list(acct = "a", repo = "b", br = "c", versionSpec = list(),
               subFolder = NA_character_, somethingNew = 1)
  expect_length(fake[c("acct", "repo", "br")], 3L)
  expect_identical(names(fake[c("acct", "repo", "br")]), c("acct", "repo", "br"))
  ## the old approach is length-dependent, and would now be wrong
  old <- fake; old[["versionSpec"]] <- NULL
  expect_gt(length(old), 3L)
})

test_that("getModule survives a splitGitRepo that returns extra elements", {
  skip_if_not_installed("Require")
  mp <- withr::local_tempdir()
  ## a module that cannot be downloaded: we only care that the column
  ## assignment does not error before it gets that far
  out <- suppressWarnings(suppressMessages(
    getModule("PredictiveEcology/thisModuleDoesNotExist@main",
              modulePath = mp, verbose = -1)))
  expect_true(is.list(out) || is.data.frame(out))
})
