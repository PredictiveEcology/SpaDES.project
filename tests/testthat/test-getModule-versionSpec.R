## A version spec on a module already on disk, e.g. "Acct/modA@main (>= 0.5)",
## was never checked: the local check sat behind `all(!overwrite %in% FALSE)`,
## FALSE for the default `overwrite = FALSE`, so such modules were "failed"
## however new they were.

mkVersionedModule <- function(modulePath, name, version = "1.0.0") {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf('  version = list(%s = "%s")', name, version),
      "))"),
    file.path(modulePath, name, paste0(name, ".R"))
  )
}

test_that("getModule accepts a local module that satisfies its version spec", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA")
  mkVersionedModule(mp, "modB")
  out <- getModule(c("Acct/modA@main (>= 0.5)", "modB"), modulePath = mp, verbose = -1)
  expect_setequal(out$success, c("Acct/modA@main (>= 0.5)", "modB"))
  expect_length(out$failed, 0L)
})

test_that("getModule checks each version spec against the modulePath holding the module", {
  mps <- c(withr::local_tempdir(), withr::local_tempdir())
  mkVersionedModule(mps[1], "modA")
  mkVersionedModule(mps[2], "modB", version = "2.1.0")
  out <- getModule(c("Acct/modB@main (>= 2.0)", "Acct/modA@main (>= 0.5)"),
                   modulePath = mps, verbose = -1)
  expect_setequal(out$success, c("Acct/modB@main (>= 2.0)", "Acct/modA@main (>= 0.5)"))
})

test_that("getModule fails a local module that is too old and cannot be redownloaded", {
  skip_on_cran()
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA")
  mkVersionedModule(mp, "modB")
  out <- suppressWarnings(getModule(
    c("Acct/modA@main (>= 0.5)", "PredictiveEcology/modB@main (>= 2.0)"),
    modulePath = mp, verbose = -1))
  expect_identical(out$success, "Acct/modA@main (>= 0.5)")
  expect_identical(out$failed, "PredictiveEcology/modB@main (>= 2.0)")
})

test_that("getModule with overwrite for one module still checks the others", {
  skip_on_cran()
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA")
  mkVersionedModule(mp, "modB")
  expect_no_error(out <- suppressWarnings(getModule(
    c("Acct/modA@main (>= 0.5)", "modB"), modulePath = mp,
    overwrite = "modA", verbose = -1)))
  expect_true("modB" %in% out$success)
})
