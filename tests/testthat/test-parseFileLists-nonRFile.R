## An unnamed element of `options`, `params`, `sideEffects` or `functions` names a file of R code.
## A file that is not R code (e.g. a data manifest) used to be downloaded, then silently dropped.

nonRFixtures <- function(td) {
  csv <- file.path(td, "manifest.csv")
  writeLines(c("filename,url", "a,b"), csv)
  lowerR <- file.path(td, "opts.r")
  writeLines("list(opt.fromLowerR = 1)", lowerR)
  txt <- file.path(td, "opts.txt")
  writeLines("list(opt.fromTxt = 2)", txt)
  list(csv = csv, lowerR = lowerR, txt = txt, paths = list(projectPath = td))
}

test_that("an unnamed non-R file is an error that names the file and the argument", {
  f <- nonRFixtures(withr::local_tempdir())
  for (arg in c("options", "params", "sideEffects", "functions")) {
    expect_error(parseFileLists(list(f$csv, a.b = 2), paths = f$paths, envir = environment(),
                                verbose = 0, rCodeFor = arg),
                 regexp = paste0("`", arg, "`.*manifest\\.csv"))
  }
  expect_error(setupOptions(options = list(f$csv), paths = f$paths, verbose = 0),
               regexp = "manifest\\.csv")
})

test_that("the error says where a downloaded file came from", {
  f <- nonRFixtures(withr::local_tempdir())
  remote <- "PredictiveEcology/PredictiveEcology.org@main/scripts/manifest.csv"
  expect_error(.stopIfNotRCode(stats::setNames(f$csv, remote), rCodeFor = "options"),
               regexp = "downloaded from PredictiveEcology/PredictiveEcology.org@main/scripts/manifest.csv")
})

test_that(".r and .txt files are read as R code", {
  f <- nonRFixtures(withr::local_tempdir())
  out <- parseFileLists(list(f$lowerR, f$txt), paths = f$paths, envir = environment(),
                        verbose = 0, rCodeFor = "options")
  expect_identical(out$opt.fromLowerR, 1)
  expect_identical(out$opt.fromTxt, 2)
})

test_that("named elements, missing files, directories and `{` blocks are not errors", {
  f <- nonRFixtures(withr::local_tempdir())
  ## a named element's value is a value, even when it is the path to a data file
  out <- parseFileLists(list(reproducible.urlRemap = f$csv), paths = f$paths, envir = environment(),
                        verbose = 0, rCodeFor = "options")
  expect_identical(out$reproducible.urlRemap, f$csv)
  ## a missing file keeps its existing message, not an error
  expect_no_error(parseFileLists(list(file.path(dirname(f$csv), "absent.csv"), a.b = 2), paths = f$paths,
                                 envir = environment(), verbose = 0, rCodeFor = "options"))
  expect_no_error(parseFileLists(list(dirname(f$csv)), paths = f$paths, envir = environment(),
                                 verbose = 0, rCodeFor = "options"))
  expect_no_error(parseFileLists(quote({x <- 1}), paths = f$paths, namedList = FALSE,
                                 envir = environment(), verbose = 0, rCodeFor = "sideEffects"))
})

test_that("callers that are not reading R code (paths, modules, packages) keep a non-R file", {
  f <- nonRFixtures(withr::local_tempdir())
  expect_no_error(out <- parseFileLists(f$csv, paths = f$paths, namedList = FALSE, envir = environment(), verbose = 0))
  expect_identical(unname(out), f$csv)
})
