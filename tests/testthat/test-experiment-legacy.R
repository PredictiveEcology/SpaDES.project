## experiment() and simInitAndExperiment(), ported from SpaDES.experiment.
## experiment() is a wrapper: it builds a factorial set of simLists via
## factorialDesign() + .buildExperimentSim(), then runs them through
## experiment2(). Offline throughout -- module-free simLists under a sequential
## future plan.

localExperimentSetup <- function(envir = parent.frame()) {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  withr::local_options(spades.moduleCodeChecks = FALSE, .local_envir = envir)
  localSpadesOptions(envir)
  oplan <- future::plan("sequential")
  withr::defer(future::plan(oplan), envir = envir)
}

mkBase <- function(outputPath) {
  suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1),
                                        paths = list(outputPath = outputPath)))
}

test_that("experiment rejects anything that is not a simList", {
  localExperimentSetup()

  expect_error(experiment("not a simList"), "must be a `simList`")
  expect_error(experiment(list()), "must be a `simList`")
})

test_that("experiment runs a single simList and returns a simLists", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  out <- suppressMessages(experiment(mkBase(td), saveExperiment = FALSE))

  expect_s4_class(out, "simLists")
  expect_length(ls(out), 1L)
  expect_true(is(out[[ls(out)[[1]]]], "simList"))
})

test_that("experiment attaches the design table to the result", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  out <- suppressMessages(experiment(mkBase(td), saveExperiment = FALSE))
  exp <- attr(out@.xData, "experiment")

  expect_named(exp, c("expDesign", "expVals"))
  expect_s3_class(exp$expDesign, "data.frame")
  expect_true("expLevel" %in% names(exp$expDesign))
})

test_that("experiment produces one simList per replicate", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  out <- suppressMessages(experiment(mkBase(td), replicates = 2, saveExperiment = FALSE))

  expect_length(ls(out), 2L)
  expect_identical(nrow(attr(out@.xData, "experiment")$expDesign), 2L)
})

test_that("experiment writes the design to experimentFile when asked", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  suppressMessages(
    experiment(mkBase(td), saveExperiment = TRUE, experimentFile = "exp.RData")
  )

  f <- file.path(td, "exp.RData")
  expect_true(file.exists(f))
  # the saved object is the same list attached to the result
  e <- new.env(parent = emptyenv())
  load(f, envir = e)
  expect_named(e$experiment, c("expDesign", "expVals"))
})

test_that("experiment does not write a file when saveExperiment is FALSE", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  suppressMessages(experiment(mkBase(td), saveExperiment = FALSE,
                              experimentFile = "nope.RData"))

  expect_false(file.exists(file.path(td, "nope.RData")))
})

test_that("experiment reports that cl is deprecated", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  msgs <- capture_messages(experiment(mkBase(td), cl = 1, saveExperiment = FALSE))

  expect_true(any(grepl("deprecated", msgs)))
  expect_true(any(grepl("future::plan", msgs, fixed = TRUE)))
})

test_that("simInitAndExperiment builds the simList and runs it", {
  localExperimentSetup()
  td <- withr::local_tempdir()

  out <- suppressMessages(
    simInitAndExperiment(times = list(start = 0, end = 1),
                         paths = list(outputPath = td),
                         saveExperiment = FALSE)
  )

  expect_s4_class(out, "simLists")
  expect_length(ls(out), 1L)
})
