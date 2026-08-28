## experiment2(), ported from SpaDES.experiment. Runs offline against module-free
## simLists under a sequential future plan, so no cluster and no network.

mkSim <- function(v) {
  s <- suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1)))
  s$val <- v
  s
}

# Everything here needs the same Suggests + a sequential plan.
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

test_that(".experiment2RequireDeps passes when the Suggests are installed", {
  localExperimentSetup()
  expect_true(SpaDES.project:::.experiment2RequireDeps())
})

test_that("experiment2 requires at least one simList", {
  localExperimentSetup()

  expect_error(experiment2(replicates = 1), "needs one or more")
  # named non-simList arguments are spades() args, not simLists
  expect_error(experiment2(events = list()), "needs one or more")
})

test_that("experiment2 rejects a non-scalar replicates", {
  localExperimentSetup()

  expect_error(suppressMessages(experiment2(mkSim(1), replicates = c(1, 2))),
               "replicates argument must be length 1")
})

test_that("experiment2 warns when createUniquePaths is not outputPath", {
  localExperimentSetup()

  expect_message(
    suppressWarnings(try(
      experiment2(mkSim(1), createUniquePaths = "inputPath"),
      silent = TRUE
    )),
    "createUniquePaths only accepts outputPath"
  )
})

test_that("experiment2 returns a simLists named <simName>_rep<n>", {
  localExperimentSetup()

  out <- suppressMessages(
    experiment2(a = mkSim(1), b = mkSim(2),
                replicates = 2, createUniquePaths = character(0))
  )

  expect_s4_class(out, "simLists")
  expect_setequal(ls(out), c("a_rep1", "a_rep2", "b_rep1", "b_rep2"))
  # each element is a simList that actually ran
  expect_true(all(vapply(ls(out), function(n) is(out[[n]], "simList"), logical(1))))
})

test_that("experiment2 names unnamed simLists positionally", {
  localExperimentSetup()

  out <- suppressMessages(
    experiment2(mkSim(1), mkSim(2), createUniquePaths = character(0))
  )

  expect_setequal(ls(out), c("1_rep1", "2_rep1"))
})

test_that("experiment2 result feeds as.data.table.simLists", {
  localExperimentSetup()
  skip_if_not_installed("purrr")

  out <- suppressMessages(
    experiment2(a = mkSim(10), b = mkSim(20), createUniquePaths = character(0))
  )
  dt <- data.table::as.data.table(out, vals = "val")

  expect_s3_class(dt, "data.table")
  expect_setequal(dt$simName, c("a_rep1", "b_rep1"))
  expect_identical(dt[simName == "b_rep1"]$value, 20)
})

test_that("experiment2 creates a per-run outputPath when asked", {
  localExperimentSetup()

  out <- suppressMessages(
    experiment2(a = mkSim(1), createUniquePaths = "outputPath")
  )

  # experiment2Inner nests outputPath under the run name
  expect_match(SpaDES.core::outputPath(out[["a_rep1"]]), "a_rep1$")
})
