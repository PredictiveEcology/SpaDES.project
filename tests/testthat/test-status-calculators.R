## statusCalculate_FireSenseFit / statusCalculate_LandR.
##
## These are exported quote()d block expressions, not functions: tmuxRefreshQueueStatus()
## eval()s them in an environment holding the queue row's columns plus helpers
## like pathBuild() and times/outs. The tests below build that environment
## directly, so they exercise the real expressions with no tmux and no queue.

mkEnv <- function(outputPath, ...) {
  e <- new.env(parent = globalenv())
  e$pathBuild <- function(...) outputPath
  e$.ELFind <- "job1"; e$.samplingRange <- "r1"
  e$.GCM <- "g1"; e$.SSP <- "s1"; e$.rep <- 1L
  for (nm in names(list(...))) assign(nm, list(...)[[nm]], envir = e)
  e
}

# --- statusCalculate_LandR ----------------------------------------------------

test_that("statusCalculate_LandR is a quoted expression", {
  expect_true(is.call(statusCalculate_LandR))
  expect_true(is.call(statusCalculate_FireSenseFit))
})

test_that("statusCalculate_LandR reports the latest year and marks done at the end year", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("cohortData_year2010.rds", "cohortData_year2020.rds")))
  e <- mkEnv(d, outs = list(times = list(end = 2020)))

  eval(statusCalculate_LandR, envir = e)

  expect_identical(e$heartbeat_iter, "2020")
  expect_true(e$done)
  expect_identical(e$iterationsTotal, "2020")
})

test_that("statusCalculate_LandR is not done before the end year", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("cohortData_year2010.rds", "cohortData_year2015.rds")))
  e <- mkEnv(d, outs = list(times = list(end = 2030)))

  eval(statusCalculate_LandR, envir = e)

  expect_identical(e$heartbeat_iter, "2015")
  expect_false(e$done)
  # finished_at / iterationsTotal are only set once done
  expect_false(exists("iterationsTotal", envir = e, inherits = FALSE))
})

test_that("statusCalculate_LandR assigns nothing when there are no checkpoints", {
  d <- withr::local_tempdir()
  e <- mkEnv(d, outs = list(times = list(end = 2020)))

  eval(statusCalculate_LandR, envir = e)

  # hb$iter is NA, so the whole block is skipped
  expect_false(exists("heartbeat_iter", envir = e, inherits = FALSE))
  expect_false(exists("done", envir = e, inherits = FALSE))
})

# --- statusCalculate_FireSenseFit ---------------------------------------------

test_that("statusCalculate_FireSenseFit marks done when the final-year burnMap exists", {
  d <- withr::local_tempdir()
  file.create(file.path(d, "burnMap_year2020.tif"))
  e <- mkEnv(d, times = list(end = 2020), queue_path = file.path(d, "q.rds"))

  eval(statusCalculate_FireSenseFit, envir = e)

  expect_true(e$done)
  expect_identical(e$iterationsTotal, "2020")
})

test_that("statusCalculate_FireSenseFit reports a heartbeat while still running", {
  d <- withr::local_tempdir()
  running <- withr::local_tempdir()
  file.create(file.path(d, "burnMap_year2010.tif"))          # not the end year
  rf <- file.path(running, "job1")
  file.create(rf)
  fm <- file.path(d, "Annual Fire Maps 2015.tif")
  file.create(fm)
  # the fire map must post-date the running flag to count as progress
  Sys.setFileTime(rf, Sys.time() - 60)
  Sys.setFileTime(fm, Sys.time())

  e <- mkEnv(d, times = list(end = 2020, start = 2011),
             queue_path = file.path(d, "q.rds"))
  e$tmuxActiveRunningPath <- function(...) running

  eval(statusCalculate_FireSenseFit, envir = e)

  expect_false(e$done)
  expect_identical(e$heartbeat_iter, "2015")
  expect_false(is.na(e$started_at))
})

test_that("statusCalculate_FireSenseFit falls back to the start year with no fire maps", {
  d <- withr::local_tempdir()
  running <- withr::local_tempdir()
  file.create(file.path(d, "burnMap_year2010.tif"))
  file.create(file.path(running, "job1"))

  e <- mkEnv(d, times = list(end = 2020, start = 2011),
             queue_path = file.path(d, "q.rds"))
  e$tmuxActiveRunningPath <- function(...) running

  eval(statusCalculate_FireSenseFit, envir = e)

  expect_false(e$done)
  # nothing newer than the running flag, so the heartbeat is the start year
  expect_identical(e$heartbeat_iter, "2011")
})

test_that("statusCalculate_FireSenseFit assigns nothing for an empty output dir", {
  d <- withr::local_tempdir()
  e <- mkEnv(d, times = list(end = 2020), queue_path = file.path(d, "q.rds"))

  eval(statusCalculate_FireSenseFit, envir = e)

  # NROW(dd) is 0, so the whole block is skipped
  expect_false(exists("done", envir = e, inherits = FALSE))
})
