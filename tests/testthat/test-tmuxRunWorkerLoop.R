## tmuxRunWorkerLoop(), pane_mode = "reuse": keep claiming jobs in this R
## session until something says stop. Every exit condition is driven by the
## return value of tmuxRunNextWorker(), which is mocked here, so no queue is
## consumed and no job is sourced.
##
## The "killAndNewPane" branch is deliberately not covered: it ends in
## respawn-pane and quit(), which would take the test process with it.

mkGlobal <- function(dir) {
  f <- file.path(dir, "global.R")
  writeLines("invisible(NULL)", f)
  f
}

test_that("tmuxRunWorkerLoop stops immediately when the stop file is present", {
  td <- withr::local_tempdir()
  sf <- file.path(td, "stop")
  file.create(sf)
  called <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { called <<- called + 1L; "ok" })

  res <- suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td), stop_file = sf))

  expect_true(res)
  # the stop file is checked before the first claim, so no job is ever taken
  expect_identical(called, 0L)
})

test_that("tmuxRunWorkerLoop keeps going until the queue reports empty", {
  td <- withr::local_tempdir()
  results <- c("ok", "ok", "empty")
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; results[[i]] })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td)))

  expect_identical(i, 3L)
})

test_that("tmuxRunWorkerLoop stops on an error result", {
  td <- withr::local_tempdir()
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; "error" })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td)))

  expect_identical(i, 1L)
})

test_that("tmuxRunWorkerLoop stops on interrupt when on_interrupt is 'fail'", {
  td <- withr::local_tempdir()
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; "interrupt" })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td), on_interrupt = "fail"))

  expect_identical(i, 1L)
})

test_that("tmuxRunWorkerLoop keeps going on interrupt when requeueing", {
  td <- withr::local_tempdir()
  results <- c("interrupt", "interrupt", "empty")
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; results[[i]] })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td), on_interrupt = "requeue"))

  # requeue means an interrupt is not fatal; only "empty" ends the loop
  expect_identical(i, 3L)
})
