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

test_that("tmuxRunWorkerLoop stops on an error result by default", {
  td <- withr::local_tempdir()
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; "error" })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td)))

  expect_identical(i, 1L)
})

test_that("tmuxRunWorkerLoop carries on after an error with on_error = 'continue', and ends when the queue is empty", {
  td <- withr::local_tempdir()
  results <- c("error", "ok", "error", "empty")
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; results[[i]] })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td), on_error = "continue"))

  expect_identical(i, 4L)
})

mkFailOkQueue <- function(td, status = "PENDING") {
  qp <- file.path(td, "queue.rds")
  saveRDS(data.frame(.ELFind = c("bad", "good"), .rep = 1:2, status = status,
                     stringsAsFactors = FALSE), qp)
  writeLines("if (.ELFind == 'bad') stop('boom')", file.path(td, "global.R"))
  qp
}

test_that("one worker on a queue [fails, ok] ends with ERROR and DONE with on_error = 'continue'", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkFailOkQueue(td)

  suppressMessages(tmuxRunWorkerLoop(queue_path = qp, global_path = file.path(td, "global.R"),
                                     on_error = "continue"))

  expect_identical(readRDS(qp)$status, c("ERROR", "DONE"))
  expect_match(readRDS(qp)$last_error[1], "boom")
})

test_that("with the default on_error = 'stop' the worker stops at the error and leaves the next row PENDING", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkFailOkQueue(td)

  suppressMessages(tmuxRunWorkerLoop(queue_path = qp, global_path = file.path(td, "global.R")))

  expect_identical(readRDS(qp)$status, c("ERROR", "PENDING"))
})

test_that("a continuing worker whose only claimable row errors ends idle, and does not spin on ERROR rows", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkFailOkQueue(td, status = c("PENDING", "HOLD"))

  res <- suppressMessages(tmuxRunWorkerLoop(queue_path = qp, global_path = file.path(td, "global.R"),
                                            on_error = "continue"))

  expect_true(res)
  expect_identical(readRDS(qp)$status, c("ERROR", "HOLD"))
})

test_that("on_error reaches the worker's own R session", {
  expr <- SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "g.R", on_interrupt = "requeue", runNameLabel = quote(a),
    activeRunningPath = NULL, ss_id = NULL, pane_mode = "reuse", email = NULL, cache_path = NULL,
    dots_path = NULL, snapshot_library = FALSE, on_error = "continue")
  expect_match(expr, 'on_error="continue"', fixed = TRUE)
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

test_that("tmuxRunWorkerLoop turns off the showCache pre-warm before running a job", {
  # setupProject() (sourced by each job) starts the pre-warm fork unless this is off;
  # a worker never harvests it, so each job carried an idle forked process.
  td <- withr::local_tempdir()
  withr::local_options(reproducible.showCachePreWarm = NULL)
  seen <- "not called"
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) {
      seen <<- getOption("reproducible.showCachePreWarm")
      "empty"
    })

  suppressMessages(
    tmuxRunWorkerLoop(queue_path = file.path(td, "q.rds"),
                      global_path = mkGlobal(td)))

  expect_identical(seen, FALSE)
})
