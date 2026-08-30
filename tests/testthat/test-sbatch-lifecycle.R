## experimentSBATCH() / awaitExperimentSBATCH() / killExperimentSBATCH().
##
## No Slurm controller involved: dry_run = TRUE covers submission, and the two
## lifecycle functions reach Slurm only through system2() and
## .sbatch_squeue_alive(), both mocked. What is asserted is the handle that is
## built, the guards, and the commands that would be issued.

mkGlobal <- function(dir) {
  f <- file.path(dir, "global.R")
  writeLines("invisible(NULL)", f)
  f
}

mkDf <- function(n = 2) {
  data.frame(.ELFind = paste0("job", seq_len(n)), .rep = seq_len(n),
             check.names = FALSE)
}

# --- experimentSBATCH, dry run ------------------------------------------------

test_that("experimentSBATCH dry_run builds a handle without submitting", {
  td <- withr::local_tempdir()

  es <- suppressMessages(experimentSBATCH(
    df = mkDf(2), global_path = mkGlobal(td), n_workers = 2L,
    queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    dry_run = TRUE))

  expect_s3_class(es, "experimentSBATCH")
  expect_length(es$job_ids, 2L)
  # nothing was submitted, so every id is NA
  expect_true(all(vapply(es$job_ids, is.na, logical(1))))
})

test_that("experimentSBATCH writes one job script per worker", {
  td <- withr::local_tempdir()

  es <- suppressMessages(experimentSBATCH(
    df = mkDf(3), global_path = mkGlobal(td), n_workers = 3L,
    queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    dry_run = TRUE))

  scripts <- unlist(es$job_scripts)
  expect_length(scripts, 3L)
  expect_true(all(file.exists(scripts)))
  body <- readLines(scripts[[1]])
  expect_identical(body[[1]], "#!/bin/bash")
  expect_true(any(grepl("tmuxRunWorkerLoop", body)))
})

test_that("experimentSBATCH records the queue and creates it on disk", {
  td <- withr::local_tempdir()
  qp <- file.path(td, "q.rds")

  es <- suppressMessages(experimentSBATCH(
    df = mkDf(2), global_path = mkGlobal(td), n_workers = 1L,
    queue_path = qp, log_dir = file.path(td, "logs"), dry_run = TRUE))

  expect_identical(normalizePath(es$queue_path), normalizePath(qp))
  expect_true(file.exists(qp))
  q <- readRDS(qp)
  expect_true("status" %in% names(q))
  expect_identical(nrow(q), 2L)
})

test_that("experimentSBATCH passes sbatch_opts into the scripts", {
  td <- withr::local_tempdir()

  es <- suppressMessages(experimentSBATCH(
    df = mkDf(1), global_path = mkGlobal(td), n_workers = 1L,
    queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    sbatch_opts = list(mem = "8G", cpus_per_task = 4), dry_run = TRUE))

  body <- readLines(unlist(es$job_scripts)[[1]])
  expect_true("#SBATCH --mem=8G" %in% body)
  expect_true("#SBATCH --cpus-per-task=4" %in% body)
})

test_that("experimentSBATCH refuses to submit without an sbatch executable", {
  td <- withr::local_tempdir()
  # a path that certainly is not an executable
  expect_error(
    suppressMessages(experimentSBATCH(
      df = mkDf(1), global_path = mkGlobal(td), n_workers = 1L,
      queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
      sbatch_cmd = file.path(td, "no-such-sbatch"), dry_run = FALSE)),
    "Could not find sbatch executable"
  )
})

# --- awaitExperimentSBATCH ----------------------------------------------------

test_that("awaitExperimentSBATCH rejects a non-handle", {
  expect_error(awaitExperimentSBATCH(list()), "inherits")
})

test_that("awaitExperimentSBATCH returns straight away for a dry-run handle", {
  es <- structure(list(job_ids = list(NA_integer_), queue_path = NULL),
                  class = "experimentSBATCH")

  msgs <- capture_messages(res <- awaitExperimentSBATCH(es))

  expect_true(any(grepl("No live job IDs", msgs)))
  expect_identical(res, es)
})

test_that("awaitExperimentSBATCH polls until nothing is alive", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .sbatch_squeue_alive = function(ids) {
      calls <<- calls + 1L
      if (calls < 3L) rep(TRUE, length(ids)) else rep(FALSE, length(ids))
    }
  )
  es <- structure(list(job_ids = list(1L, 2L), queue_path = NULL),
                  class = "experimentSBATCH")

  msgs <- capture_messages(awaitExperimentSBATCH(es, interval_s = 0))

  expect_identical(calls, 3L)
  expect_true(any(grepl("All SBATCH job\\(s\\) finished", msgs)))
})

# --- killExperimentSBATCH -----------------------------------------------------

test_that("killExperimentSBATCH creates a stop file per worker by default", {
  td <- withr::local_tempdir()
  sf <- file.path(td, c("stop1", "stop2"))
  es <- structure(list(job_ids = list(1L, 2L), stop_files = as.list(sf)),
                  class = "experimentSBATCH")

  msgs <- capture_messages(killExperimentSBATCH(es))

  expect_true(all(file.exists(sf)))
  expect_true(any(grepl("Stop files created for 2 worker", msgs)))
})

test_that("killExperimentSBATCH counts only the stop files it had to create", {
  td <- withr::local_tempdir()
  sf <- file.path(td, c("stop1", "stop2"))
  file.create(sf[[1]])
  es <- structure(list(job_ids = list(1L), stop_files = as.list(sf)),
                  class = "experimentSBATCH")

  msgs <- capture_messages(killExperimentSBATCH(es))

  expect_true(any(grepl("Stop files created for 1 worker", msgs)))
})

test_that("killExperimentSBATCH force refuses without an scancel executable", {
  td <- withr::local_tempdir()
  es <- structure(list(job_ids = list(1L), stop_files = list()),
                  class = "experimentSBATCH")

  expect_error(
    killExperimentSBATCH(es, force = TRUE,
                         scancel_cmd = file.path(td, "no-such-scancel")),
    "Could not find scancel executable"
  )
})

test_that("killExperimentSBATCH force sends every live id to scancel", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    Sys.which = function(...) "/usr/bin/scancel",
    system2 = function(command, args, ...) {
      cap$command <- command; cap$args <- args; ""
    },
    .package = "base"
  )
  es <- structure(list(job_ids = list(11L, NA_integer_, 22L), stop_files = list()),
                  class = "experimentSBATCH")

  suppressMessages(killExperimentSBATCH(es, force = TRUE))

  expect_identical(cap$command, "scancel")
  # NA ids are dropped, live ones passed through
  expect_setequal(cap$args, c("11", "22"))
})

test_that("killExperimentSBATCH force reports when there is nothing to cancel", {
  es <- structure(list(job_ids = list(NA_integer_), stop_files = list()),
                  class = "experimentSBATCH")

  msgs <- capture_messages(res <- killExperimentSBATCH(es, force = TRUE))

  expect_true(any(grepl("No live job IDs to cancel", msgs)))
  expect_identical(res, es)
})
