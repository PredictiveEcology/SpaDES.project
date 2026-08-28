## .mirror_local_queue() and .gs_demote_after_kill() from R/googlesheets.R.
##
## .mirror_local_queue() is real filesystem work (filelock + readRDS/saveRDS),
## so it needs no mocking at all. .gs_demote_after_kill() talks to Sheets only
## through two internals, both mocked here.

mkQueue <- function(path, q) {
  saveRDS(q, path)
  path
}

# --- .mirror_local_queue ------------------------------------------------------

test_that(".mirror_local_queue writes updates into the local RDS", {
  skip_if_not_installed("filelock")
  qp <- withr::local_tempfile(fileext = ".rds")
  mkQueue(qp, data.frame(status = c("PENDING", "PENDING"),
                         claimed_by = c(NA_character_, NA_character_),
                         stringsAsFactors = FALSE))

  SpaDES.project:::.mirror_local_queue(qp, row_i = 2L,
                                       updates = list(status = "RUNNING",
                                                      claimed_by = "worker-1"))

  q <- readRDS(qp)
  expect_identical(q$status, c("PENDING", "RUNNING"))
  expect_identical(q$claimed_by, c(NA_character_, "worker-1"))
})

test_that(".mirror_local_queue coerces to the existing column's type", {
  skip_if_not_installed("filelock")
  qp <- withr::local_tempfile(fileext = ".rds")
  mkQueue(qp, data.frame(iter = c(1L, 2L), elapsed = c(1.5, 2.5),
                         label = c("a", "b"), stringsAsFactors = FALSE))

  SpaDES.project:::.mirror_local_queue(qp, row_i = 1L,
    updates = list(iter = "7", elapsed = "9.25", label = 42))

  q <- readRDS(qp)
  expect_type(q$iter, "integer")
  expect_identical(q$iter[[1]], 7L)
  expect_type(q$elapsed, "double")
  expect_identical(q$elapsed[[1]], 9.25)
  expect_type(q$label, "character")
  expect_identical(q$label[[1]], "42")
})

test_that(".mirror_local_queue ignores columns the queue does not have", {
  skip_if_not_installed("filelock")
  qp <- withr::local_tempfile(fileext = ".rds")
  mkQueue(qp, data.frame(status = "PENDING", stringsAsFactors = FALSE))

  SpaDES.project:::.mirror_local_queue(qp, row_i = 1L,
    updates = list(status = "DONE", noSuchColumn = "x"))

  q <- readRDS(qp)
  expect_identical(q$status, "DONE")
  expect_false("noSuchColumn" %in% names(q))
})

test_that(".mirror_local_queue is a no-op for a missing or empty queue_path", {
  skip_if_not_installed("filelock")

  expect_null(SpaDES.project:::.mirror_local_queue(NULL, 1L, list(status = "X")))
  expect_null(SpaDES.project:::.mirror_local_queue("", 1L, list(status = "X")))
  expect_null(SpaDES.project:::.mirror_local_queue(
    file.path(withr::local_tempdir(), "absent.rds"), 1L, list(status = "X")))
})

test_that(".mirror_local_queue traces when asked", {
  skip_if_not_installed("filelock")
  qp <- withr::local_tempfile(fileext = ".rds")
  mkQueue(qp, data.frame(status = "PENDING", stringsAsFactors = FALSE))
  withr::local_options(spades.mirror.trace = TRUE)

  msgs <- capture_messages(
    SpaDES.project:::.mirror_local_queue(qp, 1L, list(status = "DONE", nope = "x"))
  )

  expect_true(any(grepl("\\[mirror\\] WROTE", msgs)))
  expect_true(any(grepl("status=DONE", msgs)))
  # columns that do not exist are reported rather than silently dropped
  expect_true(any(grepl("SKIPPED_COLS", msgs)))
})

test_that(".mirror_local_queue traces the skip for an absent queue", {
  skip_if_not_installed("filelock")
  withr::local_options(spades.mirror.trace = TRUE)

  # a path that does not exist -- see the note below for why NULL is not used
  gone <- file.path(withr::local_tempdir(), "absent.rds")
  msgs <- capture_messages(
    SpaDES.project:::.mirror_local_queue(gone, 3L, list(status = "X"))
  )

  expect_true(any(grepl("\\[mirror\\] SKIP", msgs)))
  expect_true(any(grepl("row=3", msgs)))
})

## Not asserted here, and reported instead: with queue_path = NULL the SKIP
## trace is silent. sprintf() with a zero-length argument returns character(0),
## and message(character(0)) emits only a bare newline, so the diagnostic is
## lost in one of the cases it exists to explain. The function still no-ops
## correctly; only the trace is affected.

# --- .gs_demote_after_kill ----------------------------------------------------

test_that(".gs_demote_after_kill returns 0 for an empty sheet id", {
  expect_identical(SpaDES.project:::.gs_demote_after_kill("", killed_pids = 1L), 0L)
})

test_that(".gs_demote_after_kill returns 0 when the queue cannot be read", {
  testthat::local_mocked_bindings(
    .gs_read_queue = function(...) stop("no network")
  )

  expect_identical(SpaDES.project:::.gs_demote_after_kill("sheet", 1L), 0L)
})

test_that(".gs_demote_after_kill returns 0 when no RUNNING row matches", {
  testthat::local_mocked_bindings(
    .gs_read_queue = function(...) data.frame(
      status = c("PENDING", "DONE"), process_id = c("11", "22"),
      stringsAsFactors = FALSE),
    .gs_write_cells = function(...) stop("must not write")
  )

  expect_identical(SpaDES.project:::.gs_demote_after_kill("sheet", killed_pids = 99L), 0L)
})

test_that(".gs_demote_after_kill demotes matching RUNNING rows to PENDING", {
  seen <- new.env(parent = emptyenv())
  seen$calls <- list()
  testthat::local_mocked_bindings(
    .gs_read_queue = function(...) data.frame(
      status       = c("RUNNING", "RUNNING", "PENDING"),
      process_id   = c("101", "202", "303"),
      claimed_by   = c("w1", "w2", NA_character_),
      started_at   = c("t1", "t2", NA_character_),
      stringsAsFactors = FALSE),
    .gs_write_cells = function(ss_id, sheet_row, updates, ...) {
      seen$calls <- c(seen$calls, list(list(row = sheet_row, updates = updates)))
      invisible(NULL)
    }
  )

  n <- SpaDES.project:::.gs_demote_after_kill("sheet", killed_pids = c(101L, 303L))

  # only row 1 is both RUNNING and in killed_pids
  expect_identical(n, 1L)
  expect_length(seen$calls, 1L)
  # +1 for the header row
  expect_identical(seen$calls[[1]]$row, 2L)
  expect_identical(seen$calls[[1]]$updates$status, "PENDING")
})

test_that(".gs_demote_after_kill clears the run metadata it demotes", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .gs_read_queue = function(...) data.frame(
      status = "RUNNING", process_id = "7", claimed_by = "w",
      stringsAsFactors = FALSE),
    .gs_write_cells = function(ss_id, sheet_row, updates, ...) {
      seen$updates <- updates
      invisible(NULL)
    }
  )

  SpaDES.project:::.gs_demote_after_kill("sheet", killed_pids = 7L)

  # every meta column is wiped, and status is the only one left non-NA
  expect_identical(seen$updates$status, "PENDING")
  meta <- seen$updates[setdiff(names(seen$updates), "status")]
  expect_true(all(vapply(meta, is.na, logical(1))))
  expect_true(all(c("claimed_by", "started_at", "finished_at", "machine_name",
                    "process_id", "heartbeat_at") %in% names(seen$updates)))
})
