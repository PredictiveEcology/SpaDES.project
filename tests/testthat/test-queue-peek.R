## queueRows(), queueStatus(), queueWaitWhile(), queueSkipForFits(). Offline: the Google lookups are mocked,
## so no Drive credential is needed.

## A queue .rds (key column `.ELFind` unless `key` says otherwise) with a logs folder holding `nWorkers` launchers
makeQueue <- function(ids, status, nWorkers = 0L, key = ".ELFind", dir = withr::local_tempdir(.local_envir = parent.frame())) {
  qf <- file.path(dir, "fits.rds")
  q <- data.frame(ids, status, stringsAsFactors = FALSE)
  names(q) <- c(key, "status")
  saveRDS(q, qf)
  logs <- file.path(dir, "logs", basename(qf))
  dir.create(logs, recursive = TRUE)
  file.create(file.path(logs, sprintf("worker_launch_%02d.sh", seq_len(nWorkers))))
  file.create(file.path(logs, "worker_1.log"))
  qf
}

## the sheet cannot be reached (no credential, offline)
mockSheetUnreachable <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    drive_find = function(...) stop("no Google credential"), .package = "googledrive", .env = env)
}

## the sheet holds `ids`/`status`, written as the queue pushes it: `.ELFind` as `dotELFind`
mockSheet <- function(ids, status, nameSeen = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    drive_find = function(name, ...) {
      if (!is.null(nameSeen)) nameSeen$name <- name
      data.frame(id = "sheet-id")
    },
    .package = "googledrive", .env = env)
  testthat::local_mocked_bindings(
    read_sheet = function(ss, sheet, col_types, ...) {
      stopifnot(identical(ss, "sheet-id"), identical(sheet, "Status"))
      data.frame(dotELFind = ids, status = status, stringsAsFactors = FALSE)
    },
    .package = "googlesheets4", .env = env)
}

test_that("queueRows falls back to the .rds in claim order when the sheet cannot be read", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue(c("6.1", "4.10", "14.3"), c("DONE", "RUNNING", "PENDING"))
  expect_identical(queueRows(qf), data.frame(id = c("6.1", "4.10", "14.3"),
                                             status = c("DONE", "RUNNING", "PENDING")))
})

test_that("queueRows honours `key`", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue(c("a", "b"), c("DONE", "PENDING"), key = ".job")
  expect_identical(queueRows(qf, key = ".job")$id, c("a", "b"))
  expect_null(queueRows(qf))   # no `.ELFind` column
})

test_that("queueRows returns NULL when neither the sheet nor the .rds can be read", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  expect_null(queueRows(file.path(withr::local_tempdir(), "none.rds")))
})

test_that("the sheet wins over a stale .rds, and is found by the file name without .rds", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("googlesheets4")
  qf <- makeQueue(c("6.1", "4.10"), c("PENDING", "PENDING"))
  seen <- new.env()
  mockSheet(c("6.1", "4.10"), c("DONE", "RUNNING"), nameSeen = seen)
  expect_identical(queueRows(qf), data.frame(id = c("6.1", "4.10"), status = c("DONE", "RUNNING")))
  expect_identical(seen$name, "fits")
  expect_identical(queueStatus(qf, "4.10"), "RUNNING")
})

test_that("queueStatus gives the row's status, or NA_character_", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue(c("6.1", "4.10"), c("DONE", "RUNNING"))
  expect_identical(queueStatus(qf, "4.10"), "RUNNING")
  expect_identical(queueStatus(qf, "99"), NA_character_)
  expect_identical(queueStatus(file.path(dirname(qf), "none.rds"), "6.1"), NA_character_)
})

test_that("queueWaitWhile sleeps `every` seconds, with a time-stamped message, until the status changes", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue("6.1", "RUNNING")
  sleeps <- numeric()
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) {
      sleeps <<- c(sleeps, time)
      if (length(sleeps) == 2L) saveRDS(data.frame(.ELFind = "6.1", status = "DONE"), qf)
    },
    .package = "base")
  msgs <- testthat::capture_messages(queueWaitWhile(qf, "6.1"))
  expect_identical(sleeps, c(300, 300))
  expect_length(msgs, 2L)
  expect_match(msgs, "^[0-2][0-9]:[0-5][0-9] waiting for 6\\.1 \\(RUNNING\\) in fits\\.rds")
  expect_identical(queueStatus(qf, "6.1"), "DONE")
})

test_that("queueWaitWhile does not wait for a row that is not in `status`, nor for an unreadable queue", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  testthat::local_mocked_bindings(Sys.sleep = function(time) stop("must not sleep"), .package = "base")
  qf <- makeQueue(c("6.1", "4.10"), c("PENDING", "RUNNING"))
  expect_no_message(queueWaitWhile(qf, "6.1"))
  expect_no_message(queueWaitWhile(qf, "99"))
  expect_no_message(queueWaitWhile(file.path(dirname(qf), "none.rds"), "6.1"))
  expect_no_message(queueWaitWhile(qf, "6.1", status = "DONE"))
  expect_error(queueWaitWhile(qf, "4.10", status = "RUNNING"), "must not sleep")
})

test_that("queueSkipForFits skips an id the fit queue has claimed (RUNNING or DONE)", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue(c("a", "b", "c"), c("RUNNING", "DONE", "PENDING"), nWorkers = 0L)
  expect_match(queueSkipForFits(qf, "a"), "the fit queue has claimed it")
  expect_match(queueSkipForFits(qf, "b"), "the fit queue has claimed it")
  expect_null(queueSkipForFits(qf, "c"))
})

test_that("queueSkipForFits leaves the k topmost PENDING/INTERRUPTED rows to the k idle fit workers", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  ## 4 workers, 1 RUNNING: 3 idle workers take the first 3 of the pending rows (DONE and RUNNING rows do not count)
  qf <- makeQueue(c("d", "r", "p1", "i1", "p2", "p3"),
                  c("DONE", "RUNNING", "PENDING", "INTERRUPTED", "PENDING", "PENDING"), nWorkers = 4L)
  for (id in c("p1", "i1", "p2"))
    expect_match(queueSkipForFits(qf, id), "one of the next 3 .*idle fit workers will claim")
  expect_null(queueSkipForFits(qf, "p3"))
  expect_null(queueSkipForFits(qf, "not-in-queue"))
})

test_that("queueSkipForFits: more RUNNING rows than launchers means no idle worker; no launchers means none either", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  qf <- makeQueue(c("r1", "r2", "p"), c("RUNNING", "RUNNING", "PENDING"), nWorkers = 1L)
  expect_null(queueSkipForFits(qf, "p"))
  qf <- makeQueue("p", "PENDING", nWorkers = 0L)
  expect_null(queueSkipForFits(qf, "p"))
})

test_that("queueSkipForFits does not skip when there is no fit queue or it cannot be read", {
  skip_if_not_installed("googledrive")
  mockSheetUnreachable()
  expect_null(queueSkipForFits(NA_character_, "a"))
  expect_null(queueSkipForFits(NULL, "a"))
  expect_null(queueSkipForFits(file.path(withr::local_tempdir(), "none.rds"), "a"))
})

test_that("queueSkipForFits reads the sheet when it can, and works with another key", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("googlesheets4")
  qf <- makeQueue(c("a", "b"), c("PENDING", "PENDING"), nWorkers = 1L)   # stale .rds: nothing claimed
  testthat::local_mocked_bindings(
    drive_find = function(...) data.frame(id = "sheet-id"), .package = "googledrive")
  testthat::local_mocked_bindings(
    read_sheet = function(ss, sheet, col_types, ...)
      data.frame(dotjob = c("a", "b"), status = c("DONE", "PENDING")),
    .package = "googlesheets4")
  expect_match(queueSkipForFits(qf, "a", key = ".job"), "claimed")
  expect_match(queueSkipForFits(qf, "b", key = ".job"), "next 1 ")
})
