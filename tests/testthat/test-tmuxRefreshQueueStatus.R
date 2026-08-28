## tmuxRefreshQueueStatus(): re-derive each queue row's status from the
## Running_ marker files on disk. Filesystem + filelock only -- no tmux server
## and no workers. These cover the guards, the corrupt-queue recovery and which
## rows the refresh is allowed to touch.

mkQueueFile <- function(q, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  p <- file.path(dir, "queue.rds")
  saveRDS(q, p)
  p
}

simpleQueue <- function(status = c("PENDING", "PENDING")) {
  data.frame(.ELFind = paste0("job", seq_along(status)),
             .rep = seq_along(status),
             status = status,
             stringsAsFactors = FALSE)
}

test_that("tmuxRefreshQueueStatus is a no-op when the queue file is absent", {
  skip_if_not_installed("filelock")
  gone <- file.path(withr::local_tempdir(), "absent.rds")

  expect_no_error(res <- tmuxRefreshQueueStatus(gone))
  expect_null(res)
  expect_false(file.exists(gone))
})

test_that("tmuxRefreshQueueStatus discards a queue it cannot read", {
  skip_if_not_installed("filelock")
  d <- withr::local_tempdir()
  p <- file.path(d, "queue.rds")
  writeLines("this is not an RDS", p)      # readRDS will fail on it

  res <- suppressWarnings(tmuxRefreshQueueStatus(p))

  # a corrupt queue is removed rather than left to fail every later refresh
  expect_null(res)
  expect_false(file.exists(p))
})

test_that("tmuxRefreshQueueStatus leaves PENDING rows pending with no marker files", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue(c("PENDING", "PENDING")))

  suppressWarnings(tmuxRefreshQueueStatus(p))

  q <- readRDS(p)
  expect_identical(as.character(q$status), c("PENDING", "PENDING"))
})

test_that("tmuxRefreshQueueStatus does not touch DONE rows by default", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue(c("DONE", "PENDING")))

  suppressWarnings(tmuxRefreshQueueStatus(p))

  q <- readRDS(p)
  # only PENDING / RUNNING / INTERRUPTED are re-derived
  expect_identical(as.character(q$status)[[1]], "DONE")
})

test_that("tmuxRefreshQueueStatus leaves a user-defined status alone", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue(c("CANCELLED", "PENDING")))

  suppressWarnings(tmuxRefreshQueueStatus(p))

  q <- readRDS(p)
  expect_identical(as.character(q$status)[[1]], "CANCELLED")
})

test_that("tmuxRefreshQueueStatus adds interrupted_at when the queue lacks it", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue("PENDING"))

  suppressWarnings(tmuxRefreshQueueStatus(p))

  q <- readRDS(p)
  expect_true("interrupted_at" %in% names(q))
})

test_that("tmuxRefreshQueueStatus releases the lock it takes", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue("PENDING"))

  suppressWarnings(tmuxRefreshQueueStatus(p))

  # if the lock were still held, this second acquisition would time out
  lck <- filelock::lock(paste0(p, ".lock"), timeout = 2000)
  expect_false(is.null(lck))
  filelock::unlock(lck)
})

test_that("tmuxRefreshQueueStatus can be run twice without changing the outcome", {
  skip_if_not_installed("filelock")
  p <- mkQueueFile(simpleQueue(c("PENDING", "DONE")))

  suppressWarnings(tmuxRefreshQueueStatus(p))
  first <- as.character(readRDS(p)$status)
  suppressWarnings(tmuxRefreshQueueStatus(p))
  second <- as.character(readRDS(p)$status)

  expect_identical(first, second)
})
