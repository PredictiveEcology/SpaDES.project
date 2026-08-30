## tmuxRunNextWorker(): claim one job from the queue and run it.
##
## Covered here are the guards and the two early returns that happen *before*
## any job is sourced, so no tmux server and no worker R session are involved.
## The Google Sheets backend is reached with .gs_claim_next_job() mocked, which
## is the only part of it that touches the network.

mkGlobal <- function(dir) {
  f <- file.path(dir, "global.R")
  writeLines("invisible(NULL)", f)
  f
}

mkQueue <- function(dir, status = "PENDING") {
  q <- data.frame(.ELFind = paste0("job", seq_along(status)),
                  .rep = seq_along(status),
                  status = status,
                  stringsAsFactors = FALSE)
  p <- file.path(dir, "queue.rds")
  saveRDS(q, p)
  p
}

test_that("tmuxRunNextWorker requires global_path to exist", {
  td <- withr::local_tempdir()

  expect_error(
    tmuxRunNextWorker(queue_path = mkQueue(td), global_path = file.path(td, "nope.R")),
    "file.exists"
  )
})

test_that("tmuxRunNextWorker returns 'empty' when no row is PENDING", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()

  res <- suppressMessages(
    tmuxRunNextWorker(queue_path = mkQueue(td, c("DONE", "DONE")),
                      global_path = mkGlobal(td)))

  expect_identical(res, "empty")
})

test_that("tmuxRunNextWorker rejects a runNameLabel that is not a queue column", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()

  expect_error(
    suppressMessages(
      tmuxRunNextWorker(queue_path = mkQueue(td), global_path = mkGlobal(td),
                        runNameLabel = "notAColumn")),
    "is not a column in the queue"
  )
})

# --- Google Sheets backend: only the claim step is mocked ---------------------

test_that("tmuxRunNextWorker returns 'empty' when the sheet has nothing to claim", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(.gs_claim_next_job = function(...) NULL)

  res <- suppressMessages(
    tmuxRunNextWorker(queue_path = mkQueue(td), global_path = mkGlobal(td),
                      ss_id = "fake-sheet-id"))

  expect_identical(res, "empty")
})

test_that("tmuxRunNextWorker returns 'lost' when another worker won the claim", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .gs_claim_next_job = function(...) structure(list(), class = "gs_claim_lost"))

  res <- suppressMessages(
    tmuxRunNextWorker(queue_path = mkQueue(td), global_path = mkGlobal(td),
                      ss_id = "fake-sheet-id"))

  expect_identical(res, "lost")
})
