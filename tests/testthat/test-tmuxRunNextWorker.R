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

test_that("tmuxRunNextWorker hands global.R the queue's values with their types, not 14.3 or 4.1", {
  td <- withr::local_tempdir()
  seen <- file.path(td, "seen.rds")
  g <- file.path(td, "global.R")
  writeLines(sprintf("saveRDS(list(ELFind = .ELFind, rep = .rep, modules = .modules), %s)",
                     deparse1(seen)), g)
  q <- data.frame(.ELFind = c("4.10", "14.3", "5.3.1"), .rep = 1:3,
                  status = "PENDING", stringsAsFactors = FALSE)
  q$.modules <- list(c("a", "b"), "c", c("d", "e"))
  qp <- file.path(td, "queue.rds")
  saveRDS(q, qp)
  ## the claimed row as .gs_read_queue() returns it: all text, `.col` stored as `dotcol`
  sheet <- as.data.frame(lapply(q, as.character), stringsAsFactors = FALSE)
  names(sheet) <- gsub("^\\.", "dot", names(sheet))

  for (i in seq_len(nrow(q))) {
    testthat::local_mocked_bindings(
      .gs_claim_next_job = function(...)
        list(row_index = i, sheet_row = i + 1L,
             col_positions = stats::setNames(seq_along(sheet), names(sheet)),
             data = sheet[i, , drop = FALSE]),
      .gs_write_cells = function(...) invisible(NULL),
      .mirror_local_queue = function(...) invisible(NULL))

    res <- suppressMessages(
      tmuxRunNextWorker(queue_path = qp, global_path = g, ss_id = "fake-sheet-id"))

    expect_identical(res, "ok")
    got <- readRDS(seen)
    expect_identical(got$ELFind, q$.ELFind[i])
    expect_identical(got$rep, q$.rep[i])
    expect_identical(got$modules, q$.modules[[i]])
  }
})
