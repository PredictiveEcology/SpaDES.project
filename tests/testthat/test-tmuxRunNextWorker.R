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

test_that("a claimed job is named by its queue value, so the pane title ends with the ELF", {
  ## 2026-09-15, FireSense phase 2: panes read "A159568-A159568-444101-" and the log "Claimed job: ".
  ## The fleet passes runNameLabel = quote(colnames(q)[1]); f382a0e evaluated it in the scenario
  ## environment, where `q` is base::q (quit), so the run name was empty.
  td <- withr::local_tempdir()
  q <- data.frame(.ELFind = c("14.3", "5.3.1"), .rep = 1L, status = "PENDING", stringsAsFactors = FALSE)
  qp <- file.path(td, "queue.rds")
  saveRDS(q, qp)
  sheet <- as.data.frame(lapply(q, as.character), stringsAsFactors = FALSE)
  names(sheet) <- gsub("^\\.", "dot", names(sheet))
  titles <- character()
  testthat::local_mocked_bindings(
    .gs_claim_next_job = function(...)
      list(row_index = 1L, sheet_row = 2L,
           col_positions = stats::setNames(seq_along(sheet), names(sheet)),
           data = sheet[1, , drop = FALSE]),
    .gs_write_cells = function(...) invisible(NULL),
    .mirror_local_queue = function(...) invisible(NULL),
    .tmux_run = function(...) {
      a <- c(...)
      if (identical(a[1L], "select-pane")) titles <<- c(titles, a[length(a)])
      invisible(NULL)
    })
  withr::local_options(.spades_pane_prefix = "A159568-42")
  withr::local_envvar(TMUX = "", TMUX_PANE = "%1")

  msgs <- testthat::capture_messages(
    res <- tmuxRunNextWorker(queue_path = qp, global_path = mkGlobal(td), ss_id = "fake-sheet-id",
                             runNameLabel = quote(colnames(q)[1])))

  expect_identical(res, "ok")
  expect_true(any(grepl("Claimed job: 14.3", msgs, fixed = TRUE)))
  expect_identical(titles[1L], "A159568-42-14.3")
})

# --- How a worker records the end of a job ------------------------------------
## 2026-10-10: a row the user set to HOLD while its job ran was written back to
## INTERRUPTED when the job errored, and a job that always fails was re-claimed by
## every worker in turn until the whole queue sat idle.

queueStatus <- function(qp) readRDS(qp)$status

## global.R that logs which job it got, then runs `body`.
mkJobGlobal <- function(dir, body) {
  f <- file.path(dir, "global.R")
  writeLines(c(sprintf("cat(.ELFind, '\\n', sep = '', file = %s, append = TRUE)",
                       deparse1(file.path(dir, "ran.txt"))), body), f)
  f
}

test_that("a row set to HOLD while its job runs stays HOLD when the job errors", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkQueue(td, c("PENDING", "PENDING"))
  g <- mkJobGlobal(td, c(
    sprintf("q <- readRDS(%s); q$status[1] <- 'HOLD'; saveRDS(q, %s)", deparse1(qp), deparse1(qp)),
    "stop('boom')"))

  res <- suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g))

  expect_identical(res, "error")
  expect_identical(queueStatus(qp), c("HOLD", "PENDING"))
  expect_match(readRDS(qp)$last_error[1], "boom")
})

test_that("a row set to HOLD while its job runs stays HOLD when the job finishes", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkQueue(td, "PENDING")
  g <- mkJobGlobal(td, sprintf("q <- readRDS(%s); q$status[1] <- 'HOLD'; saveRDS(q, %s)",
                               deparse1(qp), deparse1(qp)))

  expect_identical(suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g)), "ok")
  expect_identical(queueStatus(qp), "HOLD")
})

test_that("an errored row is marked ERROR and the next claim takes the next PENDING row", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkQueue(td, c("PENDING", "PENDING"))
  g <- mkJobGlobal(td, "stop('always fails')")

  run <- function() suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g))

  expect_identical(run(), "error")
  expect_identical(queueStatus(qp), c("ERROR", "PENDING"))
  expect_identical(run(), "error")
  expect_identical(queueStatus(qp), c("ERROR", "ERROR"))
  expect_identical(readLines(file.path(td, "ran.txt")), c("job1", "job2"))
  expect_identical(run(), "empty")
})

test_that("an ERROR row runs again once it is set back to PENDING", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  qp <- mkQueue(td, "PENDING")
  g <- mkJobGlobal(td, "stop('always fails')")
  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g))
  q <- readRDS(qp); q$status[1] <- "PENDING"; saveRDS(q, qp)

  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g))

  expect_identical(readLines(file.path(td, "ran.txt")), c("job1", "job1"))
})

test_that("an interrupted job is requeued, or marked INTERRUPTED with on_interrupt = 'fail'", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  g <- mkJobGlobal(td, "signalCondition(structure(class = c('interrupt', 'condition'), list(message = '', call = NULL)))")

  qp <- mkQueue(td, "PENDING")
  expect_identical(suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g)), "interrupt")
  expect_identical(queueStatus(qp), "PENDING")

  qp <- mkQueue(td, "PENDING")
  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g, on_interrupt = "fail"))
  expect_identical(queueStatus(qp), "INTERRUPTED")
})

test_that("Sheets backend: an errored job is ERROR, and a HOLD set meanwhile is not overwritten", {
  td <- withr::local_tempdir()
  qp <- mkQueue(td)
  sheet <- data.frame(dotELFind = "job1", dotrep = "1", status = "PENDING", stringsAsFactors = FALSE)
  writes <- list()
  sheetStatus <- "RUNNING"
  testthat::local_mocked_bindings(
    .gs_claim_next_job = function(...)
      list(row_index = 1L, sheet_row = 2L,
           col_positions = stats::setNames(seq_along(sheet), names(sheet)),
           data = sheet),
    .gs_read_queue = function(...) data.frame(status = sheetStatus, stringsAsFactors = FALSE),
    .gs_write_cells = function(ss_id, sheet_row, updates, ...) {
      writes[[length(writes) + 1L]] <<- updates
      invisible(NULL)
    },
    .mirror_local_queue = function(...) invisible(NULL))
  g <- mkJobGlobal(td, "stop('boom')")
  statuses <- function() unlist(lapply(writes, `[[`, "status"))

  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g, ss_id = "fake-sheet-id"))
  expect_true(all(statuses() == "ERROR"))

  writes <- list(); sheetStatus <- "HOLD"
  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g, ss_id = "fake-sheet-id"))
  expect_length(statuses(), 0L)
  expect_match(writes[[length(writes)]]$last_error, "boom")
})

test_that("a list column reaches global.R as the list itself, not wrapped in another list", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  q <- data.frame(.ELFind = "job1", status = "PENDING", stringsAsFactors = FALSE)
  q$.params <- list(list(fireSense_spreadFit = list(spreadLink = "ceiling")))
  qp <- file.path(td, "queue.rds")
  saveRDS(q, qp)
  out <- file.path(td, "params.rds")
  g <- file.path(td, "global.R")
  writeLines(sprintf("saveRDS(.params, '%s')", out), g)

  suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = g))

  expect_identical(readRDS(out), list(fireSense_spreadFit = list(spreadLink = "ceiling")))
})
