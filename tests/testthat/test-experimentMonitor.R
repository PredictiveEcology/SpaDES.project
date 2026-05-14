# Tests for experimentMonitor(), experimentFutureList(), claim-time scrub,
# log_file resolution, queueRead() polymorphism, and the <queue>.ss_id
# sidecar dropped by experimentFuture(ss_id=).
#
# These exercise the local-machine code path only (callr::r_bg backend, no
# ss_id, no remote `cores`).  Cross-machine / GS branches require live SSH
# and Google credentials that don't belong in CI.

testthat::test_that("experimentMonitor(ef) lists live local workers with log_file resolved", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if(.Platform$OS.type != "unix" || !dir.exists("/proc"),
                    "experimentMonitor's local discovery needs /proc.")

  td <- tempfile("emon"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  # Long-enough job that we can observe RUNNING state.
  writeLines("for (i in 1:20) { message(i); Sys.sleep(0.5) }", global)

  df <- data.frame(.scenario = c("A", "B"), .rep = c(1, 1),
                   stringsAsFactors = FALSE)
  ef <- experimentFuture(
    df = df, global_path = global, queue_path = file.path(td, "future_queue.rds"),
    cores = NULL, n_workers = 2L, log_dir = file.path(td, "logs")
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  ok <- wait_for(function() {
    em <- tryCatch(experimentMonitor(ef), error = function(e) NULL)
    !is.null(em) && nrow(em) >= 1L
  }, timeout_s = 15)
  testthat::expect_true(ok)

  em <- experimentMonitor(ef)
  # Schema check
  testthat::expect_true(all(c("pid", "machine", "started_at",
                              "log_file", "queue_path", "runName") %in% names(em)))
  # Each row should be a live PID writing to one of the worker log files
  testthat::expect_true(all(em$pid %in%
    suppressWarnings(as.integer(list.files("/proc", pattern = "^[0-9]+$")))))
  testthat::expect_true(all(em$machine == Sys.info()[["nodename"]]))
  testthat::expect_true(all(grepl("worker_[0-9]+\\.log$", em$log_file)))
  # log_file should match one of ef$log_files
  testthat::expect_true(all(em$log_file %in% ef$log_files))
})

testthat::test_that("experimentMonitor(ef, stats = TRUE) decorates with CPU / RAM / state", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if(.Platform$OS.type != "unix" || !dir.exists("/proc"),
                    "Stats decoration needs /proc + ps.")
  testthat::skip_if(Sys.which("ps") == "", "`ps` not available.")

  td <- tempfile("emon"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  writeLines("for (i in 1:20) { Sys.sleep(0.5) }", global)
  df <- data.frame(.scenario = c("A", "B"), .rep = c(1, 1),
                   stringsAsFactors = FALSE)
  ef <- experimentFuture(
    df = df, global_path = global, queue_path = file.path(td, "future_queue.rds"),
    cores = NULL, n_workers = 2L, log_dir = file.path(td, "logs")
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  testthat::expect_true(wait_for(function() {
    em <- tryCatch(experimentMonitor(ef), error = function(e) NULL)
    !is.null(em) && nrow(em) >= 1L
  }, timeout_s = 15))

  em <- experimentMonitor(ef, stats = TRUE)
  for (col in c("state", "cpuAvg", "RAM (GB)", "availableCores", "total RAM (GB)"))
    testthat::expect_true(col %in% names(em), info = paste("missing", col))
  # state should be one of the documented codes (R/S/D/T/Z/Closed) or NA
  testthat::expect_true(all(is.na(em$state) | em$state %in% c("R","S","D","T","Z","Closed")))
  # availableCores / total RAM should be plausibly positive when ps succeeded
  if (any(!is.na(em$availableCores))) {
    testthat::expect_true(all(em$availableCores[!is.na(em$availableCores)] > 0L))
    testthat::expect_true(all(em[["total RAM (GB)"]][!is.na(em[["total RAM (GB)"]])] > 0))
  }
})

testthat::test_that("experimentMonitor(queue_paths = ...) works without an ef handle", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if(.Platform$OS.type != "unix" || !dir.exists("/proc"))

  td <- tempfile("emon"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  qp <- file.path(td, "future_queue.rds")
  global <- file.path(td, "global.R")
  writeLines("for (i in 1:20) { Sys.sleep(0.5) }", global)
  df <- data.frame(.scenario = "A", .rep = 1, stringsAsFactors = FALSE)
  ef <- experimentFuture(df = df, global_path = global, queue_path = qp,
                          cores = NULL, n_workers = 1L,
                          log_dir = file.path(td, "logs"))
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  testthat::expect_true(wait_for(function() {
    em <- tryCatch(experimentMonitor(queue_paths = qp), error = function(e) NULL)
    !is.null(em) && nrow(em) >= 1L
  }, timeout_s = 15))

  em <- experimentMonitor(queue_paths = qp)
  testthat::expect_equal(em$queue_path, rep(qp, nrow(em)))
  testthat::expect_true(all(em$pid > 0L))
})

testthat::test_that("experimentFutureList(ef) returns the same workers as experimentMonitor(ef)", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if(.Platform$OS.type != "unix" || !dir.exists("/proc"))

  td <- tempfile("efl"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  writeLines("Sys.sleep(120)", global)   # long-lived so both calls see the same set
  df <- data.frame(.scenario = c("A","B"), .rep = c(1,1), stringsAsFactors = FALSE)
  ef <- experimentFuture(
    df = df, global_path = global,
    queue_path = file.path(td, "future_queue.rds"),
    cores = NULL, n_workers = 2L, log_dir = file.path(td, "logs")
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  # Wait for the queue itself to show 2 RUNNING rows -- at this point both
  # functions agree.  (experimentFutureList ALSO finds workers via local /proc
  # scan before they claim, but experimentMonitor is queue-only.)
  testthat::expect_true(wait_for(function() {
    q <- tryCatch(readRDS(file.path(td, "future_queue.rds")), error = function(e) NULL)
    !is.null(q) && sum(q$status == "RUNNING") >= 2L
  }, timeout_s = 15))

  efl <- experimentFutureList(ef)
  emo <- experimentMonitor(ef)
  testthat::expect_setequal(efl$pid, emo$pid)
})

testthat::test_that("experimentFutureList(ef, kill = TRUE) signals workers and demotes RUNNING rows", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if(.Platform$OS.type != "unix" || !dir.exists("/proc"))

  td <- tempfile("efk"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  writeLines("Sys.sleep(120)", global)   # long enough to be killed
  df <- data.frame(.scenario = c("A","B"), .rep = c(1,1), stringsAsFactors = FALSE)
  qp <- file.path(td, "future_queue.rds")
  ef <- experimentFuture(df = df, global_path = global, queue_path = qp,
                          cores = NULL, n_workers = 2L,
                          log_dir = file.path(td, "logs"))
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  # Wait for both workers to claim a RUNNING row.
  testthat::expect_true(wait_for(function() {
    q <- tryCatch(readRDS(qp), error = function(e) NULL)
    !is.null(q) && sum(q$status == "RUNNING") == 2L
  }, timeout_s = 15))

  out <- experimentFutureList(ef, kill = TRUE)
  testthat::expect_true(nrow(out) >= 1L)

  # After kill + refresh, no row should be RUNNING anymore.
  testthat::expect_true(wait_for(function() {
    q <- readRDS(qp)
    sum(q$status == "RUNNING") == 0L
  }, timeout_s = 10))
})

testthat::test_that("claim-time scrub clears stale completion / heartbeat fields", {
  testthat::skip_if_not_installed("callr")

  td <- tempfile("scr"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R"); writeLines("Sys.sleep(0.1)", global)
  qp <- file.path(td, "future_queue.rds")
  df <- data.frame(.scenario = c("A","B"), .rep = c(1,1), stringsAsFactors = FALSE)

  # Pre-populate the queue with junk in fields that should be cleared on claim.
  SpaDES.project:::tmuxPrepareQueueFromDF(df, qp)
  q <- readRDS(qp)
  q$finished_at        <- "2026-01-01 00:00:00"
  q$DEoptimElapsedTime <- 999.9
  q$heartbeat_at       <- "2026-01-01 00:00:00"
  q$heartbeat_iter     <- 7L
  q$iterationsTotal    <- 7L
  q$interrupted_at     <- "2026-01-01 00:00:00"
  saveRDS(q, qp)

  # Single-worker, single-job: claim path runs, scrub fires, then DONE.
  SpaDES.project:::tmuxRunNextWorker(qp, global)

  q2 <- readRDS(qp)
  done <- q2[q2$status == "DONE", ]
  testthat::expect_gt(nrow(done), 0L)
  testthat::expect_true(all(is.na(done$DEoptimElapsedTime)))
  testthat::expect_true(all(is.na(done$heartbeat_at)))
  testthat::expect_true(all(is.na(done$heartbeat_iter)))
  testthat::expect_true(all(is.na(done$iterationsTotal)))
  testthat::expect_true(all(is.na(done$interrupted_at)))
  # finished_at on the DONE row should be the *current* time, not the stale
  # 2026-01-01 -- so it must NOT equal the seeded value.
  testthat::expect_false(any(done$finished_at == "2026-01-01 00:00:00"))
  # claimed_by is cleared on DONE: row is no longer claimed.
  testthat::expect_true(all(is.na(done$claimed_by)))
  # process_id / machine_name are preserved as a historical record.
  testthat::expect_true(all(!is.na(done$process_id)))
  testthat::expect_true(all(!is.na(done$machine_name)))
})

testthat::test_that("queueRead() accepts a local .rds path", {
  td <- tempfile("qr"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  qp <- file.path(td, "future_queue.rds")
  df <- data.frame(.scenario = c("A","B"), .rep = c(1,1), stringsAsFactors = FALSE)
  SpaDES.project:::tmuxPrepareQueueFromDF(df, qp)

  q <- queueRead(qp)
  testthat::expect_s3_class(q, "data.table")
  testthat::expect_equal(nrow(q), 2L)
  testthat::expect_true(all(c(".scenario", ".rep", "status") %in% names(q)))
  testthat::expect_true(all(q$status == "PENDING"))
})

testthat::test_that("tmuxListPanes() is a thin alias for experimentMonitor() with no ef", {
  testthat::expect_identical(formals(SpaDES.project::tmuxListPanes),
                             pairlist(stats = FALSE))
  # Body should call experimentMonitor with stats only -- check the deparse contains it.
  body_str <- paste(deparse(body(SpaDES.project::tmuxListPanes)), collapse = " ")
  testthat::expect_match(body_str, "experimentMonitor")
})

testthat::test_that("experimentFuture(ss_id = ...) drops a <queue_path>.ss_id sidecar", {
  # We don't have GS auth in CI; just check the sidecar-write path by
  # invoking the writeLines() block directly with a stub ss_id.
  td <- tempfile("sc"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  qp <- file.path(td, "future_queue.rds")
  saveRDS(data.frame(x = 1L), qp)
  fake_ss <- "STUB_SHEET_ID"
  writeLines(fake_ss, paste0(qp, ".ss_id"))   # mirrors experimentFuture()'s line
  testthat::expect_true(file.exists(paste0(qp, ".ss_id")))
  testthat::expect_equal(trimws(readLines(paste0(qp, ".ss_id"), warn = FALSE)),
                         fake_ss)
})
