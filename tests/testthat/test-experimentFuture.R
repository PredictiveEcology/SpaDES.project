# Basic smoke tests for experimentFuture in local-callr mode with a
# file-backed queue.  These avoid Google Sheets, tmux, and remote SSH.

testthat::test_that("experimentFuture (local, file queue) runs all jobs to DONE", {
  testthat::skip_if_not_installed("callr")

  td <- tempfile("eft"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  global <- file.path(td, "global.R")
  outdir <- file.path(td, "out"); dir.create(outdir)
  queue_path <- file.path(td, "future_queue.rds")
  log_dir    <- file.path(td, "logs"); dir.create(log_dir)

  # Trivial job: write a marker file for this run.
  writeLines(sprintf(
    'saveRDS(list(.ELFind=get(".ELFind"), .rep=get(".rep")),
             file.path("%s", paste0("res_", get(".ELFind"), "_", get(".rep"), ".rds")))',
    outdir
  ), global)

  df <- data.frame(.ELFind = c("A", "B", "C"), .rep = c(1, 1, 1),
                   check.names = FALSE)

  ef <- experimentFuture(
    df          = df,
    global_path = global,
    queue_path  = queue_path,
    cores       = NULL,          # local callr, no SSH
    n_workers   = 2L,
    log_dir     = log_dir
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  # Wait up to 3 minutes for all rows to flip to DONE.
  ok <- wait_for(function() {
    q <- tryCatch(readRDS(queue_path), error = function(e) NULL)
    !is.null(q) && all(q$status == "DONE")
  }, timeout_s = 180)

  testthat::expect_true(ok)
  testthat::expect_setequal(
    list.files(outdir, "^res_.*\\.rds$"),
    c("res_A_1.rds", "res_B_1.rds", "res_C_1.rds")
  )
})

testthat::test_that("experimentFuture returns an experimentFuture handle with expected fields", {
  testthat::skip_if_not_installed("callr")

  td <- tempfile("eft"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  global <- file.path(td, "global.R"); writeLines("Sys.sleep(0.1)", global)
  queue_path <- file.path(td, "future_queue.rds")
  log_dir    <- file.path(td, "logs"); dir.create(log_dir)

  df <- data.frame(.ELFind = "Z", .rep = 1, check.names = FALSE)

  ef <- experimentFuture(
    df          = df,
    global_path = global,
    queue_path  = queue_path,
    cores       = NULL,
    n_workers   = 1L,
    log_dir     = log_dir
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  testthat::expect_s3_class(ef, "experimentFuture")
  testthat::expect_true(all(c("procs", "queue_path", "log_dir") %in% names(ef)))
  testthat::expect_equal(ef$queue_path, queue_path)
  testthat::expect_equal(length(ef$procs), 1L)
})

testthat::test_that("awaitExperimentFuture blocks until workers finish", {
  testthat::skip_if_not_installed("callr")

  td <- tempfile("eft"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  global <- file.path(td, "global.R")
  outdir <- file.path(td, "out"); dir.create(outdir)
  writeLines(sprintf(
    'saveRDS(1, file.path("%s", paste0("done_", get(".rep"), ".rds")))',
    outdir
  ), global)
  queue_path <- file.path(td, "future_queue.rds")
  log_dir    <- file.path(td, "logs"); dir.create(log_dir)

  df <- data.frame(.ELFind = c("X", "Y"), .rep = c(1, 2), check.names = FALSE)

  ef <- experimentFuture(
    df          = df,
    global_path = global,
    queue_path  = queue_path,
    cores       = NULL,
    n_workers   = 2L,
    log_dir     = log_dir
  )
  on.exit(try(killExperimentFuture(ef, force = TRUE), silent = TRUE), add = TRUE)

  # Should return when workers are done; cap test time with a timeout watcher.
  t0 <- Sys.time()
  awaitExperimentFuture(ef, verbose = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  testthat::expect_lt(elapsed, 180)

  q <- readRDS(queue_path)
  testthat::expect_true(all(q$status == "DONE"))
})
