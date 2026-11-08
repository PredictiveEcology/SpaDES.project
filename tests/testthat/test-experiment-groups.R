## Queue groups (`.group`, `.after`, `.afterType`): one experiment*() call runs a whole campaign.
## A row's group decides which workers take it (and, on Slurm, its #SBATCH options); `.after`
## holds a row until the groups it names have finished.

mkGroupGlobal <- function(dir, log) {
  f <- file.path(dir, "global.R")
  writeLines(sprintf("cat(.name, '\\n', file = %s, append = TRUE)", deparse1(log)), f)
  f
}

mkGroupQueue <- function(dir, df) {
  qp <- file.path(dir, "queue.rds")
  tmuxPrepareQueueFromDF(df, qp)
  qp
}

## the score row first and the fit rows last: claim order must come from `.after`, not row order
chainDf <- function() data.frame(
  .name      = c("score", "hc", "fit1", "fit2"),
  .group     = c("score", "hc", "fit", "fit"),
  .after     = c("hc", "fit", "", ""),
  .afterType = c("afterany", "afterok", "afterok", "afterok"),
  stringsAsFactors = FALSE)

test_that("an afterany row is claimable once the group it follows can never run", {
  q <- data.frame(.name = c("fit1", "hc1", "hc2", "score"),
                  .group = c("fit", "hc", "hc", "score"),
                  .after = c("", "fit", "fit", "hc"),
                  .afterType = c("afterok", "afterok", "afterok", "afterany"),
                  status = c("ERROR", "PENDING", "PENDING", "PENDING"),
                  stringsAsFactors = FALSE)
  got <- .claimableRow(q, "score")
  expect_identical(got$i, 4L)
  expect_false(got$wait)
  ## hc can still run: score waits
  q$status[2] <- "RUNNING"
  got <- .claimableRow(q, "score")
  expect_true(is.na(got$i))
  expect_true(got$wait)
})

# --- claim order (file queue) ---------------------------------------------------

test_that("a worker takes a row only after the groups in its .after have finished", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  log <- file.path(td, "order.txt")
  qp <- mkGroupQueue(td, chainDf())
  suppressMessages(tmuxRunWorkerLoop(qp, mkGroupGlobal(td, log), on_error = "continue"))
  expect_identical(trimws(readLines(log)), c("fit1", "fit2", "hc", "score"))
  expect_identical(readRDS(qp)$status, rep("DONE", 4L))
})

test_that("a worker given a group takes only that group's rows", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  log <- file.path(td, "order.txt")
  qp <- mkGroupQueue(td, chainDf())
  suppressMessages(tmuxRunWorkerLoop(qp, mkGroupGlobal(td, log), group = "fit"))
  expect_identical(trimws(readLines(log)), c("fit1", "fit2"))
  expect_identical(readRDS(qp)$status, c("PENDING", "PENDING", "DONE", "DONE"))
})

test_that("a row waits while its .after group is unfinished; afterok never runs after an ERROR, afterany does", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir()
  log <- file.path(td, "order.txt")
  g <- mkGroupGlobal(td, log)
  df <- data.frame(.name = c("hc", "fit"), .group = c("hc", "fit"), .after = c("fit", ""),
                   stringsAsFactors = FALSE)
  qp <- mkGroupQueue(td, df)
  q <- readRDS(qp); q$status[2] <- "RUNNING"; saveRDS(q, qp)
  expect_identical(suppressMessages(tmuxRunNextWorker(qp, g, group = "hc")), "wait")

  q$status[2] <- "ERROR"; saveRDS(q, qp)
  expect_identical(suppressMessages(tmuxRunNextWorker(qp, g, group = "hc")), "empty")

  q$.afterType <- "afterany"; saveRDS(q, qp)
  expect_identical(suppressMessages(tmuxRunNextWorker(qp, g, group = "hc")), "ok")
  expect_identical(trimws(readLines(log)), "hc")
})

test_that("a worker loop sleeps on 'wait' and claims again", {
  td <- withr::local_tempdir()
  res <- c("wait", "wait", "ok", "empty")
  i <- 0L
  testthat::local_mocked_bindings(
    tmuxRunNextWorker = function(...) { i <<- i + 1L; res[[i]] },
    .afterPollSeconds = function() 0)
  suppressMessages(tmuxRunWorkerLoop(file.path(td, "q.rds"), mkGroupGlobal(td, file.path(td, "x"))))
  expect_identical(i, 4L)
})

# --- launch-time checks -----------------------------------------------------------

test_that("the queue's groups come in dependency order, and bad .after values are refused", {
  q <- cbind(chainDf(), status = "PENDING")
  gr <- .queueGroups(q)
  expect_identical(gr$group, c("fit", "hc", "score"))
  expect_identical(gr$after, list(character(), "fit", "hc"))
  expect_identical(gr$afterType, c("afterok", "afterok", "afterany"))
  expect_identical(gr$nOpen, c(2L, 1L, 1L))

  expect_null(.queueGroups(data.frame(a = 1, status = "PENDING")))
  bad <- q; bad$.after[1] <- "nope"
  expect_error(.queueGroups(bad), "nope")
  cyc <- q; cyc$.after[3:4] <- "score"
  expect_error(.queueGroups(cyc), "cycle")
  mixed <- q; mixed$.after[4] <- "hc"
  expect_error(.queueGroups(mixed), "fit")
  noGroup <- q; noGroup$.group <- NULL
  expect_error(.queueGroups(noGroup), ".group")
})

test_that("n_workers per group: named entries dedicate workers, the unnamed one is the default", {
  gr <- .queueGroups(cbind(chainDf(), status = "PENDING"))
  ## Slurm: every group has its own workers, at most one per open row
  expect_identical(.workerGroups(c(fit = 2, 4), gr, pool = FALSE), c("fit", "fit", "hc", "score"))
  ## tmux / future: unnamed workers are a pool that takes rows of any group
  expect_identical(.workerGroups(c(fit = 1, 2), gr, pool = TRUE), c("fit", NA, NA))
  expect_error(.workerGroups(c(fit = 1), gr, pool = TRUE), "hc, score")
  expect_error(.workerGroups(c(fitt = 1, 2), gr, pool = TRUE), "fitt")
  expect_identical(.workerGroups(3L, NULL, pool = TRUE), rep(NA_character_, 3L))
  expect_error(.workerGroups(c(fit = 1), NULL, pool = TRUE), ".group")
})

# --- experimentSBATCH ---------------------------------------------------------------

## an sbatch stand-in: prints the next job id (from 101) and records which script it got
fakeSbatch <- function(dir) {
  f <- file.path(dir, "sbatch")
  writeLines(c("#!/bin/bash",
               sprintf("n=$(cat %s/n 2>/dev/null || echo 100); n=$((n+1)); echo $n > %s/n", dir, dir),
               sprintf("echo \"$1\" >> %s/submitted", dir),
               "echo \"Submitted batch job $n on cluster gpsc8\""), f)
  Sys.chmod(f, "0755")
  f
}

test_that("experimentSBATCH submits each group with its own options, after the groups it waits for", {
  skip_on_os("windows")
  td <- withr::local_tempdir()
  es <- suppressMessages(experimentSBATCH(
    df = chainDf(), global_path = mkGroupGlobal(td, file.path(td, "x")),
    n_workers = c(fit = 2, 1), queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    sbatch_opts = list(list(partition = "standard", clusters = "gpsc8"),
                       fit = list(mem = "300G"), score = list(mem = "32G")),
    sbatch_cmd = fakeSbatch(td)))

  expect_identical(es$job_ids, 101:104)
  expect_identical(es$groups, c("fit", "fit", "hc", "score"))
  expect_identical(es$clusters, "gpsc8")
  expect_identical(readLines(file.path(td, "submitted")), es$job_scripts)
  body <- lapply(es$job_scripts, readLines)
  sb <- function(b) grep("^#SBATCH", b, value = TRUE)
  expect_true(all(c("#SBATCH --mem=300G", "#SBATCH --partition=standard") %in% sb(body[[1]])))
  expect_false(any(grepl("dependency", sb(body[[2]]))))
  expect_true("#SBATCH --dependency=afterok:101:102" %in% sb(body[[3]]))
  expect_false(any(grepl("--mem", sb(body[[3]]))))
  expect_true(all(c("#SBATCH --dependency=afterany:103", "#SBATCH --mem=32G") %in% sb(body[[4]])))
  expect_true(any(grepl('group = \\\\?"hc', body[[3]]))) # \" inside the library-snapshot wrapper
  expect_true(any(grepl("job-name=spades-score-01", body[[4]], fixed = TRUE)))
  ## one queue, all rows
  expect_identical(nrow(readRDS(es$queue_path)), 4L)
})

test_that("experimentSBATCH builds the dev library once for all groups", {
  td <- withr::local_tempdir()
  n <- 0L
  testthat::local_mocked_bindings(newDevLibrary = function(...) { n <<- n + 1L; c("/x/dev", "/x/stable") })
  es <- suppressMessages(experimentSBATCH(
    df = chainDf(), global_path = mkGroupGlobal(td, file.path(td, "x")), n_workers = 1L,
    queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    devPackages = "pkg", dry_run = TRUE))
  expect_identical(n, 1L)
  expect_length(es$job_scripts, 3L)
  for (s in es$job_scripts)
    expect_true(any(grepl('\\.libPaths\\(c\\(\\\\?"/x/dev\\\\?", \\\\?"/x/stable\\\\?"\\)\\)', readLines(s)))) # \" inside the shell wrapper (Windows)
})

test_that("experimentSBATCH without .group is as before: n_workers jobs, one options list, no dependency", {
  td <- withr::local_tempdir()
  es <- suppressMessages(experimentSBATCH(
    df = data.frame(.name = c("a", "b")), global_path = mkGroupGlobal(td, file.path(td, "x")),
    n_workers = 3L, queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    sbatch_opts = list(mem = "8G"), dry_run = TRUE))
  expect_identical(basename(es$job_scripts), sprintf("worker_%02d.sh", 1:3))
  b <- readLines(es$job_scripts[[3]])
  expect_true("#SBATCH --mem=8G" %in% b)
  expect_true("#SBATCH --job-name=spades-worker-03" %in% b)
  expect_false(any(grepl("dependency|group =", b)))
})

# --- experimentFuture (local workers) -----------------------------------------------------

test_that("experimentFuture runs the groups in .after order, each on its own workers", {
  skip_if_not_installed("callr")
  skip_if_not_installed("filelock")
  skip_on_os("windows")
  td <- withr::local_tempdir()
  log <- file.path(td, "order.txt")
  withr::local_envvar(SPADES_AFTER_POLL_SECONDS = "0.2") # the workers inherit it
  ef <- suppressMessages(experimentFuture(
    df = chainDf(), global_path = mkGroupGlobal(td, log),
    n_workers = c(fit = 2, hc = 1, score = 1), queue_path = file.path(td, "q.rds"),
    log_dir = file.path(td, "logs"), snapshot_library = FALSE))
  suppressMessages(awaitExperimentFuture(ef, verbose = FALSE))
  expect_length(ef$procs, 4L)
  ran <- trimws(readLines(log))
  expect_setequal(ran[1:2], c("fit1", "fit2"))
  expect_identical(ran[3:4], c("hc", "score"))
})

# --- experimentTmux ------------------------------------------------------------------

test_that("a tmux worker call carries its group, and a worker of no group carries none", {
  expr <- function(group) .build_worker_r_expr(
    queue_path = "/q.rds", global_path = "/g.R", on_interrupt = "requeue", runNameLabel = quote(x),
    activeRunningPath = NULL, ss_id = NULL, pane_mode = "reuse", email = NULL, cache_path = NULL,
    dots_path = NULL, snapshot_library = FALSE, group = group)
  expect_true(grepl('n_workers_on_host=1L, group="fit")', expr("fit"), fixed = TRUE))
  expect_false(grepl("group", expr(NA_character_)))
})
