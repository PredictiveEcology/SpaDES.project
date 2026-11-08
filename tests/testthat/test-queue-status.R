## The queue's state model (?experimentStatus): who may change a row's status and to what.
## Each test here is an incident that happened on a real campaign; the comment says which.

mkStatusQueue <- function(dir, status, machine = NA_character_, pid = NA_integer_) {
  q <- data.frame(.ELFind = paste0("job", seq_along(status)), status = status,
                  claimed_by = NA_character_, started_at = NA_character_, finished_at = NA_character_,
                  machine_name = rep_len(machine, length(status)), process_id = rep_len(pid, length(status)),
                  stringsAsFactors = FALSE)
  p <- file.path(dir, "queue.rds")
  saveRDS(q, p)
  p
}

mkStatusGlobal <- function(dir) {
  f <- file.path(dir, "global.R")
  writeLines(sprintf("cat(.ELFind, '\\n', sep = '', file = %s, append = TRUE)",
                     deparse1(file.path(dir, "ran.txt"))), f)
  f
}

## A pid that is certainly dead: a child that has exited and been reaped.
deadPid <- function() {
  testthat::skip_if_not_installed("processx")
  p <- processx::process$new("true")
  p$wait()
  p$get_pid()
}

## No tmux pane title says anything is alive, and no other machine can be reached.
noOtherLiveness <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .tmux_all_pane_titles = function(...) character(0),
    .ssh_pids_alive = function(machine, pids) NULL,
    .env = env)
}

# --- a launch runs what is left -------------------------------------------------------------

## 2026-09-28: a relaunch on the same queue name found every row still HOLD, so 12 workers
## found nothing and sat idle; ERROR rows were never retried either. A new queue name was
## the workaround.
test_that("a launch releases HOLD and ERROR rows and leaves other statuses alone", {
  noOtherLiveness()
  q <- data.frame(status = c("HOLD", "ERROR", "QUARANTINED", "DONE", "PENDING", "INTERRUPTED"),
                  stringsAsFactors = FALSE)
  expect_message(out <- SpaDES.project:::.queueAtLaunch(q), "1 HOLD and 1 ERROR")
  expect_identical(out$status, c("PENDING", "PENDING", "QUARANTINED", "DONE", "PENDING", "INTERRUPTED"))
  expect_silent(SpaDES.project:::.queueAtLaunch(out))
})

test_that("experimentSBATCH on an existing queue releases its HOLD and ERROR rows", {
  skip_on_os("windows")
  noOtherLiveness()
  td <- withr::local_tempdir()
  qp <- mkStatusQueue(td, c("HOLD", "ERROR", "DONE"))
  suppressMessages(experimentSBATCH(global_path = mkStatusGlobal(td), n_workers = 1L, queue_path = qp,
                                    log_dir = file.path(td, "logs"), dry_run = TRUE))
  expect_identical(readRDS(qp)$status, c("PENDING", "PENDING", "DONE"))
})

## 2026-10-19: a resumed launch rewrote RUNNING rows back to PENDING from marker-file
## heuristics, pushed that over the sheet, and two workers ran 6.1.3 and 6.2.1 at once.
test_that("a launch leaves a RUNNING row with a live process alone and reclaims one whose process is dead", {
  noOtherLiveness()
  me <- Sys.info()[["nodename"]]
  q <- data.frame(status = c("RUNNING", "RUNNING", "RUNNING", "RUNNING"),
                  claimed_by = c("a", "b", "c", "d"),
                  machine_name = c(me, me, "otherhost", NA),
                  process_id = c(Sys.getpid(), deadPid(), 12345L, 12345L),
                  stringsAsFactors = FALSE)
  expect_message(out <- SpaDES.project:::.queueAtLaunch(q), "Reclaimed 1 RUNNING")
  ## live process: untouched; dead: back in the queue with the reason; unreachable or no
  ## machine: unknown is not dead
  expect_identical(out$status, c("RUNNING", "INTERRUPTED", "RUNNING", "RUNNING"))
  expect_identical(out$claimed_by, c("a", NA, "c", "d"))
  expect_match(out$last_error[2], paste0(me, "-", q$process_id[2]))
  expect_false(is.na(out$interrupted_at[2]))
})

# --- workers reclaim dead rows ---------------------------------------------------------------

## 2026-10-02 and after: rows stayed RUNNING under dead PIDs after a kill; file queues never
## put them back, so every relaunch needed a new queue name.
test_that("a file worker reclaims a dead local row before claiming, and leaves a live one alone", {
  skip_if_not_installed("filelock")
  noOtherLiveness()
  td <- withr::local_tempdir()
  me <- Sys.info()[["nodename"]]
  qp <- mkStatusQueue(td, c("RUNNING", "RUNNING"), machine = me, pid = c(deadPid(), Sys.getpid()))

  res <- suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = mkStatusGlobal(td)))

  expect_identical(res, "ok")
  q <- readRDS(qp)
  expect_identical(q$status, c("DONE", "RUNNING"))     # the dead row was reclaimed and run
  expect_identical(readLines(file.path(td, "ran.txt")), "job1")
})

test_that("every reclaim path writes the same INTERRUPTED row", {
  skip_if_not_installed("filelock")
  noOtherLiveness()
  td <- withr::local_tempdir()
  me <- Sys.info()[["nodename"]]
  pid <- deadPid()
  want <- SpaDES.project:::.reclaimUpdates(me, pid)
  expect_identical(want$status, "INTERRUPTED")
  expect_true(is.na(want$claimed_by))

  ## the file refresh
  qp <- mkStatusQueue(td, "RUNNING", machine = me, pid = pid)
  q <- suppressMessages(tmuxRefreshQueueStatus(qp))
  expect_identical(as.character(q$status), "INTERRUPTED")
  expect_identical(q$last_error, want$last_error)
  expect_false(is.na(q$interrupted_at))

  ## the sheet, after a kill
  seen <- NULL
  testthat::local_mocked_bindings(
    .gs_read_queue = function(...) data.frame(status = "RUNNING", machine_name = me,
                                              process_id = as.character(pid), claimed_by = "w",
                                              stringsAsFactors = FALSE),
    .gs_write_cells = function(ss_id, sheet_row, updates, ...) { seen <<- updates; invisible(NULL) })
  SpaDES.project:::.gs_demote_after_kill("sheet", killed_pids = pid)
  expect_identical(seen[c("status", "claimed_by", "last_error")], want[c("status", "claimed_by", "last_error")])
})

test_that("the claim writes the same row in both backends", {
  upd <- SpaDES.project:::.claimUpdates("worker-1", machine = "m", pid = 7L, now = "t")
  expect_setequal(names(upd), SpaDES.project:::meta_cols)
  expect_identical(upd[c("status", "claimed_by", "machine_name", "process_id", "started_at")],
                   list(status = "RUNNING", claimed_by = "worker-1", machine_name = "m", process_id = 7L,
                        started_at = "t"))
  expect_true(all(is.na(unlist(upd[setdiff(names(upd), c("status", "claimed_by", "machine_name",
                                                            "process_id", "started_at"))]))))
})

# --- the refresh fills display columns and nothing else -----------------------------------

## Before: PENDING was the default a refresh wrote, a Running_ marker made a row RUNNING, and a
## heartbeat iteration that was a multiple of 25 made it DONE.
test_that("tmuxRefreshQueueStatus writes no status but INTERRUPTED for a dead worker", {
  skip_if_not_installed("filelock")
  noOtherLiveness()
  td <- withr::local_tempdir()
  qp <- mkStatusQueue(td, c("PENDING", "INTERRUPTED", "RUNNING"), machine = "otherhost", pid = 1L)
  ## a marker file of a live local process for the PENDING row
  arp <- tmuxActiveRunningPath(NULL, qp)
  saveRDS("job1", file.path(arp, paste0("Running_job1_", Sys.getpid(), "_.rds")))
  q <- readRDS(qp); q$interrupted_at <- c(NA, "2026-01-01 00:00:00", NA); saveRDS(q, qp)

  out <- suppressMessages(tmuxRefreshQueueStatus(qp, runNameLabel = quote(colnames(q)[1])))

  expect_identical(as.character(out$status), c("PENDING", "INTERRUPTED", "RUNNING"))
  expect_identical(out$interrupted_at[2], "2026-01-01 00:00:00") # history kept, not wiped
})

test_that("tmuxRefreshQueueStatus fills the heartbeat columns from the iteration files", {
  skip_if_not_installed("filelock")
  noOtherLiveness()
  td <- withr::local_tempdir()
  qp <- mkStatusQueue(td, "RUNNING", machine = Sys.info()[["nodename"]], pid = Sys.getpid())
  hists <- file.path(td, "hists"); dir.create(hists)
  file.create(file.path(hists, c("a_iter10_2026-01-01 00:00:00.png", "a_iter25_2026-01-01 00:10:00.png")))

  out <- suppressMessages(tmuxRefreshQueueStatus(qp, runNameLabel = quote(colnames(q)[1]),
                                                 folderWithIterInFilename = hists))

  expect_identical(as.character(out$status), "RUNNING")
  expect_identical(as.integer(out$heartbeat_iter), 25L)
  expect_identical(out$heartbeat_at, "2026-01-01 00:10:00")
})

# --- the sheet is the record, the local file its mirror -------------------------------------

## 2026-10-12: PENDING rows were set to HOLD in the local .rds of a sheet-backed queue; the
## workers claimed them from the sheet anyway. The sync pane also pushed the whole local file
## over the sheet every cycle, so a claim written in between was lost.
test_that("sync: the sheet's status wins in the mirror, and only display columns go up", {
  writes <- list()
  testthat::local_mocked_bindings(
    ## same formals as the real function: `sheet =` is passed by name and would otherwise
    ## partially match `sheet_row`
    .gs_write_cells = function(ss_id, sheet_row, updates, col_positions, sheet = "Status", current_row = NULL) {
      writes[[length(writes) + 1L]] <<- list(row = sheet_row, updates = updates); invisible(NULL) })
  q <- data.frame(.ELFind = c("a", "b"), status = c("HOLD", "DONE"), claimed_by = c(NA, "w"),
                  heartbeat_iter = c(10L, NA), heartbeat_at = c("t10", NA), stringsAsFactors = FALSE)
  gs <- data.frame(dotELFind = c("a", "b"), status = c("PENDING", "RUNNING"), claimed_by = c(NA, "w2"),
                   heartbeat_iter = c(NA, NA), heartbeat_at = c(NA, NA), stringsAsFactors = FALSE)

  out <- SpaDES.project:::.syncMirrorWithSheet(q, gs, "sheet")

  expect_identical(out$status, c("PENDING", "RUNNING"))   # the record wins; a local HOLD is not a HOLD
  expect_identical(out$claimed_by, c(NA, "w2"))
  expect_length(writes, 1L)                               # one row differs in its display columns
  expect_identical(writes[[1]]$row, 2L)
  expect_setequal(names(writes[[1]]$updates), c("heartbeat_iter", "heartbeat_at"))
  expect_false("status" %in% unlist(lapply(writes, function(w) names(w$updates))))
})

# --- groups on Slurm ---------------------------------------------------------------------

## A score job set to run afterany its hindcasts started (Slurm: the hindcast jobs had ended)
## and polled for its whole walltime: the hindcast rows were still PENDING, their job killed.
test_that("in a Slurm worker, open rows of a group whose jobs ended do not hold up afterany", {
  q <- data.frame(.name = c("fit", "hc1", "hc2", "score"),
                  .group = c("fit", "hc", "hc", "score"),
                  .after = c("", "fit", "fit", "hc"),
                  .afterType = c("afterok", "afterok", "afterok", "afterany"),
                  status = c("DONE", "PENDING", "DONE", "PENDING"),
                  stringsAsFactors = FALSE)
  waiting <- SpaDES.project:::.claimableRow(q, "score")
  expect_true(is.na(waiting$i)); expect_true(waiting$wait)   # a tmux pool would still run hc1

  ended <- SpaDES.project:::.claimableRow(q, "score", afterGroupsEnded = TRUE)
  expect_identical(ended$i, 4L); expect_false(ended$wait)

  ## afterok on the same ended group: never, so the worker ends instead of polling
  q$.afterType[4] <- "afterok"
  never <- SpaDES.project:::.claimableRow(q, "score", afterGroupsEnded = TRUE)
  expect_true(is.na(never$i)); expect_false(never$wait)
  ## but a row still RUNNING is waited for either way
  q$status[2] <- "RUNNING"
  expect_true(SpaDES.project:::.claimableRow(q, "score", afterGroupsEnded = TRUE)$wait)
})

## sbatch.R defaulted on_error = "stop": after one error the worker left its other rows PENDING
## and nothing in a batch job could continue from the prompt that does not exist.
test_that("experimentSBATCH job scripts continue after an error and tell the worker its .after groups have ended", {
  skip_on_os("windows")
  td <- withr::local_tempdir()
  es <- suppressMessages(experimentSBATCH(
    df = data.frame(.ELFind = "a", stringsAsFactors = FALSE), global_path = mkStatusGlobal(td),
    n_workers = 1L, queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"), dry_run = TRUE))
  body <- readLines(es$job_scripts[[1]])
  expect_true(any(grepl('on_error = \\\\?"continue', body)))
  expect_true(any(grepl("afterGroupsEnded = TRUE", body, fixed = TRUE)))
})

# --- compatibility --------------------------------------------------------------------------

test_that("a queue written before machine_name and process_id existed is launched and claimed", {
  skip_if_not_installed("filelock")
  noOtherLiveness()
  td <- withr::local_tempdir()
  qp <- file.path(td, "queue.rds")
  saveRDS(data.frame(.ELFind = c("a", "b"), status = c("RUNNING", "ERROR"), claimed_by = c("%1", NA),
                     started_at = c("t", NA), finished_at = c(NA, NA), stringsAsFactors = FALSE), qp)

  out <- suppressMessages(SpaDES.project:::.queue_modify(qp, SpaDES.project:::.queueAtLaunch))
  expect_identical(out$status, c("RUNNING", "PENDING"))   # no pid to judge: RUNNING is left alone

  expect_identical(suppressMessages(tmuxRunNextWorker(queue_path = qp, global_path = mkStatusGlobal(td))), "ok")
  q <- readRDS(qp)
  expect_identical(q$status, c("RUNNING", "DONE"))
  expect_true(all(c("machine_name", "process_id") %in% names(q)))
})
