# The experiment queue's statuses and every write that changes one. Both backends -- the
# local `.rds` file and the Google Sheet -- and every launcher go through these functions,
# so there is one answer to "what may a row become, and who decides". See ?experimentStatus.

txtPending     <- "PENDING"
txtInterrupted <- "INTERRUPTED"
txtRunning     <- "RUNNING"
txtDone        <- "DONE"
txtError       <- "ERROR"
txtHold        <- "HOLD"

## The queue's columns that are not the user's data. `.recordCols` are written by claims and
## job ends (the record of who ran what and how it ended); `.displayCols` are filled from
## the job's output files by tmuxRefreshQueueStatus() for people watching the queue.
.recordCols  <- c("status", "claimed_by", "started_at", "finished_at", "machine_name",
                  "process_id", "pid_ns", "interrupted_at", "last_error")
.displayCols <- c("DEoptimElapsedTime", "heartbeat_at", "heartbeat_iter", "iterationsTotal")
meta_cols <- c("status", "claimed_by", "started_at", "finished_at",
               "DEoptimElapsedTime", "machine_name", "process_id", "pid_ns",
               "heartbeat_at", "heartbeat_iter", "iterationsTotal",
               "interrupted_at", "last_error")

.openStatus   <- function() c(txtPending, txtInterrupted)             # a worker may claim
.activeStatus <- function() c(txtPending, txtInterrupted, txtRunning) # not finished

#' The experiment queue's statuses
#'
#' @description
#' Every row of a queue made by [experimentTmux()], [experimentSBATCH()] or
#' [experimentFuture()] has one `status`. The package knows five, and one more belongs
#' to you:
#' \describe{
#'   \item{`PENDING`}{Open: a worker may claim it. Set when the queue is made, when a
#'     launch releases a `HOLD` or `ERROR` row, and when an interrupted job is requeued
#'     (`on_interrupt = "requeue"`).}
#'   \item{`INTERRUPTED`}{Open too, and a previous attempt did not finish:
#'     `interrupted_at` and `last_error` say why. Set when a job is interrupted with
#'     `on_interrupt = "fail"`, when R quits inside a job, and when a `RUNNING` row's
#'     process is found dead.}
#'   \item{`RUNNING`}{One worker owns it: `claimed_by`, `machine_name`, `process_id`,
#'     `pid_ns`, `started_at`. Only a claim writes `RUNNING`.}
#'   \item{`DONE`}{The job finished (`finished_at`).}
#'   \item{`ERROR`}{The job errored (`last_error`). No worker claims it; the next launch on
#'     this queue sets it back to `PENDING`, or set it yourself.}
#'   \item{`HOLD`}{Paused by you: no worker claims it, a job end never overwrites it, and
#'     the next launch on this queue sets it back to `PENDING`. Set it on the queue's
#'     record: the Google Sheet when the launch had `ss_id`, else the `.rds` file.}
#'   \item{anything else}{Yours. The package never claims, changes or releases such a row
#'     (`QUARANTINED`, say).}
#' }
#' What changes a status:
#' \itemize{
#'   \item a claim: an open row becomes `RUNNING` (both backends, through the same code);
#'   \item a job end: a `RUNNING` row becomes `DONE`, `ERROR`, `PENDING` or `INTERRUPTED`.
#'     A status you set while the job ran is kept; only `last_error` and the times are
#'     recorded;
#'   \item a reclaim: a `RUNNING` row whose process is known dead becomes `INTERRUPTED`.
#'     The launcher does this for the whole queue before any worker starts; a worker does
#'     it before each claim (the file backend for rows of its own machine, under the
#'     queue lock; the sheet backend for every machine, checking the row again before
#'     writing); [tmuxRefreshQueueStatus()] does it for the whole queue. A process is
#'     dead when `/proc` (locally, or over one `ssh` per machine) says so; a machine that
#'     cannot be reached, a row without a pid, and a row whose `pid_ns` (the PID namespace
#'     the claim recorded, which a Slurm job has of its own) is not the one the check runs
#'     in, are left alone;
#'   \item a launch: `HOLD` and `ERROR` rows become `PENDING`. A `RUNNING` row with a live
#'     process is never touched by a launch.
#' }
#' With `ss_id`, the sheet is the record and the local `.rds` is a mirror of it: workers
#' write the sheet first, then the mirror, and the sync pane copies the sheet's status and
#' claim columns down and the display columns (`heartbeat_*`, `DEoptimElapsedTime`,
#' `iterationsTotal`) up. A status written into the mirror is overwritten by the next
#' sync.
#'
#' Rows in groups (`?experimentGroups`) are claimed only when the groups in their `.after`
#' permit it.
#' @name experimentStatus
NULL

## Coerce a value to the type of an existing column, as the sheet (all text) and the local
## file (typed) have to agree.
.coerceLike <- function(val, cur) {
  tryCatch(
    if (is.integer(cur)) suppressWarnings(as.integer(val))
    else if (is.numeric(cur)) suppressWarnings(as.numeric(val))
    else if (is.logical(cur)) as.logical(val)
    else as.character(val),
    error = function(e) val)
}

## Write the named values `upd` into row `i` of the queue `q`. A column the queue lacks is
## added (as character) when the value is not NA, so a queue made before a column existed
## starts recording it; an NA into a missing column is nothing to record.
.applyUpdates <- function(q, i, upd) {
  for (nm in names(upd)) {
    val <- upd[[nm]]
    if (!nm %in% names(q)) {
      if (all(is.na(val))) next
      q[[nm]] <- rep(NA_character_, NROW(q))
    }
    q[[nm]][i] <- .coerceLike(val, q[[nm]])
  }
  q
}

## This process's PID namespace, e.g. "pid:[4026531836]"; NA where there is none to read
## (macOS, Windows). A PID means something only inside its namespace: Slurm gives every job
## its own, so two jobs on one node can hold the same machine_name and the same PIDs.
.pidNs <- function() {
  ns <- Sys.readlink("/proc/self/ns/pid")
  if (is.na(ns) || !nzchar(ns)) NA_character_ else ns
}

## Does a row's `rowNs` name the namespace `ns` that PIDs are being checked in? Both NA
## (no namespaces on this platform) counts as the same.
.sameNs <- function(rowNs, ns) identical(as.character(rowNs), as.character(ns))

## What a claim writes: the row is RUNNING under this worker, and every other record and
## display field of an earlier attempt is cleared, so RUNNING never carries a finished_at,
## heartbeat or last_error from before.
.claimUpdates <- function(worker_id, machine = Sys.info()[["nodename"]], pid = Sys.getpid(),
                          pid_ns = .pidNs(), now = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  upd <- stats::setNames(rep(list(NA_character_), length(meta_cols)), meta_cols)
  upd$status       <- txtRunning
  upd$claimed_by   <- worker_id
  upd$started_at   <- now
  upd$machine_name <- machine
  upd$process_id   <- pid
  upd$pid_ns       <- pid_ns
  upd
}

# What a worker writes to its row when the job ends. Only a row still RUNNING gets a
# new status: if the user set it to something else while the job ran (HOLD, say), that
# is theirs to change, so only the error text and timestamps are recorded.
# `outcome`: "ok", "error", "interrupt" (Ctrl-C) or "quit" (R exited inside the job).
# An errored job is ERROR, which no worker claims; a launch sets it back to PENDING.
.jobEndUpdates <- function(outcome, on_interrupt, currentStatus, lastErr = NULL) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  status <- switch(outcome,
    ok        = txtDone,
    error     = txtError,
    interrupt = if (on_interrupt == "requeue") txtPending else txtInterrupted,
    quit      = txtInterrupted)
  upd <- list(status = status, claimed_by = NA_character_)
  if (outcome == "ok") {
    upd$finished_at <- now
    upd$last_error  <- NA_character_
  } else {
    if (status != txtPending) upd$interrupted_at <- now
    upd$last_error <- lastErr
  }
  if (!is.na(currentStatus) && !identical(currentStatus, txtRunning))
    upd$status <- NULL
  upd
}

# File backend: apply .jobEndUpdates() to row `i` of the queue `q`.
.applyJobEnd <- function(q, i, outcome, on_interrupt, lastErr = NULL)
  .applyUpdates(q, i, .jobEndUpdates(outcome, on_interrupt, q$status[i], lastErr))

## What a reclaim writes: the RUNNING row of a dead worker goes back in the queue as
## INTERRUPTED, with the reason. The same for every path that finds a dead worker.
.reclaimUpdates <- function(machine, pid, now = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  worker <- paste(c(if (length(machine)) machine else NA, pid)[!is.na(c(machine, pid))], collapse = "-")
  list(status = txtInterrupted, claimed_by = NA_character_, interrupted_at = now,
       last_error = paste("worker", worker, "ended without recording the job"))
}

## Is each of `pids` alive on `machine`? One ssh; a zombie (State: Z in /proc/<pid>/status)
## is dead, as in .pidIsAlive(). NULL when the machine cannot be reached: unreachable is not
## proof of death. The ssh shell is in the host's PID namespace, so a pid recorded in another
## one (`pid_ns`, one per pid, as .pidNs() writes it) cannot be judged: NA, not dead.
.ssh_pids_alive <- function(machine, pids, pid_ns = NULL) {
  if (length(pids) == 0L) return(logical(0))
  cmd <- paste0(
    "echo ns:$(readlink /proc/self/ns/pid); for pid in ", paste(pids, collapse = " "),
    "; do if [ -r /proc/$pid/status ] && ! grep -q ^State:.Z /proc/$pid/status;",
    " then echo alive; else echo dead; fi; done")
  res <- tryCatch(
    system2("ssh", c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5", machine, shQuote(cmd)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL)
  if (is.null(res) || length(res) != length(pids) + 1L) return(NULL)
  remoteNs <- sub("^ns:", "", res[1L])
  if (!nzchar(remoteNs)) remoteNs <- NA_character_
  alive <- res[-1L] == "alive"
  if (!is.null(pid_ns))
    alive[!vapply(pid_ns, .sameNs, logical(1), ns = remoteNs, USE.NAMES = FALSE)] <- NA
  alive
}

## The RUNNING rows of `q` whose process is known dead. `machines = "local"`: only rows of
## this machine (cheap: for a worker holding the queue lock); `"all"`: one ssh per other
## machine too. Unknown is not dead: a row without machine or pid, on a machine that
## cannot be reached, or from another PID namespace than the check runs in (a Slurm job's
## own; a row written before pid_ns existed counts as another), is not returned. A tmux
## pane titled "<machine>-<pid>" is proof of life that saves the check.
.deadRunningRows <- function(q, machines = c("all", "local")) {
  machines <- match.arg(machines)
  status <- as.character(q$status)
  mach   <- if (is.null(q$machine_name)) rep(NA_character_, NROW(q)) else as.character(q$machine_name)
  pid    <- if (is.null(q$process_id)) rep(NA_integer_, NROW(q)) else suppressWarnings(as.integer(q$process_id))
  ns     <- if (is.null(q$pid_ns)) rep(NA_character_, NROW(q)) else as.character(q$pid_ns)
  idx <- which(status == txtRunning & !is.na(mach) & !is.na(pid))
  local <- Sys.info()[["nodename"]]
  if (machines == "local") idx <- idx[mach[idx] == local]
  if (!length(idx)) return(integer())
  titles <- tryCatch(.tmux_all_pane_titles(), error = function(e) character(0))
  dead <- integer()
  for (m in unique(mach[idx])) {
    k <- idx[mach[idx] == m]
    inPane <- vapply(pid[k], function(p) any(grepl(paste0(m, "-", p), titles, fixed = TRUE)),
                     logical(1))
    k <- k[!inPane]
    if (!length(k)) next
    alive <- if (m == local) {
      here <- vapply(ns[k], .sameNs, logical(1), ns = .pidNs(), USE.NAMES = FALSE)
      a <- rep(NA, length(k))
      a[here] <- vapply(pid[k][here], .pidIsAlive, logical(1), USE.NAMES = FALSE)
      a
    } else .ssh_pids_alive(m, pid[k], ns[k])
    if (is.null(alive)) next
    dead <- c(dead, k[!is.na(alive) & !alive])
  }
  sort(dead)
}

## Put the dead workers' rows `idx` of `q` back in the queue (see .reclaimUpdates()).
.reclaimRows <- function(q, idx) {
  for (i in idx)
    q <- .applyUpdates(q, i, .reclaimUpdates(q$machine_name[i], q$process_id[i]))
  if (length(idx))
    message("Reclaimed ", length(idx), " RUNNING row(s) whose worker is dead: now ",
            txtInterrupted, " (row ", paste(idx, collapse = ", "), ")")
  q
}

## A launch on an existing queue means "run what is left": HOLD and ERROR rows go back to
## PENDING. Other statuses are not the package's to change.
.releaseAtLaunch <- function(q) {
  status <- as.character(q$status)
  held <- which(status %in% c(txtHold, txtError))
  if (length(held)) {
    q$status[held] <- txtPending
    message("Launch: ", sum(status[held] == txtHold), " ", txtHold, " and ",
            sum(status[held] == txtError), " ", txtError, " row(s) set back to ", txtPending)
  }
  q
}

## Everything a launcher does to the queue's record before its workers start.
.queueAtLaunch <- function(q) {
  q <- .releaseAtLaunch(q)
  dead <- .deadRunningRows(q, "all")
  if (length(dead)) q <- .reclaimRows(q, dead)
  q
}
