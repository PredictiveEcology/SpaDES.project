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
#'     in, are left alone. A row claimed in a Slurm job is judged by its job instead:
#'     `claimed_by` ends in `/slurm:<cluster>:<job id>.<restart count>`, and the claim is
#'     dead when a later attempt of the same job (a requeue, after a node failure) is the
#'     one checking, or when Slurm says the job is no longer running: `squeue` lists it in
#'     another state (`PENDING` after a requeue, say), or leaves it out of a reply that also
#'     lists a job certainly in the queue, or `sacct` gives it a final state. `squeue` is
#'     asked with a 10 s limit and never while the queue's lock is held (a file worker asks
#'     first, then reclaims under the lock only a row still `RUNNING` under the same claim).
#'     A `squeue` that cannot answer leaves the row alone. After a node failure Slurm
#'     requeues the job and counts the old attempt as dead even if it still runs on a node
#'     cut off from the controller; its row is run again;
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

## The `pid_ns` of rows `i` of queue `q`; NA for a queue written before the column existed.
.rowNs <- function(q, i = seq_len(NROW(q)))
  if (is.null(q$pid_ns)) rep(NA_character_, length(i)) else as.character(q$pid_ns[i])

## Is each local `pid` alive? Only a pid recorded in this process's PID namespace (`pid_ns`)
## can be looked up in its /proc; any other is NA: unknown, not dead.
.localPidsAlive <- function(pid, pid_ns) {
  here <- vapply(pid_ns, .sameNs, logical(1), ns = .pidNs(), USE.NAMES = FALSE)
  alive <- rep(NA, length(pid))
  alive[here] <- vapply(pid[here], .pidIsAlive, logical(1), USE.NAMES = FALSE)
  alive
}

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

## The Slurm job attempt this process runs in, "slurm:<cluster>:<job id>.<restart count>";
## "" outside Slurm. A requeued job keeps its id and counts up SLURM_RESTART_COUNT (unset
## in the first attempt), so the token names one attempt of one job.
.slurmToken <- function() {
  job <- Sys.getenv("SLURM_JOB_ID")
  if (!nzchar(job)) return("")
  restart <- Sys.getenv("SLURM_RESTART_COUNT")
  paste0("slurm:", Sys.getenv("SLURM_CLUSTER_NAME"), ":", job, ".",
         if (nzchar(restart)) restart else "0")
}

## The `claimed_by` of a worker that is not a tmux pane: "<machine>-<pid>", and in a Slurm job
## "/<.slurmToken()>" after it. Slurm gives every job its own PID namespace, so on one node two
## jobs can have the same machine and PID; the job and its attempt tell them apart, and are what
## another process can check (.slurmClaimsAlive()).
.workerId <- function() {
  id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())
  token <- .slurmToken()
  if (nzchar(token)) paste0(id, "/", token) else id
}

## The Slurm job attempt in each `claimed_by` (see .workerId()): cluster, job id and restart
## count, NA where the row was not claimed in a Slurm job.
.slurmClaims <- function(claimedBy) {
  rx <- "^.*/slurm:([^:]*):([0-9]+)\\.([0-9]+)$"
  claimedBy <- as.character(claimedBy)
  hit <- !is.na(claimedBy) & grepl(rx, claimedBy)
  part <- function(n) ifelse(hit, sub(rx, n, claimedBy), NA_character_)
  data.frame(cluster = part("\\1"), job = part("\\2"),
             restart = suppressWarnings(as.integer(part("\\3"))), stringsAsFactors = FALSE)
}

## Run a Slurm command with a time limit. NULL when it is not installed, fails, or times out
## (a hung slurmctld): no answer. Otherwise its output lines (stderr included).
.slurmRun <- function(cmd, args, timeout = 10) {
  if (!nzchar(Sys.which(cmd))) return(NULL)
  out <- tryCatch(suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE, timeout = timeout)),
                  error = function(e) NULL)
  status <- attr(out, "status")
  if (is.null(out) || (!is.null(status) && status != 0L)) return(NULL)
  out
}

## `-M <cluster>` only for a cluster other than the one this process runs in: inside a job,
## squeue already talks to its own cluster, and -M needs slurmdbd, which not every site runs.
.slurmClusterArg <- function(cluster) {
  if (!nzchar(cluster) || identical(cluster, Sys.getenv("SLURM_CLUSTER_NAME"))) character(0)
  else .sbatch_cluster_args(cluster)
}

## Final Slurm states: a job in one of these has ended. sacct writes "CANCELLED by <uid>".
.slurmEndedStates <- c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "OUT_OF_MEMORY",
                       "NODE_FAIL", "PREEMPTED", "BOOT_FAIL", "DEADLINE", "REVOKED")

## The state of each of `ids` (job ids, character) on `cluster`, aligned with `ids`: what
## squeue says for a job it lists; "GONE" for a job it does not list, but only when the same
## reply lists a job that is certainly in the queue (this process's own job, asked along with
## the others, or any of `ids`), so an empty or partial reply from a restarting slurmctld, or a
## site that hides jobs, is not read as "every job has ended"; else the job's last state in
## sacct, if it is a final one. NA: unknown. Never asked without ids (squeue would list every
## job on the cluster); one squeue, and one sacct only when squeue gave no usable answer.
.slurmJobStates <- function(ids, cluster = "") {
  ids <- as.character(ids)
  states <- rep(NA_character_, length(ids))
  if (!length(ids)) return(states)
  me <- Sys.getenv("SLURM_JOB_ID")
  sentinel <- if (nzchar(me) && identical(cluster, Sys.getenv("SLURM_CLUSTER_NAME"))) me
  ask <- unique(c(ids, sentinel))
  args <- c("-h", "-j", paste(ask, collapse = ","), "-o", "%A|%T")
  out <- .slurmRun("squeue", c(.slurmClusterArg(cluster), args))
  ## outside a job (a login node), a site without slurmdbd refuses -M, and its one cluster
  ## is the default; inside a job without -M squeue would answer for its own cluster instead
  if (is.null(out) && length(.slurmClusterArg(cluster)) && !nzchar(Sys.getenv("SLURM_CLUSTER_NAME")))
    out <- .slurmRun("squeue", args)
  ## with -M, squeue starts with a "CLUSTER: <name>" line; only "<id>|<state>" lines count
  parts <- strsplit(grep("^[0-9]+\\|", out, value = TRUE), "|", fixed = TRUE)
  listed <- stats::setNames(vapply(parts, `[`, "", 2L), vapply(parts, `[`, "", 1L))
  if (length(listed) && (is.null(sentinel) || sentinel %in% names(listed))) {
    states <- unname(listed[ids])
    states[is.na(states)] <- "GONE"
    return(states)
  }
  ## no reply we can trust: only a final state in the accounting record says a job has ended
  acct <- .slurmRun("sacct", c(.slurmClusterArg(cluster), "-X", "-n", "-P",
                               "-j", paste(ids, collapse = ","), "-o", "JobIDRaw,State"))
  parts <- strsplit(grep("^[0-9]+\\|", acct, value = TRUE), "|", fixed = TRUE)
  last <- stats::setNames(sub(" .*", "", vapply(parts, `[`, "", 2L)), vapply(parts, `[`, "", 1L))
  last <- last[!duplicated(names(last), fromLast = TRUE)]     # the job's latest attempt
  st <- unname(last[ids])
  states[!is.na(st) & st %in% .slurmEndedStates] <- st[!is.na(st) & st %in% .slurmEndedStates]
  states
}

## Job states in which the attempt that started the job may still be running. Any other state
## (PENDING after a requeue, REQUEUED, a final state, GONE) means that attempt is over.
.slurmLiveStates <- c("RUNNING", "COMPLETING", "CONFIGURING", "SUSPENDED", "STOPPED",
                      "SIGNALING", "RESIZING", "STAGE_OUT")

## Is the Slurm job attempt that claimed each row alive? `claims`: .slurmClaims() of the rows.
## An earlier attempt of the job this process runs in is over: it is the job being requeued
## (no Slurm call). With `ask`, any other job is asked of squeue (.slurmJobStates()), once per
## cluster. NA (unknown) otherwise, when Slurm cannot answer, and for this attempt's own rows.
##
## After a NODE_FAIL the old attempt can still be running on a node cut off from the
## controller. Slurm has already given up on it (and requeued the job), so it is dead here too:
## its row is run again, and if that process ever comes back it ends a row someone else may be
## running. That is the price of not losing the row; Slurm kills such a process when the node
## rejoins.
.slurmClaimsAlive <- function(claims, ask = FALSE) {
  alive <- rep(NA, NROW(claims))
  me <- .slurmClaims(paste0("/", .slurmToken()))
  same <- !is.na(me$job) & claims$cluster == me$cluster & claims$job == me$job
  alive[same & claims$restart < me$restart] <- FALSE
  if (!ask) return(alive)
  others <- which(!same)
  for (cl in unique(claims$cluster[others])) {
    k <- others[claims$cluster[others] == cl]
    states <- .slurmJobStates(claims$job[k], cl)
    known <- !is.na(states)
    alive[k[known]] <- states[known] %in% .slurmLiveStates
  }
  alive
}

## The RUNNING rows of `q` claimed in a Slurm job (see .workerId()) that are known dead; see
## .slurmClaimsAlive() for `ask`.
.deadSlurmRows <- function(q, ask = FALSE) {
  claims <- .slurmClaims(if (is.null(q$claimed_by)) rep(NA_character_, NROW(q)) else q$claimed_by)
  k <- which(as.character(q$status) == txtRunning & !is.na(claims$job))
  if (!length(k)) return(integer())
  alive <- .slurmClaimsAlive(claims[k, , drop = FALSE], ask = ask)
  k[!is.na(alive) & !alive]
}

## File backend: put back the RUNNING rows of dead Slurm jobs, asking Slurm without holding the
## queue lock (squeue can take seconds, or hang with slurmctld, and every worker waits on that
## lock). The queue is read, Slurm asked, and then, under the lock, a row is reclaimed only if
## it is still RUNNING under the same claim. Errors leave the queue as it was.
.reclaimDeadSlurmRows <- function(queue_path) {
  q <- tryCatch(.queue_read(queue_path), error = function(e) NULL)
  if (is.null(q)) return(invisible(integer()))
  dead <- .deadSlurmRows(q, ask = TRUE)
  if (!length(dead)) return(invisible(integer()))
  claim <- as.character(q$claimed_by[dead])
  done <- integer()
  try(.queue_modify(queue_path, function(q2) {
    same <- vapply(seq_along(dead), function(j) dead[j] <= NROW(q2) &&
                     identical(as.character(q2$status[dead[j]]), txtRunning) &&
                     identical(as.character(q2$claimed_by[dead[j]]), claim[j]), logical(1))
    done <<- dead[same]
    .reclaimRows(q2, done)
  }), silent = TRUE)
  invisible(done)
}

## The RUNNING rows of `q` whose process is known dead. A row claimed in a Slurm job (see
## .workerId()) is judged by its job, not its PID, which means nothing outside that job's PID
## namespace: `askSlurm = TRUE` asks squeue (once per cluster; callers holding the file queue's
## lock pass FALSE and use .reclaimDeadSlurmRows() before taking it); either way, an earlier
## attempt of this process's own job is dead. Other rows by their PID: `machines = "local"`:
## only rows of this machine (cheap: for a worker holding the queue lock); `"all"`: one ssh per
## other machine too. Unknown is not dead: a row without machine or pid, on a machine that
## cannot be reached, or from another PID namespace than the check runs in (a row written
## before pid_ns existed counts as another), is not returned. A tmux pane titled
## "<machine>-<pid>" is proof of life that saves the check.
.deadRunningRows <- function(q, machines = c("all", "local"), askSlurm = FALSE) {
  machines <- match.arg(machines)
  status <- as.character(q$status)
  mach   <- if (is.null(q$machine_name)) rep(NA_character_, NROW(q)) else as.character(q$machine_name)
  pid    <- if (is.null(q$process_id)) rep(NA_integer_, NROW(q)) else suppressWarnings(as.integer(q$process_id))
  ns     <- .rowNs(q)
  inSlurm <- status == txtRunning & !is.na(.slurmClaims(
    if (is.null(q$claimed_by)) rep(NA_character_, NROW(q)) else q$claimed_by)$job)
  dead <- .deadSlurmRows(q, ask = askSlurm)
  idx <- which(status == txtRunning & !inSlurm & !is.na(mach) & !is.na(pid))
  local <- Sys.info()[["nodename"]]
  if (machines == "local") idx <- idx[mach[idx] == local]
  if (!length(idx)) return(sort(dead))
  titles <- tryCatch(.tmux_all_pane_titles(), error = function(e) character(0))
  for (m in unique(mach[idx])) {
    k <- idx[mach[idx] == m]
    inPane <- vapply(pid[k], function(p) any(grepl(paste0(m, "-", p), titles, fixed = TRUE)),
                     logical(1))
    k <- k[!inPane]
    if (!length(k)) next
    alive <- if (m == local) .localPidsAlive(pid[k], ns[k]) else .ssh_pids_alive(m, pid[k], ns[k])
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
