#' Read a running queue without claiming from it
#'
#' Helpers for a job that looks at *another* queue (or its own) while workers
#' keep it current: which rows exist and in what state, wait for a row to leave
#' a state, or decide that a job should not run because a companion queue is
#' already on it. They never write to the queue.
#'
#' The queue's Google Sheet is its record. The local `.rds` can lag (a lost lock
#' skips the claim write, a resumed launch rewrites it), so `queueRows()` reads
#' the sheet's "Status" tab first and uses the `.rds` only when the sheet cannot
#' be read. The sheet is found by name, the queue file's name without `.rds`,
#' as `experimentTmux()` names it. A column written `.ELFind` in the queue is
#' `dotELFind` in the sheet; `key` is always given with the dot.
#'
#' @param queueFile Path to the queue `.rds`.
#' @param key Name of the queue column that identifies a row, written as in the
#'   queue (`".ELFind"`, not `"dotELFind"`).
#' @param id The value of `key` of the row of interest, as character.
#' @param status The state(s) to wait while the row has one of them.
#' @param every Seconds to sleep between reads.
#' @param fitQueue Path to the companion ("fit") queue `.rds`, or `NA` for none.
#'
#' @return `queueRows()`: a `data.frame` with character columns `id` and `status`,
#'   in claim order, or `NULL` when neither the sheet nor the file can be read.
#'   `queueStatus()`: the row's status, `NA_character_` if the row is absent or
#'   the queue unreadable. `queueWaitWhile()`: the status that ended the wait,
#'   invisibly. `queueSkipForFits()`: `NULL` (do not skip) or a string saying why
#'   the job should be skipped.
#' @seealso [queueRead()] for a full snapshot of a queue in a Drive folder.
#' @rdname queue-peek
#' @export
queueRows <- function(queueFile, key = ".ELFind") {
  q <- tryCatch({
    id <- googledrive::drive_find(sub("\\.rds$", "", basename(queueFile)), type = "spreadsheet", n_max = 1)$id
    .queueIdStatus(.gs_read_queue(id), key)
  }, error = function(e) NULL)
  if (is.null(q) && file.exists(queueFile))
    q <- tryCatch(.queueIdStatus(.queue_read(queueFile, tries = 1L), key), error = function(e) NULL)
  q
}

#' @rdname queue-peek
#' @export
queueStatus <- function(queueFile, id, key = ".ELFind") {
  q <- queueRows(queueFile, key)
  if (is.null(q)) NA_character_ else q$status[q$id == id][1L]
}

#' @details `queueWaitWhile()` sleeps while the row has one of the `status`es,
#'   printing the time of day and the wait each round: a fit job waits only while
#'   a companion queue is building its row right now (`RUNNING`), then reuses that
#'   work, and never waits for a row that has not started.
#' @rdname queue-peek
#' @export
queueWaitWhile <- function(queueFile, id, status = "RUNNING", every = 300, key = ".ELFind") {
  repeat {
    st <- queueStatus(queueFile, id, key)
    if (!st %in% status) break
    message(format(Sys.time(), "%H:%M"), " waiting for ", id, " (", st, ") in ", basename(queueFile))
    Sys.sleep(every)
  }
  invisible(st)
}

#' @details `queueSkipForFits()` is the rule for a job that prepares inputs for
#'   the rows of a fit queue running side by side: skip a row the fit queue has
#'   claimed (`RUNNING` or `DONE`), and a row its idle workers will claim next.
#'   The fit queue takes its topmost pending rows, so with `k` fit workers not
#'   running a job, its first `k` `PENDING` or `INTERRUPTED` rows are left to the
#'   fits. The workers are counted from the `worker_launch_<n>.sh` files in the
#'   fit queue's log folder (`logs/<queue file name>` beside the queue).
#' @rdname queue-peek
#' @export
queueSkipForFits <- function(fitQueue, id, key = ".ELFind") {
  if (length(fitQueue) != 1L || is.na(fitQueue) || is.null(fq <- queueRows(fitQueue, key)))
    return(NULL)
  nWorkers <- length(dir(file.path(dirname(fitQueue), "logs", basename(fitQueue)), "^worker_launch_[0-9]+\\.sh$"))
  idle <- max(0L, nWorkers - sum(fq$status %in% "RUNNING"))
  nextFit <- utils::head(fq$id[fq$status %in% c("PENDING", "INTERRUPTED")], idle)
  if (id %in% nextFit)
    paste0("one of the next ", idle, " rows the idle fit workers will claim")
  else if (fq$status[fq$id == id][1L] %in% c("RUNNING", "DONE"))
    "the fit queue has claimed it"
}

## id and status of a queue read from the sheet or the file, or NULL if it lacks the key or status column
.queueIdStatus <- function(q, key) {
  q <- revertDotNames(as.data.frame(q))
  if (all(c(key, "status") %in% names(q)))
    data.frame(id = as.character(q[[key]]), status = as.character(q$status))
}
