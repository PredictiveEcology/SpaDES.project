# ======================================================================
# Google Sheets distributed queue helpers
# ======================================================================

# Read the "Status" sheet as an all-character data.frame
.gs_read_queue <- function(ss_id, sheet = "Status") {
  reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
  q <- suppressMessages(
    googlesheets4::read_sheet(ss_id, sheet = sheet, col_types = "c")
  )
  as.data.frame(q, stringsAsFactors = FALSE)
}

# The sheet stores every queue value as text (.gs_push_queue() writes
# `lapply(q, as.character)`), so give it back the column types the queue has in R.
# `template` is a data.frame with those types -- the experiment `df` or the local
# queue file -- matched by column name, whether written `.col` or `dotcol`:
#   * character: kept as the text it is, so an ELF named "14.3" or "4.10" is never
#     read as the number 14.3 or 4.1;
#   * numeric, integer, logical: coerced to that class;
#   * list: each cell rebuilt from the `c(...)` / `list(...)` text as.character() wrote.
# A NULL template is an error. A column the template lacks stays text unless every cell is such a constructor.
# Meta columns (status, timestamps, ...) are left as the sheet has them.
.gs_restore_types <- function(gs, template) {
  ## Without a template every value would silently stay text ("2016" for an integer
  ## column), so a job could run with the wrong types. Refuse instead.
  if (is.null(template))
    stop(".gs_restore_types needs a template with the queue's column types; got NULL")
  wasDT <- data.table::is.data.table(gs)
  gs <- as.data.frame(gs, stringsAsFactors = FALSE)
  for (nm in names(gs)) {
    nmDot <- sub(paste0("^", dotTxt), ".", nm)
    if (nm %in% meta_cols || nmDot %in% meta_cols) next
    tn <- intersect(c(nm, nmDot), names(template))
    gs[[nm]] <- .gs_restore_column(gs[[nm]], if (length(tn)) template[[tn[1L]]])
  }
  if (wasDT) data.table::setDT(gs)
  gs
}

.gs_restore_column <- function(x, template = NULL) {
  if (!is.character(x) || is.character(template) || is.factor(template))
    return(x)
  if (is.list(template))
    return(lapply(x, .gs_constructor_value))
  if (is.numeric(template) || is.logical(template))
    return(suppressWarnings(methods::as(x, class(template)[1L])))
  if (is.null(template)) {
    vals <- lapply(x, .gs_constructor_value)
    if (all(vapply(vals, function(v) is.character(v) && length(v) == 1L, logical(1))))
      return(x)
    return(vals)
  }
  x
}

# Rebuild a cell that as.character() wrote for a list-column element -- `c("a", "b")`,
# `list(start = 1, end = 2)`, `1991:2020`, `character(0)`. Any other text is returned
# unchanged: it is not evaluated just because it would parse.
.gs_constructor_value <- function(txt) {
  if (is.na(txt)) return(txt)
  expr <- tryCatch(str2lang(txt), error = function(e) NULL)
  if (is.call(expr) && is.name(expr[[1L]]) &&
      as.character(expr[[1L]]) %in% c("c", "list", ":", "character", "numeric",
                                      "integer", "double", "logical"))
    return(tryCatch(eval(expr, baseenv()), error = function(e) txt))
  txt
}

# Write named scalar values into specific columns of one sheet row.
# Batches all updates into a single range_write call to avoid quota exhaustion.
# col_positions: named integer vector  col_name -> col_index (1-based)
# current_row:  optional named character vector of the row's current values
#               (avoids an extra read when the caller already has the data)
.gs_write_cells <- function(ss_id, sheet_row, updates, col_positions,
                             sheet = "Status", current_row = NULL) {
  valid_names <- names(updates)[!is.na(col_positions[names(updates)])]
  if (length(valid_names) == 0L) return(invisible(NULL))

  update_cols <- col_positions[valid_names]
  min_col     <- min(update_cols)
  max_col     <- max(update_cols)
  ncols       <- max_col - min_col + 1L

  # Seed row_data from existing sheet values to protect non-updated cells
  row_data <- rep(NA_character_, ncols)
  if (!is.null(current_row)) {
    # Caller supplied current values: use them for cells in our range
    for (nm in names(current_row)) {
      ci <- col_positions[nm]
      if (!is.na(ci) && ci >= min_col && ci <= max_col)
        row_data[ci - min_col + 1L] <- as.character(current_row[[nm]])
    }
  } else {
    # Read only the bounding range in one call
    existing <- try(
      suppressMessages(
        googlesheets4::range_read(
          ss        = ss_id,
          sheet     = sheet,
          range     = googlesheets4::cell_limits(c(sheet_row, min_col),
                                                  c(sheet_row, max_col)),
          col_names = FALSE,
          col_types = "c"
        )
      ),
      silent = TRUE
    )
    if (!inherits(existing, "try-error") && nrow(existing) > 0L) {
      for (i in seq_len(min(ncol(existing), ncols)))
        row_data[i] <- as.character(existing[[i]])
    }
  }

  # Apply all updates
  for (nm in valid_names)
    row_data[col_positions[nm] - min_col + 1L] <- as.character(updates[[nm]])

  # Single write for the entire bounding range
  googlesheets4::range_write(
    ss        = ss_id,
    data      = as.data.frame(t(row_data), stringsAsFactors = FALSE),
    range     = googlesheets4::cell_limits(c(sheet_row, min_col),
                                            c(sheet_row, max_col)),
    sheet     = sheet,
    col_names = FALSE
  )
  invisible(NULL)
}

#' Is this pid a live process?
#'
#' Not `file.exists("/proc/<pid>")`: a zombie -- a process that has exited but whose
#' parent has not reaped it -- still has a `/proc` entry, so bare existence reports it
#' alive. A worker killed while its parent is something that does not wait for it (a tmux
#' server, say) can stay a zombie indefinitely, and its queue row would then never be
#' reclaimed: that study area is silently skipped for the rest of the run.
#'
#' The state is the field after the last `")"` in `/proc/<pid>/stat`, because the comm
#' field can itself contain spaces and parentheses.
#'
#' @param pid Integer or character process id.
#' @param procRoot Root of the proc filesystem; only overridden by tests, which cannot
#'   manufacture a zombie reliably (R reaps its own forked children).
#' @return `TRUE` if the process exists and is not a zombie.
#' @keywords internal
.pidIsAlive <- function(pid, procRoot = "/proc") {
  # no /proc (macOS, Windows): ask the OS instead of reading every pid as dead
  if (!dir.exists(procRoot)) return(unname(.pidsAlive(as.integer(pid))))
  f <- file.path(procRoot, pid, "stat")
  if (!file.exists(f)) return(FALSE)
  l <- tryCatch(readLines(f, warn = FALSE)[1L], error = function(e) NA_character_)
  if (is.na(l)) return(FALSE)
  !identical(strsplit(sub("^.*\\) ", "", l), " ")[[1L]][1L], "Z")
}

# Sheet backend: RUNNING rows whose worker is dead (.deadRunningRows(): local /proc, one
# ssh per other machine, pane titles as proof of life) go back in the queue as INTERRUPTED.
# The sheet has no lock, so the row is read again right before writing: the decision came
# from a read taken before the liveness checks, and since then another worker may have
# reclaimed the row and a third claimed it and started the job. Writing INTERRUPTED over
# that claim would put a running job back in the queue, and the next worker would run it
# too (#169). An unreadable sheet is not proof either, so the row is left alone then.
.gs_reclaim_dead_jobs <- function(ss_id, sheet = "Status") {
  q <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0L) return(invisible(NULL))
  dead <- .deadRunningRows(q, "all")
  if (!length(dead)) return(invisible(NULL))

  col_pos <- setNames(seq_along(names(q)), names(q))
  q_now <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  for (idx in dead) {
    machine <- q$machine_name[idx]
    pid     <- suppressWarnings(as.integer(q$process_id[idx]))
    unchanged <- !is.null(q_now) && NROW(q_now) >= idx &&
      identical(q_now$status[idx], txtRunning) &&
      identical(q_now$machine_name[idx], machine) &&
      identical(suppressWarnings(as.integer(q_now$process_id[idx])), pid)
    if (!unchanged) {
      message("Not reclaiming job row ", idx, ": it changed after PID ", pid,
              " on ", machine, " was found dead")
      next
    }
    try(.gs_write_cells(ss_id, idx + 1L, updates = .reclaimUpdates(machine, pid),
                        col_positions = col_pos, sheet = sheet), silent = TRUE)
    message("Reclaimed RUNNING job row ", idx, " (PID ", pid, " on ", machine, " is dead): now ",
            txtInterrupted)
  }
  invisible(NULL)
}

# Write the end of a job to the sheet and the local mirror; see .jobEndUpdates().
# The row's status is read just before writing, so a status the user set meanwhile
# is seen. An unreadable sheet is treated as still RUNNING.
.gs_record_job_end <- function(ss_id, sheet_row, row_i, col_pos, queue_path,
                               outcome, on_interrupt, lastErr = NULL) {
  current <- tryCatch(.gs_read_queue(ss_id)$status[row_i], error = function(e) NA_character_)
  upd <- .jobEndUpdates(outcome, on_interrupt, current, lastErr)
  try(.gs_write_cells(ss_id, sheet_row, updates = upd, col_positions = col_pos))
  try(.mirror_local_queue(queue_path, row_i, upd))
  invisible(upd)
}

# Mirror a row-level update from the GS-backed queue to the local RDS file.
# No-op if queue_path is NULL or missing; safe under concurrent workers via
# filelock. Updates are matched against local column names (which are the
# .dotted form used by tmuxPrepareQueueFromDF / setupProject), not the GS
# dotXxx form.
.mirror_local_queue <- function(queue_path, row_i, updates, lockTries = 3L) {
  trace <- isTRUE(getOption("spades.mirror.trace", FALSE)) ||
           nzchar(Sys.getenv("SPADES_MIRROR_TRACE"))
  if (is.null(queue_path) || !nzchar(queue_path) || !file.exists(queue_path)) {
    if (trace) message(sprintf("[mirror] SKIP row=%s queue_path=%s (missing/empty)",
                               row_i, as.character(queue_path)))
    return(invisible(NULL))
  }
  if (!requireNamespace("filelock", quietly = TRUE)) return(invisible(NULL))
  LOCKF <- paste0(queue_path, ".lock")
  ## A timed-out lock used to return silently, so the claim never reached the local rds.
  ## Try again, and say so if it still fails.
  for (attempt in seq_len(lockTries)) {
    lck <- try(filelock::lock(LOCKF, timeout = 10000L), silent = TRUE)
    if (!inherits(lck, "try-error") && !is.null(lck)) break
  }
  if (inherits(lck, "try-error") || is.null(lck)) {
    message(sprintf("[mirror] could not lock %s after %d tries; row %s was NOT updated in the local queue (%s)",
                    LOCKF, lockTries, row_i, paste(names(updates), collapse = ", ")))
    return(invisible(NULL))
  }
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- try(readRDS(queue_path), silent = TRUE)
  if (inherits(q, "try-error")) {
    if (trace) message(sprintf("[mirror] READ_FAIL row=%s", row_i))
    return(invisible(NULL))
  }
  applied <- character(0)
  skipped <- character(0)
  for (nm in names(updates)) {
    if (nm %in% names(q)) {
      val2 <- .coerceLike(updates[[nm]], q[[nm]])
      q[[nm]][row_i] <- val2
      applied <- c(applied, sprintf("%s=%s", nm, as.character(val2)))
    } else if (nm %in% meta_cols) {
      ## A meta column the queue predates -- `last_error`, for one. Add it rather than
      ## drop the update: an existing run should start recording failures immediately,
      ## not only after the queue is rebuilt. Data columns are still skipped, since
      ## inventing one of those would change what the run means.
      q[[nm]] <- NA_character_
      q[[nm]][row_i] <- as.character(updates[[nm]])
      applied <- c(applied, sprintf("%s=%s (column added)", nm, as.character(updates[[nm]])))
    } else {
      skipped <- c(skipped, nm)
    }
  }
  try(.queue_write(q, queue_path), silent = TRUE)
  if (trace) {
    msg <- sprintf("[mirror] WROTE row=%s [%s]", row_i, paste(applied, collapse = ", "))
    if (length(skipped))
      msg <- paste0(msg, sprintf("  SKIPPED_COLS=[%s]", paste(skipped, collapse = ",")))
    message(msg)
  }
  invisible(NULL)
}

# Sheet rows RUNNING under one of `killed_pids` (workers experimentFutureList(kill = TRUE)
# just killed) go back in the queue as INTERRUPTED, like every other dead worker's row.
.gs_demote_after_kill <- function(ss_id, killed_pids, sheet = "Status") {
  if (!nzchar(as.character(ss_id))) return(invisible(0L))
  q <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0L) return(invisible(0L))
  pids <- suppressWarnings(as.integer(q$process_id))
  to_demote <- which(q$status == txtRunning & pids %in% as.integer(killed_pids))
  if (length(to_demote) == 0L) return(invisible(0L))
  col_pos <- setNames(seq_along(names(q)), names(q))
  for (i in to_demote) {
    try(.gs_write_cells(ss_id, sheet_row = i + 1L,
      updates = .reclaimUpdates(q$machine_name[i], pids[i]),
      col_positions = col_pos,
      sheet         = sheet,
      current_row   = as.list(q[i, ])
    ), silent = TRUE)
  }
  invisible(length(to_demote))
}

# Attempt to claim the next PENDING/INTERRUPTED job from the sheet.
# Uses optimistic concurrency: write claim, wait, re-read to verify.
# Retries on lost race; returns
#   * list(row_index, sheet_row, col_positions, data)            -- success
#   * NULL                                                       -- queue empty
#   * structure(list(), class = "gs_claim_wait")                 -- rows left wait for `.after` groups
#   * structure(list(), class = "gs_claim_lost")                 -- exhausted retries
# `group`: NULL (any row) or the `.group` whose rows this worker claims; `afterGroupsEnded`:
# see .claimableRow().
# `queue_path`: when supplied, GS state is mirrored back to the local RDS
# so queueRead(local) and experimentFutureList() see the live state.
.gs_claim_next_job <- function(ss_id, worker_id, sheet = "Status",
                                queue_path = NULL, max_attempts = 20L, group = NULL,
                                afterGroupsEnded = FALSE) {
  for (attempt in seq_len(max_attempts)) {
    # Reclaim any RUNNING rows whose process died before scanning.
    .gs_reclaim_dead_jobs(ss_id, sheet)

    q <- .gs_read_queue(ss_id, sheet)
    if (nrow(q) == 0L) return(NULL)

    col_pos     <- setNames(seq_along(names(q)), names(q))
    # Always claim the topmost claimable row.  If two workers collide on the
    # same row, the loser's retry sees that row already RUNNING and falls
    # through to the next pending row.
    claim <- .claimableRow(q, group, afterGroupsEnded)
    if (is.na(claim$i))
      return(if (claim$wait) structure(list(), class = "gs_claim_wait") else NULL)
    row_i     <- claim$i
    sheet_row <- row_i + 1L          # +1 for the header row

    claim_updates <- .claimUpdates(worker_id)
    .gs_write_cells(ss_id, sheet_row,
      updates       = claim_updates,
      col_positions = col_pos,
      sheet         = sheet,
      current_row   = as.list(q[row_i, ])
    )

    # Re-read to verify we won the race (another worker may have claimed first)
    Sys.sleep(2)
    q2 <- .gs_read_queue(ss_id, sheet)
    if (isTRUE(q2$claimed_by[row_i] == worker_id)) {
      # Mirror claim back to the local RDS so non-GS readers see the truth.
      .mirror_local_queue(queue_path, row_i, claim_updates)
      return(list(row_index    = row_i,
                  sheet_row    = sheet_row,
                  col_positions = col_pos,
                  data         = q2[row_i, ]))
    }
    # Lost race -- back off with jitter, then retry.
    Sys.sleep(stats::runif(1, 0.5, 1.5))
  }
  # All attempts collided. Don't kill the worker; signal a transient miss.
  structure(list(), class = "gs_claim_lost")
}

#' Mirror local queue to Google Sheets
#' @param queue_path Path to the local tmux_queue.rds
#' @param ss_id The Google Sheet ID (from the URL)
#' @param sheet_name The name of the tab to write to
tmuxMirrorQueueToSheets <- function(queue_path, ss_id, sheet_name = "Status") {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' is required for mirroring.")
  }
  if (!file.exists(queue_path)) return(FALSE)
  
  # Read the current local state
  q <- .queue_read(queue_path)
  
  # Convert any POSIXct to character for cleaner Sheet display if preferred
  # googlesheets4 handles them natively, but character is safer for "read-only" views
  
  # Overwrite the sheet with current data
  # sheet_write() styles the header and freezes the first row automatically
  googledrive::with_drive_quiet(
    googlesheets4::sheet_write(q, ss = ss_id, sheet = sheet_name)
  )
  
  return(TRUE)
}


#' Sync queue to Google Sheets (Package Internal)
#' @keywords internal
.sync_loop_internal <- function(queue_path, ss_id, email = getOption("gargle_oauth_email"),
                                cache_path = getOption("gargle_oauth_cache"), runNameLabel,
                                statusCalculate = getOption("spades.statusCalculate"),
                                activeRunningPath = getOption("spades.activeRunningPath"),
                                interval = 300, ...) {
  # This code runs inside the tmux pane
  # library(googlesheets4)
  options(
    gargle_oauth_email = email,
    gargle_oauth_cache = cache_path,
    gargle_oauth_client_type = "web", # Critical for remote/tmux background flows
    cli.progress_show_after = 0 # Force bar to show immediately
  )
  # 2025 non-interactive auth using your package's cached secrets
  reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
  # googlesheets4::gs4_auth()
  message(
    "Updating Google Sheet located at: \n", 
    googledriveIDtoHumanURL(ss_id),
    # paste0("https://drive.google.com/file/d/", ss_id),
    "\nevery ", interval, " seconds")
  repeat {
    if (file.exists(queue_path)) {
      # The sheet is the record, the local file its mirror (see ?experimentStatus).
      # 1. The record: rows of dead workers go back in the queue.
      try(.gs_reclaim_dead_jobs(ss_id), silent = TRUE)
      # 2. The mirror: display columns from the jobs' output files.
      activeRunningPath <- tmuxActiveRunningPath(activeRunningPath = activeRunningPath, queue_path)
      tmuxRefreshQueueStatus(queue_path, runNameLabel = runNameLabel,
                                statusCalculate = statusCalculate,
                                activeRunningPath = activeRunningPath, ...)
      q <- try(.queue_read(queue_path), silent = TRUE)
      if (inherits(q, "try-error")) { Sys.sleep(2); next }
      gs_q <- try(.gs_read_queue(ss_id), silent = TRUE)
      if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L)
        .queue_write(.syncMirrorWithSheet(q, gs_q, ss_id), queue_path)
    }
    
    # --- CLI COUNTDOWN FIX ---
    # Use the qualified 'cli::pb_...' names to avoid 'object not found' errors
    pb <- cli::cli_progress_bar(
      format = "Next update in {cli::pb_eta_str} {cli::pb_bar} {cli::pb_percent}",
      total = interval,
      clear = TRUE
    )
    
    for (s in seq_len(interval)) {
      Sys.sleep(1)
      cli::cli_progress_update(set = s, id = pb)
    }
    cli::cli_progress_done(id = pb)
  }
}

# One sync cycle between the mirror `q` (the local file) and the record `gs_q` (the sheet,
# all text), by row position: the sheet's record columns (status, who claimed it, when it
# started and ended) overwrite the mirror's; rows added at the bottom of the sheet are
# appended; and the mirror's display columns (heartbeats, from the output files) are
# written to the sheet, one cell range per row that differs. Nothing else goes up: the
# whole mirror used to be pushed over the sheet from A1 every cycle, which overwrote any
# claim a worker wrote between the read and the push, and gave two writable copies of
# the status. Returns the mirror to write.
.syncMirrorWithSheet <- function(q, gs_q, ss_id, sheet = "Status") {
  n <- min(nrow(q), nrow(gs_q))
  for (mc in intersect(.recordCols, intersect(names(gs_q), names(q))))
    q[[mc]][seq_len(n)] <- .coerceLike(gs_q[[mc]][seq_len(n)], q[[mc]])
  if (nrow(gs_q) > n) {
    new_gs <- .gs_restore_types(gs_q[(n + 1L):nrow(gs_q), , drop = FALSE], q)
    q <- rbind(q, new_gs[, names(q), drop = FALSE])
  }
  col_pos <- setNames(seq_along(names(gs_q)), names(gs_q))
  show <- intersect(.displayCols, intersect(names(gs_q), names(q)))
  for (j in seq_len(n)) {
    mine <- lapply(q[j, show, drop = FALSE], as.character)
    theirs <- lapply(gs_q[j, show, drop = FALSE], as.character)
    differs <- !mapply(function(a, b) identical(a, b) || (is.na(a) && is.na(b)), mine, theirs)
    if (any(differs))
      try(.gs_write_cells(ss_id, j + 1L, updates = mine[differs], col_positions = col_pos,
                          sheet = sheet, current_row = as.list(gs_q[j, ])), silent = TRUE)
  }
  q
}

# Push a whole queue to the sheet, replacing its contents from A1. Dot-prefixed
# column names cannot survive a round trip through Sheets, so `.col` is written
# as `dotcol` (see revertDotNames()). Everything is written as character.
# `q` MUST be the queue data.frame: experimentTmux() once reached this point
# with `q` unassigned in the fresh-sheet branch, so `lapply(q, as.character)`
# deparsed base::q -- the quit function -- into the Status tab and every
# worker found an empty queue.
.gs_push_queue <- function(ss_id, q, sheet = "Status") {
  if (!is.data.frame(q))
    stop("The queue to push to the Google Sheet must be a data.frame, not ",
         class(q)[1], call. = FALSE)
  reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
  q_sync        <- as.data.frame(lapply(q, as.character), stringsAsFactors = FALSE)
  names(q_sync) <- gsub("^\\.", dotTxt, names(q_sync))
  googlesheets4::with_gs4_quiet(
    googlesheets4::range_write(ss = ss_id, data = q_sync, sheet = sheet,
                               range = "A1", reformat = FALSE))
  invisible(q_sync)
}

# ----------------------------------------------------------------------
# Queue file I/O. Many workers rewrite the queue .rds while others read it.
# ----------------------------------------------------------------------

# Write the queue so a reader sees the old file or the new one, never half of one:
# save to a temp file in the same directory, then rename over `queue_path`.
.queue_write <- function(q, queue_path) {
  tmp <- tempfile(pattern = paste0(basename(queue_path), "."), tmpdir = dirname(queue_path),
                  fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(q, tmp)
  if (!file.rename(tmp, queue_path))
    stop("Could not move the new queue into place at ", queue_path)
  invisible(queue_path)
}

# Change the queue file in place: `fun(q)` under the lock, written back only if it changed.
# Returns the queue as it now is. Do not call it while holding that lock.
.queue_modify <- function(queue_path, fun, timeout = 10000L) {
  LOCKF <- paste0(queue_path, ".lock")
  lck <- filelock::lock(LOCKF, timeout = timeout)
  if (is.null(lck)) stop("Could not take ", LOCKF)
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- readRDS(queue_path)
  q2 <- fun(q)
  if (!identical(q2, q)) .queue_write(q2, queue_path)
  q2
}

# Read the queue under the same `.lock` that writers take, retrying; stop if it still
# cannot be read. Never returns NULL. Do not call it while holding that lock.
.queue_read <- function(queue_path, tries = 5L, wait = 1, timeout = 10000L) {
  LOCKF <- paste0(queue_path, ".lock")
  err <- NULL
  for (attempt in seq_len(tries)) {
    lck <- try(filelock::lock(LOCKF, timeout = timeout), silent = TRUE)
    if (!inherits(lck, "try-error") && !is.null(lck)) {
      q <- tryCatch(readRDS(queue_path), error = function(e) {err <<- conditionMessage(e); NULL})
      try(filelock::unlock(lck), silent = TRUE)
      if (!is.null(q)) return(q)
    } else {
      err <- paste("could not take", LOCKF)
    }
    if (attempt < tries) Sys.sleep(wait)
  }
  stop("Could not read the queue ", queue_path, " after ", tries, " tries (", err, ")")
}
