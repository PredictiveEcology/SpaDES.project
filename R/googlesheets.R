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

# Reclaim RUNNING rows whose R process is no longer alive on any machine.
#
# Liveness decision per row:
#   1. If "<machine>-<pid>" appears in ANY tmux pane title across ALL tmux
#      servers on this machine -> the worker pane is alive, skip.
#   2. Otherwise fall through to the /proc check:
#        - local  machine  : file.exists("/proc/<pid>")
#        - remote machine  : one SSH connection, batched /proc check for all pids
#      SSH unreachable -> leave alone (unreachable is not proof of death).
#      SSH reachable + PID dead -> reclaim.
#
# Pane titles are a POSITIVE signal only; their absence falls through to SSH.
# This prevents cross-server false reclaims and handles old-style panes
# (no node-pid in title) safely via the SSH backstop.
.gs_reclaim_dead_jobs <- function(ss_id, sheet = "Status") {
  q <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0L) return(invisible(NULL))

  running_idx <- which(
    q$status == "RUNNING" &
    !is.na(q$machine_name) &
    !is.na(q$process_id)
  )
  if (length(running_idx) == 0L) return(invisible(NULL))

  col_pos     <- setNames(seq_along(names(q)), names(q))
  now         <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  local_node  <- Sys.info()[["nodename"]]
  pane_titles <- .tmux_all_pane_titles()      # character(0) if unavailable
  machines    <- unique(q$machine_name[running_idx])

  .reclaim <- function(idx, pid, machine, reason) {
    sheet_row <- idx + 1L
    try(.gs_write_cells(ss_id, sheet_row,
                        updates       = list(status         = "INTERRUPTED",
                                             claimed_by     = NA_character_,
                                             interrupted_at = now),
                        col_positions = col_pos,
                        sheet         = sheet), silent = TRUE)
    message("Reclaimed stale RUNNING job row ", idx,
            " (", reason, ": PID ", pid, " on ", machine, ")")
  }

  for (machine in machines) {
    m_idx <- running_idx[q$machine_name[running_idx] == machine]
    pids  <- suppressWarnings(as.integer(q$process_id[m_idx]))
    ok    <- !is.na(pids)
    m_idx <- m_idx[ok]
    pids  <- pids[ok]
    if (length(pids) == 0L) next

    # --- pane-title positive check ---
    pane_alive <- if (length(pane_titles)) {
      vapply(pids, function(pid)
        any(grepl(paste0(machine, "-", pid), pane_titles, fixed = TRUE)),
        logical(1L))
    } else {
      rep(FALSE, length(pids))
    }

    # Rows with a confirmed pane: nothing to do.
    need_ssh_i <- which(!pane_alive)
    if (!length(need_ssh_i)) next

    ssh_m_idx <- m_idx[need_ssh_i]
    ssh_pids  <- pids[need_ssh_i]

    # --- /proc liveness check (local or SSH) ---
    if (machine == local_node) {
      alive <- file.exists(paste0("/proc/", ssh_pids))
    } else {
      pid_str   <- paste(ssh_pids, collapse = " ")
      check_cmd <- paste0(
        "for pid in ", pid_str,
        "; do [ -d /proc/$pid ] && echo alive || echo dead; done"
      )
      result <- tryCatch(
        system2("ssh",
                c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
                  machine, shQuote(check_cmd)),
                stdout = TRUE, stderr = FALSE),
        error = function(e) NULL
      )
      if (is.null(result) || length(result) != length(ssh_pids)) {
        # SSH unreachable -- unreachable is not proof of death.  Leave alone.
        next
      }
      alive <- result == "alive"
    }

    for (j in seq_along(ssh_m_idx)) {
      if (!alive[j])
        .reclaim(ssh_m_idx[j], ssh_pids[j], machine, "PID dead")
    }
  }
  invisible(NULL)
}

# Mirror a row-level update from the GS-backed queue to the local RDS file.
# No-op if queue_path is NULL or missing; safe under concurrent workers via
# filelock. Updates are matched against local column names (which are the
# .dotted form used by tmuxPrepareQueueFromDF / setupProject), not the GS
# dotXxx form.
.mirror_local_queue <- function(queue_path, row_i, updates) {
  trace <- isTRUE(getOption("spades.mirror.trace", FALSE)) ||
           nzchar(Sys.getenv("SPADES_MIRROR_TRACE"))
  if (is.null(queue_path) || !nzchar(queue_path) || !file.exists(queue_path)) {
    if (trace) message(sprintf("[mirror] SKIP row=%s queue_path=%s (missing/empty)",
                               row_i, as.character(queue_path)))
    return(invisible(NULL))
  }
  if (!requireNamespace("filelock", quietly = TRUE)) return(invisible(NULL))
  LOCKF <- paste0(queue_path, ".lock")
  lck <- try(filelock::lock(LOCKF, timeout = 10000L), silent = TRUE)
  if (inherits(lck, "try-error") || is.null(lck)) {
    if (trace) message(sprintf("[mirror] LOCK_FAIL row=%s", row_i))
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
      val <- updates[[nm]]
      cur <- q[[nm]]
      val2 <- tryCatch(
        if (is.integer(cur)) suppressWarnings(as.integer(val))
        else if (is.numeric(cur)) suppressWarnings(as.numeric(val))
        else as.character(val),
        error = function(e) val
      )
      q[[nm]][row_i] <- val2
      applied <- c(applied, sprintf("%s=%s", nm, as.character(val2)))
    } else {
      skipped <- c(skipped, nm)
    }
  }
  try(saveRDS(q, queue_path), silent = TRUE)
  if (trace) {
    msg <- sprintf("[mirror] WROTE row=%s [%s]", row_i, paste(applied, collapse = ", "))
    if (length(skipped))
      msg <- paste0(msg, sprintf("  SKIPPED_COLS=[%s]", paste(skipped, collapse = ",")))
    message(msg)
  }
  invisible(NULL)
}

# Demote any GS rows whose RUNNING claim is held by one of `killed_pids`,
# resetting status -> PENDING and clearing the run's metadata.  Used by
# experimentFutureList(kill = TRUE) to mirror the local-queue demotion to GS.
.gs_demote_after_kill <- function(ss_id, killed_pids, sheet = "Status") {
  if (!nzchar(as.character(ss_id))) return(invisible(0L))
  q <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0L) return(invisible(0L))
  pids <- suppressWarnings(as.integer(q$process_id))
  to_demote <- which(q$status == "RUNNING" & pids %in% as.integer(killed_pids))
  if (length(to_demote) == 0L) return(invisible(0L))
  col_pos <- setNames(seq_along(names(q)), names(q))
  # Demote to PENDING and wipe ALL run-specific metadata (matches the
  # local-queue demotion in tmuxRefreshQueueStatus, which clears every
  # meta column except `status`).
  for (i in to_demote) {
    try(.gs_write_cells(ss_id, sheet_row = i + 1L,
      updates = list(
        status             = "PENDING",
        claimed_by         = NA_character_,
        started_at         = NA_character_,
        finished_at        = NA_character_,
        DEoptimElapsedTime = NA_character_,
        machine_name       = NA_character_,
        process_id         = NA_character_,
        heartbeat_at       = NA_character_,
        heartbeat_iter     = NA_character_,
        iterationsTotal    = NA_character_,
        interrupted_at     = NA_character_
      ),
      col_positions = col_pos,
      sheet         = sheet,
      current_row   = as.list(q[i, ])
    ), silent = TRUE)
  }
  invisible(length(to_demote))
}

# Attempt to claim the next PENDING/INTERRUPTED job from the sheet.
# Uses optimistic concurrency: write claim, wait, re-read to verify.
# Random pending-row selection reduces collisions when many workers race.
# Retries on lost race; returns
#   * list(row_index, sheet_row, col_positions, data)            -- success
#   * NULL                                                       -- queue empty
#   * structure(list(), class = "gs_claim_lost")                 -- exhausted retries
# `queue_path`: when supplied, GS state is mirrored back to the local RDS
# so queueRead(local) and experimentFutureList() see the live state.
.gs_claim_next_job <- function(ss_id, worker_id, sheet = "Status",
                                queue_path = NULL, max_attempts = 20L) {
  for (attempt in seq_len(max_attempts)) {
    # Reclaim any RUNNING rows whose process died on this machine before scanning.
    .gs_reclaim_dead_jobs(ss_id, sheet)

    q <- .gs_read_queue(ss_id, sheet)
    if (nrow(q) == 0L) return(NULL)

    col_pos     <- setNames(seq_along(names(q)), names(q))
    pending_idx <- which(q$status %in% c("PENDING", "INTERRUPTED"))
    if (length(pending_idx) == 0L) return(NULL)

    # Always claim the topmost pending row.  If two workers collide on the
    # same row, the loser's retry sees that row already RUNNING and falls
    # through to the next pending row.
    row_i     <- pending_idx[1L]
    sheet_row <- row_i + 1L          # +1 for the header row
    now       <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Claim updates: set the new run's owner / start time, and ALSO scrub
    # any stale completion/interrupt fields left over from a previous run
    # of the same row.  Without these clears, finished_at / DEoptimElapsedTime
    # / heartbeat_* / interrupted_at would still carry the previous run's
    # values while the row is RUNNING, which is misleading.
    claim_updates <- list(
      status             = "RUNNING",
      claimed_by         = worker_id,
      started_at         = now,
      finished_at        = NA_character_,
      DEoptimElapsedTime = NA_character_,
      machine_name       = Sys.info()[["nodename"]],
      process_id         = as.character(Sys.getpid()),
      heartbeat_at       = NA_character_,
      heartbeat_iter     = NA_character_,
      iterationsTotal    = NA_character_,
      interrupted_at     = NA_character_
    )
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
      .mirror_local_queue(queue_path, row_i, list(
        status             = "RUNNING",
        claimed_by         = worker_id,
        started_at         = now,
        finished_at        = NA,
        DEoptimElapsedTime = NA,
        machine_name       = Sys.info()[["nodename"]],
        process_id         = Sys.getpid(),
        heartbeat_at       = NA,
        heartbeat_iter     = NA,
        iterationsTotal    = NA,
        interrupted_at     = NA
      ))
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
  q <- readRDS(queue_path)
  
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

      activeRunningPath <- tmuxActiveRunningPath(activeRunningPath = activeRunningPath, basename(queue_path))
      tmuxRefreshQueueStatus(queue_path, runNameLabel = runNameLabel,
                                statusCalculate = statusCalculate,
                                activeRunningPath = activeRunningPath, ...)
      q <- try(readRDS(queue_path), silent = TRUE)
      if (inherits(q, "try-error")) { Sys.sleep(2); next }

      # ---- GS → local: merge GS state into local RDS ----
      gs_q <- try(.gs_read_queue(ss_id), silent = TRUE)
      if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L) {

        n_local <- nrow(q)
        n_gs    <- nrow(gs_q)

        for (j in seq_len(min(n_local, n_gs))) {
          gs_status    <- gs_q$status[j]
          local_status <- q$status[j]

          # A remote worker claimed this job in GS (RUNNING) but the local RDS
          # hasn't caught up yet (still PENDING).  Pull the full claim from GS
          # so the local→GS push doesn't stomp the worker's live RUNNING entry.
          # .gs_reclaim_dead_jobs() above already cleared any stale RUNNING, so
          # a GS RUNNING here means a confirmed-live (or recently-live) worker.
          if (!is.na(gs_status) &&
              gs_status    == txtRunning &&
              local_status == txtPending) {
            q$status[j]       <- gs_status
            for (mc in c("claimed_by", "started_at", "machine_name", "process_id")) {
              if (mc %in% names(gs_q) && mc %in% names(q))
                q[[mc]][j] <- gs_q[[mc]][j]
            }
            next
          }

          # For all other rows: trust GS only for intentional user edits
          # (e.g. resetting INTERRUPTED -> PENDING).  Never let a stale GS
          # value demote an authoritative local RUNNING or completed DONE.
          if (!is.na(gs_status) &&
              gs_status    != txtRunning &&
              local_status != txtRunning &&
              local_status != txtDone    &&
              !identical(gs_status, local_status)) {
            q$status[j] <- gs_status
            if (gs_status == txtPending) {
              q$claimed_by[j]  <- NA_character_
              q$started_at[j]  <- NA_character_
              q$finished_at[j] <- NA_character_
            }
          }
        }

        # Append rows the user added directly in the sheet
        if (n_gs > n_local) {
          new_gs <- gs_q[(n_local + 1L):n_gs, , drop = FALSE]
          # Coerce to match local column types where possible
          for (col in intersect(names(q), names(new_gs))) {
            tryCatch(
              new_gs[[col]] <- methods::as(new_gs[[col]], class(q[[col]])),
              error = function(e) NULL
            )
          }
          q <- rbind(q, new_gs[, names(q), drop = FALSE])
        }

        saveRDS(q, queue_path)
      }

      # ---- local → GS: push current status for display ----
      q_sync        <- as.data.frame(lapply(q, as.character))
      names(q_sync) <- gsub("^\\.", dotTxt, names(q_sync))
      try(
        googlesheets4::with_gs4_quiet(
          googlesheets4::range_write(
            ss = ss_id, data = q_sync, sheet = "Status", range = "A1", reformat = FALSE
          )), silent = TRUE)
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
