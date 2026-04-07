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

# Reclaim RUNNING rows whose R process is no longer alive on this machine.
# Uses /proc/<pid> (Linux) to test liveness.  Called before each claim attempt
# so rows stuck in RUNNING by a crashed worker become INTERRUPTED and are
# re-queued.  Reads the sheet once and writes only the dead rows.
.gs_reclaim_dead_jobs <- function(ss_id, sheet = "Status") {
  nodename <- Sys.info()[["nodename"]]
  q <- tryCatch(.gs_read_queue(ss_id, sheet), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0L) return(invisible(NULL))

  running_idx <- which(
    q$status == "RUNNING" &
    !is.na(q$machine_name) & q$machine_name == nodename &
    !is.na(q$process_id)
  )
  if (length(running_idx) == 0L) return(invisible(NULL))

  col_pos <- setNames(seq_along(names(q)), names(q))
  now     <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  for (idx in running_idx) {
    pid   <- suppressWarnings(as.integer(q$process_id[idx]))
    if (is.na(pid)) next
    alive <- file.exists(paste0("/proc/", pid))
    if (!alive) {
      sheet_row <- idx + 1L   # +1 for header row
      try(.gs_write_cells(ss_id, sheet_row,
                          updates       = list(status = "INTERRUPTED",
                                               finished_at = now),
                          col_positions = col_pos,
                          sheet         = sheet), silent = TRUE)
      message("Reclaimed stale RUNNING job row ", idx,
              " (PID ", pid, " not found on ", nodename, ")")
    }
  }
  invisible(NULL)
}

# Attempt to claim the next PENDING/INTERRUPTED job from the sheet.
# Uses optimistic concurrency: write claim, wait, re-read to verify.
# Returns list(row_index, sheet_row, col_positions, data) or NULL (empty / lost race).
.gs_claim_next_job <- function(ss_id, worker_id, sheet = "Status") {
  # Reclaim any RUNNING rows whose process died on this machine before scanning.
  .gs_reclaim_dead_jobs(ss_id, sheet)

  q <- .gs_read_queue(ss_id, sheet)
  if (nrow(q) == 0L) return(NULL)

  col_pos     <- setNames(seq_along(names(q)), names(q))
  pending_idx <- which(q$status %in% c("PENDING", "INTERRUPTED"))
  if (length(pending_idx) == 0L) return(NULL)

  row_i     <- pending_idx[1L]
  sheet_row <- row_i + 1L          # +1 for the header row
  now       <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  .gs_write_cells(ss_id, sheet_row,
    updates = list(
      status       = "RUNNING",
      claimed_by   = worker_id,
      started_at   = now,
      machine_name = Sys.info()[["nodename"]],
      process_id   = as.character(Sys.getpid())
    ),
    col_positions = col_pos,
    sheet         = sheet,
    current_row   = as.list(q[row_i, ])   # avoids an extra read in .gs_write_cells
  )

  # Re-read to verify we won the race (another worker may have claimed first)
  Sys.sleep(2)
  q2 <- .gs_read_queue(ss_id, sheet)
  if (!isTRUE(q2$claimed_by[row_i] == worker_id)) return(NULL)  # lost

  list(row_index    = row_i,
       sheet_row    = sheet_row,
       col_positions = col_pos,
       data         = q2[row_i, ])
}

#' Mirror local queue to Google Sheets
#' @param queue_path Path to the local tmux_queue.rds
#' @param ss_id The Google Sheet ID (from the URL)
#' @param sheet_name The name of the tab to write to
tmux_mirror_queue_to_sheets <- function(queue_path, ss_id, sheet_name = "Status") {
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

      runNameLabel      <- eval(runNameLabel)
      activeRunningPath <- activeRunningPathForTmux(activeRunningPath = activeRunningPath, basename(queue_path))
      tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel,
                                statusCalculate = statusCalculate,
                                activeRunningPath = activeRunningPath, ...)
      q <- try(readRDS(queue_path), silent = TRUE)
      if (inherits(q, "try-error")) { Sys.sleep(2); next }

      # ---- GS → local: merge user edits and new rows ----
      gs_q <- try(.gs_read_queue(ss_id), silent = TRUE)
      if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L) {

        n_local <- nrow(q)
        n_gs    <- nrow(gs_q)

        # For existing rows: trust GS status when user has changed it
        # (never override a worker-written RUNNING — that's live state)
        for (j in seq_len(min(n_local, n_gs))) {
          gs_status <- gs_q$status[j]
          if (!is.na(gs_status) && gs_status != "RUNNING" &&
              !identical(gs_status, q$status[j])) {
            q$status[j] <- gs_status
            if (gs_status == "PENDING") {
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
      names(q_sync) <- gsub("^\\.", "", names(q_sync))
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
