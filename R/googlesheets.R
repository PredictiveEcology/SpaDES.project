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
.sync_loop_internal <- function(queue_path, ss_id, email, cache_path, runNameLabel, 
                                statusCalculate = getOption("spades.statusCalculate"),
                                activeRunningPath = getOption("spades.activeRunningPath"),
                                interval = 300) {
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
      
      runNameLabel <- eval(runNameLabel)
      activeRunningPath <- activeRunningPathForTmux(activeRunningPath = activeRunningPath, basename(queue_path))
      tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel, 
                                statusCalculate = statusCalculate,
                                activeRunningPath = activeRunningPath)
      q <- try(readRDS(queue_path), silent = TRUE)
      if (inherits(q, "try-error")) { Sys.sleep(2); next }
  
      q_sync <- as.data.frame(lapply(q, as.character))
      names(q_sync) <- gsub("^\\.", "", names(q_sync))
      
      try(
        googlesheets4::with_gs4_quiet(
          googlesheets4::range_write(
            ss = ss_id, data = q_sync, sheet = "Status", range = "A1", reformat = FALSE
          )), silent = TRUE)
      
      # cli::cli_alert_success("Google Sheet updated at {Sys.time()}")
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
