#' Run an experiment in tmux panes (single-shot or queue mode)
#'
#' @description
#' Spawns `n_workers` panes in the current tmux window, starts **plain R**,
#' and either:
#' - **single-shot**: assigns **all columns** of `df` row *i* to pane *i*, then `source(global_path)` once; or
#' - **queue mode** (`continue=TRUE`): auto-writes a file-backed queue and each pane
#'   **loops**: claim next `PENDING` row (atomic), assign **all columns**, `source(global_path)`,
#'   mark `DONE`, repeat until no `PENDING` remain.
#'
#' First pane: **no initial delay**. Panes 2..N sleep inside R (`Sys.sleep`) before the first run.
#'
#' Targeting uses tmux formats (`#{session_name}:#{window_index}`) and pane IDs via
#' `list-panes -F "#{pane_id}"`. Commands are injected via `send-keys … C-m`. See tmux manual. 
#' @param df data.frame; columns become objects in each worker pane.
#' @param global_path character; **absolute** path to the script to `source()` in each pane.
#' @param n_workers integer; number of panes to spawn (default `4`).
#' @param start_cmd character; command to start R (default `"R"`).
#' @param delay_after_split numeric; tmux settle after splits (default `0.2`).
#' @param delay_after_layout numeric; tmux settle after layout (default `0.2`).
#' @param delay_between_R_start numeric; pause after starting R (default `0.0`).
#' @param delay_before_source numeric; panes 2..N initial sleep inside R (default `60.0`).
#' @param stagger_by numeric; extra offset per pane beyond pane 2 (default `0.0`).
#' @param set_mouse logical; enable tmux mouse via `set-option -g mouse on` (default `TRUE`).
#' @param continue logical; `TRUE` for queue mode; `FALSE` for single-shot (default `FALSE`).
#' @param queue_path character; path for queue `.rds` when `continue=TRUE`; if `NULL`, defaults
#'   next to `global_path`.
#' @return Invisibly returns the vector of worker pane IDs.
#' @export
experimentTmux <- function(df,
                           global_path = "global.R",
                           n_workers = 4,
                           start_cmd = "R",
                           delay_after_split = 0.2,
                           delay_after_layout = 0.2,
                           delay_between_R_start = 0.0,
                           delay_before_source = 60.0,
                           stagger_by = 0.0,
                           set_mouse = TRUE,
                           continue = FALSE,
                           queue_path = NULL) {

  # deps and preconditions
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  if (!is.data.frame(df)) stop("'df' must be a data.frame.", call. = FALSE)
  if (!file.exists(global_path)) warning("global_path not found: ", global_path)
  if (Sys.getenv("TMUX") == "") stop("Not inside tmux.", call. = FALSE)
  if (n_workers < 1L) stop("'n_workers' must be >= 1.", call. = FALSE)

  # resolve current tmux window
  target_win <- .tmux_current_window()  # uses formats like #{session_name}:#{window_index} [3](https://processx.r-lib.org/)

  # list existing panes → create workers → tile
  pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  for (i in seq_len(n_workers)) {
    .tmux_run("split-window", "-v", "-t", target_win)                      # create panes [2](https://rstudio.r-universe.dev/articles/rstudioapi/terminal.html)
    if (delay_after_split > 0) Sys.sleep(delay_after_split)
  }
  .tmux_run("select-layout", "-t", target_win, "tiled")                    # tile evenly [4](https://commandmasters.com/commands/gnome-terminal-linux/)
  if (delay_after_layout > 0) Sys.sleep(delay_after_layout)
  if (set_mouse) .tmux_run("set-option", "-g", "mouse", "on")              # mouse on [5](https://superuser.com/questions/215483/how-can-i-open-a-new-terminal-window-from-a-terminal-in-linux)

  post <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  master  <- Sys.getenv("TMUX_PANE")
  workers <- setdiff(setdiff(post, pre), master)
  if (length(workers) < n_workers) {
    stop(sprintf("Expected %d new worker panes; got %d.", n_workers, length(workers)), call. = FALSE)
  }
  workers <- workers[seq_len(n_workers)]

  # start R in each pane
  for (pid in workers) {
    .tmux_run("send-keys", "-t", pid, start_cmd, "C-m")                    # inject "R" + Enter [4](https://commandmasters.com/commands/gnome-terminal-linux/)
    if (delay_between_R_start > 0) Sys.sleep(delay_between_R_start)
  }

  if (continue) {
    # ---- Queue mode: auto-write queue & spawn worker loops ----
    if (is.null(queue_path)) {
      queue_path <- file.path(dirname(normalizePath(global_path)), "tmux_queue.rds")
    }
    tmux_prepare_queue_from_df(df, queue_path)
    if (!requireNamespace("filelock", quietly = TRUE)) {
      warning("Workers require 'filelock' on host. Install with install.packages('filelock').")
    }
    worker_script <- .write_worker_loop(queue_path, global_path)           # generates loop w/ file locks [1](https://r-lib.r-universe.dev/processx/doc/readme)

    for (i in seq_along(workers)) {
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      code <- sprintf('Sys.sleep(%s); source(%s)', pre_sleep, deparse(worker_script))
      .tmux_run("send-keys", "-t", workers[i], code, "C-m")
    }
  } else {
    # ---- Single-shot: assign ALL columns row i → source once ----
    n_send <- min(n_workers, nrow(df))
    for (i in seq_len(n_send)) {
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      code <- .make_assignment_code_all_cols(df, i, global_path, pre_sleep = pre_sleep)
      .tmux_run("send-keys", "-t", workers[i], code, "C-m")
    }
  }

  invisible(workers)
}
