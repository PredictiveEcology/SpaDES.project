
#' Initialize a file-backed queue from a data.frame
#'
#' @description
#' Creates/overwrites a queue file (RDS) that mirrors `df` and adds status columns:
#' `status` (PENDING/RUNNING/DONE), `claimed_by`, `started_at`, `finished_at`.
#'
#' @param df data.frame with at least columns you’ll assign into workers (e.g., `.ELFind`, `.rep`).
#' @param queue_path character, path to the queue `.rds` file (absolute path recommended).
#' @return Invisibly returns `queue_path`.
#' @export
tmux_prepare_queue_from_df <- function(df, queue_path) {
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1)
  q <- cbind(
    df,
    status      = "PENDING",
    claimed_by  = NA_character_,
    started_at  = as.character(NA),
    finished_at = as.character(NA)
  )
  saveRDS(q, queue_path)
  invisible(queue_path)
}

#' Spawn tmux workers that consume a file-backed queue until done
#'
#' @description
#' Spawns `n_workers` panes that run a **worker loop**:
#' atomically claim the next `PENDING` row, set objects (column names → object names),
#' `source(global_path)`, mark `DONE`, repeat until no `PENDING` remain.
#'
#' Delays happen **inside pane R**:
#' - Pane 1: no delay.
#' - Pane i>1: waits `delay_before_source + (i-2)*stagger_by` inside R before the **first** claim,
#'   then continues without extra delays.
#'
#' @param queue_path character; absolute path to `queue.rds`.
#' @param global_path character; absolute path to `global.R` (or your script).
#' @param n_workers integer; number of panes to spawn.
#' @param start_cmd character; command to start R in the pane (default `"R"`).
#' @param delay_after_split numeric; tmux settle after splits (default `0.2`).
#' @param delay_after_layout numeric; tmux settle after layout (default `0.2`).
#' @param delay_between_R_start numeric; pause after starting R (default `0`).
#' @param delay_before_source numeric; pane‑internal initial sleep for panes 2..n (default `60`).
#' @param stagger_by numeric; extra offset per pane beyond pane 2 (default `0`).
#' @param set_mouse logical; enable tmux mouse via `set-option -g mouse on` (default `TRUE`).
#'
#' @return Invisibly returns the vector of worker pane IDs.
#' @export
tmux_spawn_workers_from_queue <- function(queue_path,
                                          global_path,
                                          n_workers = 4,
                                          start_cmd = "R",
                                          delay_after_split = 0.2,
                                          delay_after_layout = 0.2,
                                          delay_between_R_start = 0.0,
                                          delay_before_source = 60.0,
                                          stagger_by = 0.0,
                                          set_mouse = TRUE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required.", call. = FALSE)
  }
  # we will ask the panes to load filelock; keep master free
  # (pane R will error if filelock missing; install it server-side)
  target_win <- .tmux_current_window()

  pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  for (i in seq_len(n_workers)) {
    .tmux_run("split-window", "-v", "-t", target_win)
    if (delay_after_split > 0) Sys.sleep(delay_after_split)
  }
  .tmux_run("select-layout", "-t", target_win, "tiled")
  if (delay_after_layout > 0) Sys.sleep(delay_after_layout)

  if (set_mouse) .tmux_run("set-option", "-g", "mouse", "on")

  post <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  master  <- Sys.getenv("TMUX_PANE")
  workers <- setdiff(setdiff(post, pre), master)
  if (length(workers) < n_workers) {
    stop(sprintf("Expected %d new worker panes; got %d.", n_workers, length(workers)), call. = FALSE)
  }
  workers <- workers[seq_len(n_workers)]

  # Write a small worker loop file; panes will source it.
  worker_script <- .write_worker_loop(queue_path, global_path)

  # Start R in each worker and source loop (pane‑internal initial sleep for panes 2..n)
  for (i in seq_along(workers)) {
    .tmux_run("send-keys", "-t", workers[i], start_cmd, "C-m")
    if (delay_between_R_start > 0) Sys.sleep(delay_between_R_start)

    pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
    # We inject: Sys.sleep(pre_sleep); source(worker_script); so master returns immediately.
    code <- sprintf('Sys.sleep(%s); source(%s)', pre_sleep, deparse(worker_script))
    .tmux_run("send-keys", "-t", workers[i], code, "C-m")
  }

   invisible(workers)
}
