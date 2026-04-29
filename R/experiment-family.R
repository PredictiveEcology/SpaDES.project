#' Experiment runners: one queue model, three spawn backends
#'
#' A SpaDES.project "experiment" is a `data.frame` (or `data.table`) where
#' each row is one job to run. Each runner pours the row's columns into the
#' worker's `.GlobalEnv` and sources `global.R` against them, marks the row
#' DONE in a shared queue, and moves on to the next PENDING row. The three
#' runners share that queue, the run-name / status calculation contracts,
#' and the resume-on-restart semantics; they differ only in *how* parallel
#' workers are spawned:
#'
#' \describe{
#'   \item{[experimentTmux()]}{One tmux pane per worker, optionally across
#'     ssh-reachable machines. Best for interactive use where you want to
#'     watch workers live (`tmux attach`). Workers can be torn down with
#'     [tmuxKillPanes()].}
#'   \item{[experimentFuture()]}{Background R processes via `callr::r_bg()`
#'     (local) or `future::cluster` (remote). No tmux involvement. Best
#'     for non-interactive driver scripts on a single workstation. Block
#'     with [awaitExperimentFuture()] or stop with [killExperimentFuture()].}
#'   \item{[experimentSBATCH()]}{One Slurm batch job per worker. Best for
#'     HPC clusters with `sbatch` / `squeue` / `scancel`. Block with
#'     [awaitExperimentSBATCH()] (polls `squeue`) or stop with
#'     [killExperimentSBATCH()] (graceful via stop files; `force = TRUE`
#'     issues `scancel`). Inspect generated job scripts with
#'     `dry_run = TRUE`.}
#' }
#'
#' All three runners accept the same core arguments:
#'
#' \describe{
#'   \item{`df`}{The parameter grid; one row = one job. Column names become
#'     R variables in the worker's `.GlobalEnv` before `global.R` is sourced.}
#'   \item{`global_path`}{Path to the R script each worker sources per job.
#'     Must be on a filesystem visible to all workers (matters for
#'     `experimentSBATCH()` and remote-host modes of the other two).}
#'   \item{`queue_path`}{Path to the local RDS queue file. Workers
#'     coordinate through file-based locks on this file; remove it (or
#'     point a fresh path) to start over, leave it to resume.}
#'   \item{`runNameLabel`}{Quoted expression evaluated against each row to
#'     derive a human-readable identifier (used in log messages, sentinel
#'     filenames, and `tmuxListPanes()` output). Default is the first two
#'     non-meta columns of the queue.}
#'   \item{`statusCalculate`}{Optional quoted expression that inspects the
#'     job's outputs and returns up-to-date status / heartbeat metadata.
#'     [statusCalculate_LandR] and [statusCalculate_FireSenseFit] are
#'     pre-built blocks for the most common SpaDES module outputs.}
#'   \item{`ss_id`}{Optional Google Sheets / Drive folder ID. When provided,
#'     workers mirror queue state to a sheet so a remote stakeholder can
#'     watch progress in a browser. With `ss_id = NULL` (default) the
#'     queue is purely local -- no Google APIs are touched.}
#' }
#'
#' A typical usage pattern:
#'
#' \preformatted{
#' df <- expand.grid(.scenario = c("A", "B"), .rep = 1:2,
#'                   stringsAsFactors = FALSE)
#' ef <- experimentFuture(df = df, global_path = "global.R",
#'                        n_workers = 2L, log_dir = "logs")
#' awaitExperimentFuture(ef)         # block until all rows DONE
#' }
#'
#' Swap `experimentFuture()` for `experimentTmux()` or `experimentSBATCH()`
#' (adjusting `cores` / `n_workers` / `sbatch_opts`) and the rest of the
#' driver script is unchanged.
#'
#' Related families:
#'
#' \itemize{
#'   \item [scenario_family] -- canonical record for one row of `df`,
#'     reversibly convertible between field values, an output directory
#'     path, and an upload tarball filename.
#'   \item [queueRead()] / [queueUploadMissing()] / [outList()] /
#'     [outScenarios()] -- helpers for queues persisted to a Google
#'     Sheet plus a Drive upload folder, including the
#'     queue-vs-uploads anti-join.
#'   \item [tmuxListPanes()] / [tmuxRefreshQueueStatus()] /
#'     [tmuxFindDuplicates()] / [tmuxKillPanes()] -- operational tools
#'     that work regardless of which runner produced the queue.
#' }
#'
#' @name experiment_family
NULL
