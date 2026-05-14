Known issues: <https://github.com/PredictiveEcology/SpaDES.project/issues>

version 1.0.1
=============

## New features

* New `re**` family — inverse of `out**` — for retrieving uploaded sims: `reGet()` (download from Google Drive), `reUntar()` (extract; optional `pathRemap` rewrites a path prefix via GNU `tar --transform`), `reLoad()` (`loadSimList()` / `readRDS()`), and the wrapper `reGetUntarLoad()` (also applies `pathRemap` to each sim's `outputs()$file` column). Vectorised over a batch (e.g. a `dribble` from `outList()`). See `?reGetUntarLoad`.
* New `experimentMonitor()` unifies tmux + experimentFuture worker discovery; `stats = TRUE` adds CPU / RAM / state. `tmuxListPanes()` is now a thin alias.
* `experimentFutureList(ef)` works cluster-wide: probes hostname → SSH-alias and does batched SSH liveness / kill / `readlink` per machine; `kill = TRUE` also pushes the demotion to the Google Sheet via the `<queue_path>.ss_id` sidecar.
* `experimentFuture(ss_id = ...)` drops a `<queue_path>.ss_id` sidecar for cross-session GS reconciliation.
* Cluster `runWorkerLoopFuture()` now uses `callr::r_bg(stdout = log_file)` instead of `sink()` — log files flush in real time and remote workers show up under `/proc/<pid>/fd/1`.

## Enhancements

* Fresh claims scrub stale `finished_at` / `DEoptimElapsedTime` / `heartbeat_*` / `iterationsTotal` / `interrupted_at` (both backends).
* `DONE` clears `claimed_by`; `process_id` and `machine_name` are preserved as a historical record.
* `.gs_demote_after_kill()` clears every per-run metadata column, matching `tmuxRefreshQueueStatus()`.
* `.gs_claim_next_job()` retries on lost race instead of returning `NULL`, so workers no longer silently exit on collisions.
* `.setup_remote_machine()` uses absolute paths as-is on the remote when `global_path` / `queue_path` lives outside `$HOME` (e.g. NFS-shared `/mnt/shared_cache/...`); fixes `~//mnt/...` mangling.
* `setupGitHub()` skips the clone prompt when `projectPath` is already a git working copy.
* `setUpstreamWithTry()` walks every configured remote on lost-branch error, then auto-adds the github fork (`<acct>/<repo>` from the modules spec) as a new remote when the branch lives there; emits actionable message instead of aborting if the branch is missing everywhere.

* `queueRead()` now accepts a local `.rds` queue path as its sole
  argument: `queueRead("path/to/queue.rds")`. Useful for reading the
  file-backed queues written by `experimentTmux()` /
  `experimentFuture()` / `experimentSBATCH()` without Google Sheets
  involvement. The two-argument Google-Sheet shape
  (`queueRead(folder, name)`) is unchanged.
* New `experimentFutureList()` finds (and optionally kills) live
  `experimentFuture()` workers across R sessions. Scans `/proc` for R
  processes whose redirected stdout points to a `worker_<NN>.log`,
  joins each PID against the queue's `RUNNING` entries to report which
  row is currently being run. `kill = TRUE` sends SIGTERM (or SIGINT /
  SIGKILL via `signal`); follow with `tmuxRefreshQueueStatus()` on
  each `queue_path` to reset stale RUNNING rows. Linux-only (uses
  `/proc/<pid>/fd/1`).
* New `experiment_family` documentation index (run `?experiment_family`)
  giving a high-level overview of the three runners (`experimentTmux()`,
  `experimentFuture()`, `experimentSBATCH()`), their shared
  queue / `runNameLabel` / `statusCalculate` contract, and links to all
  the companion helpers (`awaitExperiment*`, `killExperiment*`,
  `tmuxListPanes`, `tmuxRefreshQueueStatus`, etc.).
* New `experimentSBATCH()` runner: Slurm-native sibling of
  `experimentTmux()` and `experimentFuture()`. Submits `n_workers`
  long-lived SBATCH jobs that each call `tmuxRunWorkerLoop()` against
  the shared queue (RDS or Google Sheets). Same `df` / `global_path` /
  `runNameLabel` / `statusCalculate` / queue semantics as the other
  two runners; `cores` is replaced by `sbatch_opts = list(partition,
  time, mem, cpus_per_task, ...)`. Companion helpers
  `awaitExperimentSBATCH()` (polls `squeue`) and
  `killExperimentSBATCH()` (graceful via stop files; `force = TRUE`
  uses `scancel`). `dry_run = TRUE` generates the job scripts without
  submitting.
* New `scenario` S3 class for representing a single simulation run as a
  canonical record. The same run is identifiable in three ways and all
  three coerce to one another: the five field values (`.ELFind`,
  `.samplingRange`, `.GCM`, `.SSP`, `.rep`), an output directory path
  (e.g. `outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5`), and an
  upload-tarball filename (e.g.
  `6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz`). New API:
  `scenario()`, `as_scenario()` (with methods for character paths,
  lists, data.frames, and re-coercion), `as_path()`, `as_tarname()`,
  `format.scenario`, `print.scenario`, and
  `register_scenario_aliases()` for project-specific column-name
  mappings. Companion helpers `queueRead()`, `queueUploadMissing()`,
  `outList()`, `outScenarios()` work with the project queue
  (Google Sheet) and output directory.
* `outSaveTarUpload()` now accepts a pre-built `tarball` argument and
  skips the tar-build step when one is supplied. Useful when an
  earlier stage already produced the tarball and only the upload
  remains.

## Removed

* `experiment3()` (and its `tmux_tail_command` helper) have been
  removed. Use `experimentFuture()` for non-tmux parallel runs, or the
  10-line `furrr::future_pmap()` kernel inline if you want a
  queue-less micro-runner. The conceptual differences are documented
  in the "Experiments" chapter of the SpaDES4Modellers book.

## Removed

* Drop `SpaDES.config` dependency (Suggests + Remotes); resolves the circular dep flagged in #78. The `config` arg to `setupProject()` was never wired up and now stops with a clearer message.

## Bug fixes

* `tmuxRunNextWorker()` no longer calls `reproducible::checkPath()`; uses `dir.create()` so workers run under `_R_CHECK_DEPENDS_ONLY_=true` (where Suggests are absent).
* `setupPaths()` detects an R version change since the previous run (e.g. 4.3 -> 4.5) by comparing the running R `major.minor` to the trailing version segment of `.libPaths()[1]`. On mismatch it calls `Require::setupOff()` to clear the stale `.Rprofile` block before the regular `Require::setLibPaths(updateRprofile = TRUE)` rewrites both `.libPaths()` and `.Rprofile` for the current R.
* `reUntar()` tests now skip on systems without GNU tar (BSD tar on macOS rejects `--absolute-names` / `--transform`); regenerate `outSave()` / `outSaveTarUpload()` Rd to include `lazy`; trim stray `@param`s on the `as_scenario()` generic so they no longer trip the `Rd \usage` check. Together these unstick GHA R-CMD-check on macOS.
* `reUntar()` / `reGetUntarLoad()` now `path.expand()` the `pathRemap` `old` / `new` so a leading `~` works (tar's `--transform` does not expand `~`).
* Positional `pathBuild()` now infers field names from bare-symbol
  arguments (e.g. `pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)`
  caches `c(".ELFind", ..., ".rep")` as `scenarioFields()` automatically).
  This restores the canonical call style used in `global.R` /
  `setupProject(paths = ...)` without requiring an upfront
  `queueRead()` or `scenarioFieldsSet()`. Positional calls with
  literals (`pathBuild("foo", 1L)`) continue to require a primed
  `scenarioFields()`.
* `experimentTmux()`, `tmuxRefreshQueueStatus()`, and the worker
  loops (`runWorkerLoopFuture()`, `.sync_loop_internal()`) now call
  `scenarioFieldsSet()` on the queue's data columns before evaluating
  `runNameLabel` / `statusCalculate`. This restores positional
  `pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)` calls (used
  by `statusCalculate_LandR`, `statusCalculate_FireSenseFit`, and
  many user `runNameLabel` quotes) when the queue comes from a
  programmatic `df` rather than from `queueRead()` (which sets the
  field cache on its own).
* `outUpload()` now calls `tempdir(check = TRUE)` before invoking
  `googledrive::drive_upload()` so the session tempdir is recreated if
  it has been deleted out from under R (intermittent NFS/cleanup
  issue).
* `setupProject()` now copies all required dependency packages (e.g., `pak`,
  `withr`) to the isolated project library, not just those whose namespaces
  happen to be loaded at call time. Previously, `setupSpaDES.ProjectDeps()`
  used `getNamespaceInfo(pkg, "path")` to locate packages, which returned `""`
  for unloaded namespaces; the subsequent `file.exists(.../INDEX)` check failed
  silently and the "Copying X packages" message was misleading. Now falls back
  to `find.package()` with `lib.loc` pointing to the caller's pre-switch
  `.libPaths()` (plumbed in via a new `prevLibPaths` arg).
* Fixed code coverage reporting: `NOT_CRAN=true` is now set in the test-coverage workflow so that `skip_on_cran()` tests run under `covr`.
* Fixed Windows path comparison in `scratchPath` test.
* `setupProject()` with `useGit = TRUE` now initializes the project git repository on branch `main` instead of `master`.
* Removed `mockery` dependency from tests.
* Fixed test warnings about non-portable paths (> 100 bytes) on macOS/Windows by suppressing in `pkgload2` test.
* Skipped `setupProject` package-installation test on Windows/macOS due to upstream `Require@development`/`data.table` incompatibility.
* Expanded test suite with utility-function tests (`spadesProjectOptions`, `setProjPkgDir`, `pkgload2`, `getModule`, `listModules`, etc.) and added `packagePath = .libPaths()[1L]` to `setupProject` test calls to prevent lib-path clobbering during testing.

version 1.0.0
=============

## New functions

* `experimentTmux()` — orchestrate multi-run parallel experiments using tmux, with queue management, heartbeat monitoring, Google Sheets mirroring, and Rstudio-compatible fallback mode.
* `tmuxRunWorkerLoop()` / `tmuxRunNextWorker()` — lower-level tmux worker helpers for stepping through a queue of simulation runs.
* `tmuxPrepareQueueFromDF()` — build a tmux run-queue from a data frame of parameter combinations.
* `tmuxRefreshQueueStatus()` — refresh and report the status of a tmux queue (done / running / waiting).
* `tmuxMirrorQueueToSheets()` — mirror a tmux queue status to a Google Sheet for remote monitoring.
* `tmuxKillPanes()` / `tmuxSetMouse()` — tmux session utilities.
* `assessDoneInFigure()` — visually assess simulation completion status from output figures.
* `preRunSetupProject()` — source and partially evaluate a `global.R` script (up to a chosen call) before running experiments, so shared setup code runs once.
* `plotSAs()` / `plotSAsLeaflet()` — plot `studyArea*` and `rasterToMatch*` objects from a list, using ggplot2 or leaflet respectively.
* `spadesProjectOptions()` — document and set `SpaDES.project`-specific R options.

## Enhancements

* `setupProject()`: the `Require` install/load call is now optionally cached via `reproducible::Cache`, controlled by the new `cacheRequire` argument, reducing repeated package-resolution overhead across runs.
* `setupProject()`: `...` arguments must now be named; an informative error is raised otherwise.
* `setupProject()` / `pathsOverrideIfInTemp()`: cross-platform fix for detecting when the entire project lives inside a temp directory — now uses `fs::path_has_parent()` instead of a root-path string comparison that failed on Windows.
* `setupProject()`: `studyAreaName2()` no longer requires `reproducible` to be installed when `studyArea = NULL`.
* `setupProject()`: `setupStudyArea()` `Cache` call now passes `useCloud = FALSE` and a safe `cacheSaveFormat` fallback, preventing failures when `reproducible.useCloud` is set globally or when using `reproducible` >= 3.0.0.
* `setupProject()`: `evalDots()` now uses `inherits = FALSE` when checking for default-dot values, preventing base R functions (e.g. `mode()`) from masking user-intended defaults.
* `experiment3()`: new `logFiles` and `delay` arguments; improved parallel execution via `furrr`.
* `setupPackages()`: improved messaging when some packages are at `HEAD` versions.
* Local files containing vectors of package names are now supported in package-list arguments.
* `collect_showCache_async()`: no longer stops on error.

## Bug fixes

* Fixed `isRstudio()` returning the wrong answer when called from an RStudio terminal (vs. console).
* Fixed `sideEffects` missing `parseFileLists` call.
* Fixed `build_proxy` error when `setupProject()` is called with no `...` arguments.
* Fixed `dim` issue in spatial helpers.
* Fixed `parallel::pskill` (does not exist) → `tools::pskill` in tmux worker heartbeat code.
* Removed use of `:::` for `reproducible::paddedFloatToChar` and `SpaDES.core::savedSimEnv`.
* Replaced `ggpubr` with `patchwork` for study-area plots, removing the transitive `ggrepel` dependency that required R >= 4.5.

version 0.1.3
=============
* `experiment3` new function to be used with `setupProject`

version 0.1.2
=============
* Allow modules to be specified using 1 of 2 full urls e.g., "https://github.com/..." see `?setup`, `modules` argument.
* drop support for R <= 4.2 as several dependencies don't work rstatix, car, quantreg, MatrixModels, Matrix
* `Restart` had infinite restarting; fixed; was related to new elements address unrelated Git issues

version 0.0.9
=============
* `Restart` argument of `setupProject` now uses better `rstudioapi` calls to open new project with active file.


version 0.0.8
=============

## Enhancements
* overhaul of all internals
* `setupProject` is a new omnibus function to address a number of needs for a SpaDES project, 
including `paths`, `modules`, `options`, and others. See `?setupProject`

## deprecations
* `newProject` has been replaced by `setupProject`

version 0.0.4
=============

## Enhancements
* `newProject` can now take a vector of module names; if provided, these will be downloaded to the `modulePath` directory.

version 0.0.2
=============

* move `SpaDES.core` from `Imports` to `Suggests`.

version 0.0.1
=============

* initial version
