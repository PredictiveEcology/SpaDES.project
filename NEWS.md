Known issues: <https://github.com/PredictiveEcology/SpaDES.project/issues>

version 1.0.0
=============

## New functions

* `experimentTmux()` — orchestrate multi-run parallel experiments using tmux, with queue management, heartbeat monitoring, Google Sheets mirroring, and Rstudio-compatible fallback mode.
* `runWorkerLoop()` / `runNextWorker()` — lower-level tmux worker helpers for stepping through a queue of simulation runs.
* `tmux_prepare_queue_from_df()` — build a tmux run-queue from a data frame of parameter combinations.
* `tmux_refresh_queue_status()` — refresh and report the status of a tmux queue (done / running / waiting).
* `tmux_mirror_queue_to_sheets()` — mirror a tmux queue status to a Google Sheet for remote monitoring.
* `tmux_kill_panes()` / `tmux_set_mouse()` — tmux session utilities.
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
