Known issues: <https://github.com/PredictiveEcology/SpaDES.project/issues>

version 1.0.1
=============

## New features

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
  mappings. Companion helpers `queueRead()`, `queuePending()`,
  `outList()`, `outScenarios()` work with the project queue
  (Google Sheet) and output directory.
* `outSaveTarUpload()` now accepts a pre-built `tarball` argument and
  skips the tar-build step when one is supplied. Useful when an
  earlier stage already produced the tarball and only the upload
  remains.

## Bug fixes

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
