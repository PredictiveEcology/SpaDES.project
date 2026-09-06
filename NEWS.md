Known issues: <https://github.com/PredictiveEcology/SpaDES.project/issues>

version 1.1.0.9009
==================

## Rewrite of `...` / `defaultDots` resolution in `setupProject()`

The machinery that resolved `...` ("dot") arguments -- a proxy environment of
active bindings, `evalDots()`, `evalDotsOuter()` and a four-stage retry ladder
that walked every frame on the call stack -- is replaced by one small resolver
(`R/resolveDots.R`) built on a single scope environment whose parent is the
calling environment. The rules it implements are the documented ones, now
literally:

* every argument is evaluated once, in the order it is written, and sees the
  resolved value of every argument written above it -- dots and formals alike --
  as in a script;
* every `defaultDots` entry the caller did not supply is bound as a value before
  anything is evaluated, so it is available to any argument that names it (a
  dot, a dot under another name such as `cores = .cores`, or a formal such as
  `times = as.list(unlist(.times))`); a caller-supplied value always wins, and a
  name that only resolves to a package function does not count as supplied;
* only values are ever bound in that scope, never unevaluated expressions, so a
  dot's own name is not visible to its own expression and
  `.x = if (exists(".x")) .x else <fallback>` behaves as written.

Fixed as a consequence (all reproduced in `tests/testthat/test-dots-resolution.R`):

* with no `defaultDots` at all, a self-defaulting dot came back as its own
  unevaluated `if` call, because the scoped evaluation was skipped entirely and
  the expression was published under the dot's name;
* `defaultDots` given as a variable rather than a literal `list()` silently
  supplied no defaults to dots written before the first formal;
* a `defaultDots` entry could not reference one written above it;
* a dot that evaluated to `NULL` was treated as a failure, retried across the
  call stack, and could vanish from the result. `NULL` is now a value, kept
  under its name;
* the `envir` argument was documented but ignored; it is now the parent of the
  resolution scope.

* formals are resolved from the call site outward (#158): a formal given as a
  bare symbol resolves to the caller's variable before any package object of
  the same name (`params = pf` no longer becomes `stats::pf`), and the
  `setup*` helpers evaluate against the same scope the `...` arguments use, so
  an `options` list can combine a caller-local variable with `paths$...`.
  `evalSUB()` also stops when an iteration makes no progress instead of looping.

Behaviour change to be aware of: a `...` expression sees exactly what a script
at the call site would see -- the calling environment and the attached packages
-- and no longer the packages `SpaDES.project` itself imports. Code such as
`scenario = { data.table(...) }` with nothing attaching data.table used to work
only because the expression was evaluated inside `setupProject()`'s own frame;
it now comes back as a tolerated error naming the function. Attach the package
(`require = "data.table"`) or qualify the call (`data.table::data.table()`).
A dot that references a formal written *below* it (e.g. `outputs` using
`times$end` with `times` declared afterwards) likewise used to work by forcing
the formal's promise early, and is now a tolerated error: write it above.

Behaviour that is deliberately unchanged: a dot that cannot be evaluated even
with the defaults is returned as its unevaluated expression, recorded as a
tolerated error in the end-of-call diagnostics, and escalated only under
`options(SpaDES.project.strict = TRUE)`; dots written before the first formal
run before it, the rest run in written order after the packages are set up.

version 1.1.0.9008
==================

## Bug fixes

* `setupProject()`: a `...` argument declared after `defaultDots` is now resolved
  before `paths` (and the other formals) are evaluated, so it can be used inside
  them. Dots are evaluated in two batches -- those before the first formal
  argument run before the formals, the rest after -- and `defaultDots` counted as
  a formal for that split. A dot written after it, which is the natural place
  since that is where its fallback lives, was therefore not resolved until after
  `paths` had already been built. `pathBuild()` received the dot's unevaluated
  expression and deparsed it into a directory name:

      outputs/.ELFind/370                                     (for `.foo = .ELFind`)
      outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/...

  while `out$.foo` was, by the end of the call, correctly `"4.3"`. This broke the
  documented headline contract -- "any argument written above another is
  available to it" -- for exactly the layout `global.R` files use, and the only
  signal was `is.na() applied to non-(list or vector) of type 'symbol'`, a
  warning.

  `defaultDots` is a fallback table, not a configuration block, and now joins
  `params`, `studyArea` and `times` in not splitting the dot sequence. The
  evaluation-order documentation is corrected to say so. Dots declared after a
  real formal such as `paths` keep their documented late evaluation.

version 1.1.0.9007
==================

## Bug fixes

* `experimentTmux()` no longer discards `df` silently when `queue_path` already
  exists. An existing queue stays authoritative -- that is what lets a resumed run
  keep its `DONE`/`RUNNING` rows rather than repeating finished work -- but the
  rule was applied without a word, so a caller who had rebuilt `df` (new scenarios
  added, completed ones dropped) would watch the previous queue run instead, with
  nothing to distinguish that from success. The only remedy was to notice, and
  delete or rename the file.

## Enhancements

* `experimentTmux()` gains `onExistingQueue`, and the reconciliation is exposed as
  `tmuxReconcileQueueWithDF()`:
  `"resume"` (default, the previous behaviour) keeps the existing queue and now
  *warns* when `df` holds rows it does not, naming them and the two ways to act on
  them; `"append"` adds just those rows as `PENDING`, leaving existing rows and
  their status untouched; `"rebuild"` starts again from `df`.
  Rows are compared on the scenario columns -- non-meta, non-list -- so list
  payload such as `.modules`/`.times` does not make an existing scenario look new.

version 1.1.0.9006
==================

## Bug fixes

* `test-makeDESCRIPTION.R` skips when `SpaDES.core` is unavailable. Those tests
  predate the delegation and had no guard, so the no-suggests leg of
  `R CMD check` -- where Suggests are deliberately absent -- failed with 11
  errors instead of skipping. This is what turned `development` red after the
  delegation landed.
* `VersionOnRepos` is declared in `R/imports.R`'s `globalVariables()`. It had been
  declared in `R/makeDESCRIPTION.R`, which no longer needs it; removing that block
  left `getVersionOnRepos` with a "no visible binding" NOTE.
* `makeDESCRIPTION()` checked only that `SpaDES.core` was installed before
  delegating to `SpaDES.core::DESCRIPTIONfromModule()`. `SpaDES.core` is in
  Suggests, so its version floor cannot be enforced at install time and an older
  copy is both installed and importable -- which surfaced as
  `'DESCRIPTIONfromModule' is not an exported object from 'namespace:SpaDES.core'`
  rather than something the reader can act on. It now checks for the function.

version 1.1.0.9004
==================

## Internal

* `makeDESCRIPTION()` delegates to `SpaDES.core::DESCRIPTIONfromModule()` instead
  of carrying its own copy of the metadata -> `DESCRIPTION` translation.
  `SpaDES.core` had an independent implementation inside `convertToPackage()`, and
  the two had drifted -- each had fixes and features the other lacked, and this
  package even inlined its own `.moduleNameNoUnderscore()` to avoid reaching into
  `SpaDES.core`. `SpaDES.core` owns module metadata (`defineModule()`,
  `packages()`, `moduleMetadata()`), so the translation lives there; the
  project-level entry points `makeDESCRIPTION()`/`makeDESCRIPTIONproject()` stay
  here. `SpaDES.core` is in Suggests, so the delegation is guarded by
  `requireNamespace()` and errors clearly if it is absent.
  Requires `SpaDES.core (>= 3.2.1.9002)`.

  Two bugs fixed on the `SpaDES.core` side come along with this: a module
  `description` written as a `paste()` call was deparsed into the field
  (`Description: paste ...`), and the version was read positionally rather than
  by module name.

version 1.1.0.9002
==================

## Bug fixes

* `setupProject()`: a `...` argument whose value comes from `defaultDots` reached
  later arguments as its *unevaluated expression* rather than its value, so any
  consumer that pastes it into a string -- most visibly `pathBuild()` in a `paths`
  argument -- deparsed it into the value's place, producing directories such as
  `outputs/.ELFind/.GCM` or
  `outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/unlist_.samplingRange`.
  `build_proxy()` installs one active binding per dot that closed over the
  capture-time value/expression and never consulted `cur` again; `capture_dots()`
  leaves that value `NULL` for every dot that `defaultDots` supplies, so the
  binding returned the expression. Meanwhile `evalDots()` *did* resolve the dot,
  writing it into `cur`, but `expose_new_bindings()` only forwards names that are
  not already bound, so the stale binding was never upgraded to the live
  `bind_forward()` behaviour every other name gets. Dot bindings now forward to
  `cur` whenever it holds the name, matching `bind_forward()`.
  This only bit callers that let `defaultDots` supply a dot -- i.e. batch/spawn
  workflows that deliberately do not define it in the calling environment.

version 1.1.0.9001
==================

## Enhancements

* `plotSAsLeaflet()` gains `labelCols` (default `c("ID", "Name", "Names")`): each
  `studyArea**` polygon is now labelled on hover with its own value from the first
  of those columns the layer carries, rather than every polygon sharing the layer
  name. A layer with none of those columns, or with no attribute table at all,
  falls back to the layer name as before.

* `reGetUntarLoad()` gains `pattern`: supply it and `gFiles` is taken as the Drive
  *folder*, listed with `outList()`. `reIndex()`'s `_index.rds` sidecars are
  always excluded.

## Enhancements

* `reUntar()` gains `skipExisting` (default `TRUE`): skips archives whose members
  are already on disk at the size the archive records.
* `reGetUntarLoad()` gains `skipExisting`, and `useCache` to `Cache()` the
  `reGet()` metadata step.

## New features

* `reIndex()` and `reGetMember()` read a single object out of a tarball on Google
  Drive using an HTTP range request, without downloading the archive: on a 4.63 GB
  archive one 6.2 MB object takes ~1 s versus ~5 min for the whole file.
* `reGetUntarLoad()` gains `remote`. With `remote = TRUE` the archive is never
  downloaded: the `simList` shell, its manifest and the output files are fetched
  from inside it by HTTP range request, and each object arrives only when
  something touches it. `$`, `[[` and `get()` are unchanged. Default `FALSE`.
  Each fetch announces itself before transferring, naming the object and its size,
  so a pause is visibly a download; silence with
  `options(SpaDES.project.remoteVerbose = FALSE)`.

## New features

* `reLoad()` and `reGetUntarLoad()` gain `parse`; `parse = FALSE` skips re-parsing
  module source code, which dominates the load time of a lazily saved `simList`.
  The result is inspect-only and cannot be passed to `spades()`.

## Bug fixes

* `getModule()` no longer assumes `Require::splitGitRepo()` returns exactly four
  elements. It now selects `acct`/`repo`/`br` by name, fixing "Supplied 3 columns
  to be assigned 4 items" after Require gained a `subFolder` element.
* `reUntar()` no longer emits GNU tar's `Removing leading '/' from member names`
  for every archive.
* `outTar()` now bundles the `<simFilename sans ext>_lazy/` directory written by
  `SpaDES.core::saveSimList(lazy = TRUE)`. It previously looked for the older
  `<simFilename>_xData.rdx`/`.rdb` pair, which SpaDES.core no longer writes;
  finding neither, it produced a tarball holding only the shell `.rds`. Because a
  lazily saved shell is a few hundred KB of metadata, that silently shipped an
  archive with nothing behind its promises.

version 1.1.0
=============

## New features

* `outSave()` gains `verbose`, defaulting to `getOption("reproducible.verbose", 1)`; it previously failed when that option was unset.
* `makeDESCRIPTION()` and `makeDESCRIPTIONproject()` are now exported, and work: build a DESCRIPTION from module metadata, or one project-level DESCRIPTION merging every module's `reqdPkgs`.

* `plotChangeOverTime()` now iterates **all** time-series objects discovered under `simList`/directory inputs (no more `name` argument) and presents them as radio-selectable base layers — same UX as `SpaDES.shiny::shine()`'s "Change from start to end" tab. A per-object legend renders in `legendPosition` (max value at top, blue = positive; uses shine's `rev(pal) + labFormat transform` idiom), and a small `htmlwidgets::onRender()` shim listens for leaflet's `baselayerchange` event to show only the active layer's legend.
* `plotChangeOverTime()` defaults to `terra::map.pal("differences")` (blue→white→red, purpose-built for difference maps) instead of `hcl.colors("RdBu", rev = TRUE)`. Falls back to `hcl.colors()` for any palette name `terra::map.pal()` doesn't recognise. Default `rev = TRUE` puts red = negative, blue = positive (override with `rev = FALSE`).
* `.scanOutputDirForTimeSeries()` now inspects the first raster in each time-series group and, for multi-band SpatRasters (e.g. `speciesLayers_xxx`), expands into one object per band — the shine pattern (`.shineScan` L148-162). Each band becomes its own slider/legend in `plotTimeSeriesLeaflet()`. Internally, each scanned object now carries `band` + `times` (previously just a flat times data.frame), and the loaders pass `lyrs = band` to `terra::rast()` so the right band is read.
* `plotTimeSeriesLeaflet()` rewritten as a multi-object viewer to mirror `SpaDES.shiny::shine()`'s "Maps" tab. The `name` and `years` arguments are dropped; the function discovers all time-series objects in the input (`simList` or directory path), adds every `(object, year)` raster as a leaflet base layer, and injects an object radio + year slider into a custom leaflet control (default `bottomleft`). Switching objects re-ranges the slider and shows the matching layer; per-object continuous legends (shine's `rev(pal) + reversed labFormat` idiom, max-at-top) toggle with the active object via `htmlwidgets::onRender()` className-index lookup. `fitBounds()` with padding scaled for both the slider and legend keeps the raster from being obscured on initial render.
* `plotTimeSeriesLeaflet()` and `plotChangeOverTime()` now also accept a `simList` or a directory path. When given one of those, they scan `outputPath()` for `<name>*.tif` files (with the new `name` argument naming the time-series, e.g. `"simPred"`), parse the embedded year/timestamp from each filename, and load the GeoTIFFs from disk. The discovery + parsing logic is ported from `SpaDES.shiny:::.shineScan()` so both packages group time-series the same way (last regex match in the stem = time, remainder = key, trailing "year" stripped).
* New `plotTimeSeriesLeaflet(x, years, ...)`: takes a multi-layer `SpatRaster` (or list of single-layer `SpatRaster`s) and produces a single self-contained leaflet htmlwidget with a draggable time-step slider. The slider drives the leaflet layer-control radios, so dragging it steps through years. Pure-JS injection via `htmlwidgets::onRender()` — no Shiny server, no new R deps; ships as static HTML on GitHub Pages. Designed to replicate the "step through time" part of `SpaDES.shiny::shine()` without server-side state.
* New `plotChangeOverTime(x, from, to, ...)`: the "change from start to finish" companion. Subtracts the `from` layer from the `to` layer and plots the difference on a leaflet map with a diverging palette (default `RdBu`, reversed) and a symmetric (zero-centred) colour scale. Defaults `from`/`to` to the first and last named layers.
* `plotSAsLeaflet()` is now usable inside a Quarto/knitr render that ships to a static site (e.g., GitHub Pages). When `knitr.in.progress` is `TRUE`, the GeoTIFFs written for `leafem::addGeotiff()` go under `knitr::fig_path()` (the chapter's `_files/figure-html/` folder Quarto copies alongside the rendered HTML) instead of `tempfile()`. Previously the widget embedded an absolute `/tmp/RtmpXX/...` path that (a) was wiped when the render's R session exited and (b) was not browser-fetchable from the deployed HTML, leaving raster panels broken on the live site. Interactive use (RStudio viewer, plain R) still uses `tempfile()`. Internal helper `.leafletGeoTiffPath()` added with tests.
* `setupProject()` now honours `getOption("spades.packagePath")` as a default override for `paths$packagePath` when the user does not supply it. This option was previously listed in `spadesProjectOptions()` but never read. Lets callers (e.g., a Quarto render agent that already has its libraries loaded) pin the project library to a specific path without modifying the visible `setupProject()` call.
* `setupProject()` now honours `getOption("spades.projectPath")` as a default override for `paths$projectPath`. Previously, when the user supplied `name` but not `paths$projectPath`, `checkProjectPath()` derived projectPath from `name` and silently ignored the option (the user-set value was masked because `spadesProjectOptions()` sets `options(spades.projectPath = ".")` as a side effect). The user-set value is now captured before that side effect and given highest priority after an explicit `paths$projectPath`.
* `setupProject()` honours `options = list(Require.noRemotes = TRUE)`: GitHub-style package specs (`account/repo@branch`), including those declared in module `reqdPkgs` metadata, are resolved from `repos` (e.g., binaries on `predictiveecology.r-universe.dev`) instead of being built from GitHub source. This avoids git authentication and a source-build toolchain (e.g., Rtools) for end users such as workshop participants. The option is applied (first options pass) before any package install. Implemented in `Require` (see `Require::RequireOptions`); no SpaDES.project-specific configuration is needed beyond passing the option.
* `setupProject()` no longer munges `options(repos)` itself; resolving the `@CRAN@` placeholder and de-duplicating repos is deferred to `Require::getCRANrepos()`, which preserves all existing repos (named or unnamed). This fixes `setupProject()` previously dropping unnamed repositories (e.g. an `r-universe` added via `unique(c(extraRepo, getOption("repos")))`, which strips names), which had broken `Require.noRemotes` installs.
* `experiment()`, `experiment2()`, `factorialDesign()`, `simInitAndExperiment()` and the `simLists` class (with `as.data.table.simLists()`) moved here from the now-unmaintained `SpaDES.experiment`; `experiment()` is now a light wrapper that builds the factorial set of `simList`s and runs them via `experiment2()`.
* `experiment2()` (and `experiment()`) forward named `...` such as `events` to `SpaDES.core::spades()`; the file-queue `experiment**` family supports per-scenario events via an `events` column in `df` (#20).
* New `teardownProject(out)`: reverses a `setupProject()` call. Removes the project library, unlinks the project paths, and restores the prior `.libPaths()` that `setupProject()` now stores on its output as `out$paths$.previousLibPaths` (#31). The previous (dot-prefixed) `.teardownProject()` is kept as an alias.
* New `re**` family — inverse of `out**` — for retrieving uploaded sims: `reGet()` (download from Google Drive), `reUntar()` (extract; optional `pathRemap` rewrites a path prefix via GNU `tar --transform`), `reLoad()` (`loadSimList()` / `readRDS()`), and the wrapper `reGetUntarLoad()` (also applies `pathRemap` to each sim's `outputs()$file` column). Vectorised over a batch (e.g. a `dribble` from `outList()`). See `?reGetUntarLoad`.
* New `experimentMonitor()` unifies tmux + experimentFuture worker discovery; `stats = TRUE` adds CPU / RAM / state. `tmuxListPanes()` is now a thin alias.
* `experimentFutureList(ef)` works cluster-wide: probes hostname → SSH-alias and does batched SSH liveness / kill / `readlink` per machine; `kill = TRUE` also pushes the demotion to the Google Sheet via the `<queue_path>.ss_id` sidecar.
* `experimentFuture(ss_id = ...)` drops a `<queue_path>.ss_id` sidecar for cross-session GS reconciliation.
* Cluster `runWorkerLoopFuture()` now uses `callr::r_bg(stdout = log_file)` instead of `sink()` — log files flush in real time and remote workers show up under `/proc/<pid>/fd/1`.

## Enhancements

* `setupProject()` / `setupPackages()` print the `dput()` of the exact package vector passed to `Require::Require` at `verbose >= 3`.
* When a worker claims a job, any leftover status from a previous attempt (finish time, elapsed time, heartbeat, iteration count, interruption time) is cleared first. This applies to both the Google Sheet and the file-based job queues.
* When a job is marked finished, its "claimed by" marker is cleared, while the process ID and machine name are kept as a record of what ran it.
* After a running job is killed, its row in the job-tracking sheet is fully reset so it can be picked up and run again.
* When two workers try to claim the same job at the same moment, the one that loses now retries with the next job instead of quitting.
* Running jobs on another machine no longer corrupts file paths that live on shared/network storage outside the home directory (e.g. `/mnt/shared_cache/...`).
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
  `register_scenario_format()` for project-specific column-name
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
* `SpaDES.config` is no longer a dependency. The `config` argument of `setupProject()` is still reserved but currently does nothing except give a clearer error (#78).

## Documentation

* `setupPaths()`, `setupModules()`, `setupPackages()`, and the other inner `setup*` helpers each have their own help page now; `?setup_family` is a new one-page overview showing how they fit together (#44).
* `setupProject()`: each argument's `@param` now states its default explicitly; the `getOption("SpaDES.project.*")` signature defaults also carry their fallback inline (e.g. `, TRUE`) so the source is self-documenting (behaviour is unchanged — these were already backfilled from `spadesProjectOptions()` at runtime via `assignDefaults()`).
* `spadesProjectOptions()`: help page now documents the default and meaning of every option it returns.

## Bug fixes

* `experimentTmux()` workers now receive the full library search path; they previously got only `.libPaths()[1]`, so under `R CMD check` / `covr` they could not load `SpaDES.project` itself and exited with "there is no package called".

* `setupProject(Restart = TRUE)` re-opens the global script after restarting Positron, via the `positron.session_init` hook (Positron >= 2026.04); on older Positron it prints the path to open instead of writing a hook that would never fire.
* `setupProject(Restart = TRUE)`: the generated `.Rprofile` cleanup no longer truncates the project `.Rprofile` (its `grep()` pattern never matched, and `readLns[-integer(0)]` returns nothing).
* `setupProject(Restart = TRUE)` no longer restarts endlessly in Positron: the "already in this project" test now compares the open workspace folder, instead of looking for an `.Rproj` file Positron never creates.
* `plotSAsLeaflet()` no longer fails on a study area with no attribute table; the polygon label was a formula, which made leaflet resolve it against the geometry's attributes.

* `plotSAs()` now uses each `rasterToMatch`'s own entry from `rasterToMatchPalette`; it previously used the first entry for every panel.
* `plotSAsLeaflet()` now resolves RColorBrewer palette names to colours; `leafem::colorOptions()` silently stored the name itself, so Brewer palettes did not render.

* `experimentMonitor()` no longer errors when no panes are running: `.tmux_attach_ps_stats()` checked for an empty table only after initialising its columns.
* `plotSAs()` and `plotSAsLeaflet()` no longer lose their layer list when reprojecting to lat/long: `toLatLong()` now returns the full list rather than `NULL` or a subset.
* `rasterToMatchPaletteNamed()` returns an empty palette, rather than the function itself, when no palette entry is named.
* `setupGitIgnore()` now writes the default ignore entries (they were computed and discarded), resolves `.gitignore` against `projectPath` rather than the working directory, and matches paths literally instead of as regular expressions.
* `linkOrCopyFiles()` no longer hangs when two source directories share their last two path segments; destinations now keep the fewest path segments needed to stay distinct.
* `setupProject()` recovers from an interrupted repo creation: a `.git` with no commits yet (unborn branch) is no longer mistaken for a finished repo, so a re-run completes the first commit and push (previously `isProjectGitRepo()` skipped creation and `setUpstreamWithTry()` errored with `argument is of length zero`).
* `setupProject()` only prompts to edit the global git config when `user.name`/`user.email` are unset, instead of on every new-repo setup; avoids hanging in no-terminal front-ends (e.g. RStudio Server) where the editor can't be answered.
* `experimentTmux()` no longer errors with "invalid 'path' argument" when `gargle_oauth_cache` is unset or `NA` (its default).
* `plotSAs()`/`plotSAsLeaflet()` no longer pick up dot-prefixed study-area objects (e.g. `sim.studyArea`) as layers to plot.

* `setupProject()`: a `studyArea` that can't be evaluated (e.g. a not-yet-installed package or an unreachable file) is now a tolerated error returned unevaluated, like the `...` arguments, instead of hard-stopping the whole call. It is reported in the end-of-call diagnostics summary; set `options(SpaDES.project.strict = TRUE)` to stop on it.
* `setupProject()` no longer prints a misleading "Module 'x' is specified N times" message when several SpaDES child modules live in subfolders of one git repo (e.g. `PredictiveEcology/scfm@development/modules/scfmIgnition`); the repo is simply cloned once and the message is now only shown for genuine repo overrides.
* `setupProject()` now evaluates each `...` argument exactly once, sequentially in declaration order. Previously a `{ }` block or self-referential dot (e.g. `.studyAreaName = { ... }`, `.samplingRange = unlist(.samplingRange)`) could be evaluated 2-3 times, and complex `...` expressions were force-evaluated prematurely in the caller scope (running side effects such as downloads early). Caller-supplied values still take precedence over `defaultDots`. See the "Argument order (evaluation sequence)" section of `?setupProject`.
* `setupProject()` no longer errors with `object '.<name>' not found` when a formal argument is itself an unevaluated expression referencing a `defaultDots`-supplied dot (e.g. `modules = unlist(.modules)`, `times = as.list(unlist(.times))`). The single-pass `...` evaluation now excludes those unevaluated formal-argument promises when building a dot's evaluation scope, so they are not forced prematurely (regression from the single-pass change above).
* `plotSAs()` (which plots study areas) no longer fails when the raster used for matching has categorical (factor) layers: those layers are now drawn with a discrete colour scale instead of erroring with "Discrete value supplied to a continuous scale". It also now handles plotting a study area on its own (with no matching raster), which previously failed.
* `setupProject()`: CRAN placeholder guard no longer errors with `subscript out of bounds` when `getOption("repos")` is an unnamed character vector or lacks a `CRAN` entry.
* `tmuxRunNextWorker()`: workers no longer need the `reproducible` package to start.
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
* `setupProject()` with `useGit = TRUE` now initializes the project git repository on branch `main` instead of `master`.
* Removed `mockery` dependency from tests.
* Fixed test warnings about non-portable paths (> 100 bytes) on macOS/Windows by suppressing in `pkgload2` test.
* Skipped `setupProject` package-installation test on Windows/macOS due to upstream `Require@development`/`data.table` incompatibility.

version 1.0.1
=============

## Bug fixes

* Fixed code coverage reporting: `NOT_CRAN=true` is now set in the test-coverage workflow so that `skip_on_cran()` tests run under `covr`.
* Fixed Windows path comparison in `scratchPath` test.
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
