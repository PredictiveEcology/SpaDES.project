#' Pre-built statusCalculate expressions for experimentTmux / experimentFuture
#'
#' @description
#' A family of ready-made quoted expressions for the `statusCalculate`
#' argument of [experimentTmux()] and [experimentFuture()].  Pass one of
#' these objects directly instead of writing a custom `quote({...})` block:
#'
#' ```r
#' experimentTmux(..., statusCalculate = statusCalculate_LandR)
#' ```
#'
#' Each expression is evaluated once per queue row inside
#' [tmux_refresh_queue_status()].  Before evaluation the row's non-meta
#' columns are unpacked into the local environment by name, as are any
#' objects forwarded through `...`.  The expression may assign to any subset
#' of the recognised meta-column names (`started_at`, `finished_at`,
#' `heartbeat_at`, `heartbeat_iter`, `iterationsTotal`, …) and should set
#' `done <- TRUE` to signal that the job has completed.
#'
#' @section Variables required in scope:
#' The expressions below expect the following to be available, either as
#' queue-data-frame columns or as named objects in the `...` passed to
#' [tmux_refresh_queue_status()]:
#'
#' \describe{
#'   \item{`pathBuild`}{A function whose arguments match the queue columns
#'     used to construct the output-directory path.  For
#'     `statusCalculate_LandR` the call is
#'     `pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)`.}
#'   \item{`outs`}{A list (typically stored in `dots_path` and loaded into
#'     the worker's environment before `global.R` is sourced) whose element
#'     `outs$times$end` gives the simulation end year.}
#' }
#'
#' @section Variables required in scope (`statusCalculate_FireSenseFit` only):
#' \describe{
#'   \item{`times`}{A list with elements `$start` and `$end` giving the
#'     simulation start and end years (integers).  Typically a queue column.}
#' }
#'
#' @seealso [experimentTmux()], [experimentFuture()],
#'   [tmux_refresh_queue_status()], [get_sim_year_heartbeat()]
#' @name statusCalculate_family
NULL


#' @describeIn statusCalculate_family
#' Heartbeat calculator for **LandR** vegetation simulations.
#'
#' Scans the job's output directory for `cohortData_year<XXXX>.rds`
#' checkpoint files (written at each SpaDES save event) and maps them to the
#' standard queue meta-columns:
#'
#' \describe{
#'   \item{`heartbeat_iter`}{Current simulation year reached (character).}
#'   \item{`heartbeat_at`}{Timestamp of the latest checkpoint file.}
#'   \item{`started_at`}{Timestamp of the earliest checkpoint file (may be
#'     refined later by the running-flag-file logic in
#'     [tmux_refresh_queue_status()]).}
#'   \item{`done`}{Set to `TRUE` when `heartbeat_iter >= outs$times$end`,
#'     triggering a status transition to `DONE`.}
#'   \item{`finished_at`}{Timestamp of the final checkpoint (set only when
#'     `done`).}
#'   \item{`iterationsTotal`}{The end year as a character string (set only
#'     when `done`).}
#' }
#'
#' @format A [base::quote()]d block expression (`is.call(statusCalculate_LandR)` is `TRUE`).
#' @export
#' @describeIn statusCalculate_family
#' Heartbeat calculator for **fireSense** fire-spread simulations.
#'
#' Scans the job's output directory for `burnMap_year<XXXX>.tif` files and
#' "Annual Fire Maps" output files to populate the queue meta-columns:
#'
#' \describe{
#'   \item{`heartbeat_iter`}{Most recent fire-map year found after the worker
#'     claimed the job, or `times$start` if none yet.}
#'   \item{`heartbeat_at`}{Modification timestamp of that file
#'     (`NA` until the first fire-map appears).}
#'   \item{`started_at`}{Modification timestamp of the running-flag file
#'     (i.e. when the worker claimed the job).}
#'   \item{`done`}{`TRUE` when a `burnMap` file containing `year<times$end>`
#'     is found.}
#'   \item{`finished_at`}{Timestamp of the final-year burnMap (set only when
#'     `done`).}
#'   \item{`iterationsTotal`}{The end year extracted from the burnMap filename
#'     (set only when `done`).}
#' }
#'
#' @format A [base::quote()]d block expression (`is.call(statusCalculate_FireSenseFit)` is `TRUE`).
#' @export
statusCalculate_FireSenseFit <- quote({
  outputPath <- pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)
  dd         <- dir(outputPath, recursive = TRUE, full.names = TRUE)

  if (NROW(dd)) {
    # burnMap_year<XXXX>.tif files mark completed fire-spread years
    burnMaps   <- grep("burnMap.*\\.tif$", dd, value = TRUE)
    is_done_yr <- grepl(paste0("year", times$end), burnMaps)

    if (!any(is_done_yr)) {
      # Still running — find the most recent "Annual Fire Maps" file written
      # after the worker's running-flag file (to avoid pre-run artefacts).
      runningFile <- dir(
        activeRunningPathForTmux(queue_path = queue_path),
        pattern = .ELFind, full.names = TRUE
      )
      fireMaps <- grep("Annual Fire Maps", dd, value = TRUE)
      fi_fire  <- file.info(fireMaps)

      if (length(runningFile)) {
        fi_run     <- file.info(runningFile)
        started_at <- format(fi_run[, "mtime"])

        recentMap      <- tail(fireMaps[fi_fire[, "mtime"] > fi_run[, "mtime"]], 1)
        heartbeat_at   <- if (length(recentMap)) format(file.info(recentMap)[, "mtime"]) else NA
        heartbeat_iter <- if (is.na(heartbeat_at)) {
          as.character(times$start)
        } else {
          gsub(".+Maps ([[:digit:]]{4,4}).+", "\\1", recentMap)
        }
      }
      done <- FALSE

    } else {
      # Final-year burnMap present — job complete
      finishedFile    <- burnMaps[is_done_yr]
      finished_at     <- format(file.info(finishedFile)[, "mtime"])
      iterationsTotal <- gsub(".+year([[:digit:]]{4,4}).+", "\\1", finishedFile)
      done            <- TRUE
    }
  }
})


#' @describeIn statusCalculate_family
#' Heartbeat calculator for **LandR** vegetation simulations.
#'
#' Scans the job's output directory for `cohortData_year<XXXX>.rds`
#' checkpoint files (written at each SpaDES save event) and maps them to the
#' standard queue meta-columns:
#'
#' \describe{
#'   \item{`heartbeat_iter`}{Current simulation year reached (character).}
#'   \item{`heartbeat_at`}{Timestamp of the latest checkpoint file.}
#'   \item{`started_at`}{Timestamp of the earliest checkpoint file (may be
#'     refined later by the running-flag-file logic in
#'     [tmux_refresh_queue_status()]).}
#'   \item{`done`}{Set to `TRUE` when `heartbeat_iter >= outs$times$end`,
#'     triggering a status transition to `DONE`.}
#'   \item{`finished_at`}{Timestamp of the final checkpoint (set only when
#'     `done`).}
#'   \item{`iterationsTotal`}{The end year as a character string (set only
#'     when `done`).}
#' }
#'
#' @format A [base::quote()]d block expression (`is.call(statusCalculate_LandR)` is `TRUE`).
#' @export
statusCalculate_LandR <- quote({
  outputPath <- pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)
  end_yr     <- tryCatch(as.integer(outs$times$end), error = function(e) NA_integer_)

  hb <- SpaDES.project::get_sim_year_heartbeat(
    output_path = outputPath,
    end_year    = end_yr
  )

  if (!is.na(hb$iter)) {
    heartbeat_iter <- as.character(hb$iter)
    heartbeat_at   <- hb$ts
    started_at     <- hb$started   # may be refined by running-flag-file logic
    done <- !is.na(end_yr) && as.integer(hb$iter) >= end_yr
    if (done) {
      finished_at     <- hb$ts
      iterationsTotal <- as.character(hb$iter)
    }
  }
})
