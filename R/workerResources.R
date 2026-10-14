## Per-worker terra / GDAL / PROJ settings for the experiment* launchers.
##
## Many R workers on one large host each take resources sized for the whole
## machine: GDAL's block cache defaults to 5% of RAM *per process*, and terra
## keeps results in memory. Every launcher's worker ends in tmuxRunWorkerLoop(),
## which calls .workerResourceDefaults() once, before global_path is sourced.
## A setting the user already chose (option or environment variable) is left alone.

## Fraction of RAM GDAL's block cache uses by default; shared among a host's workers.
.gdalCacheFraction <- 0.05

## Names the local host goes by in a `cores` vector.
.localHostNames <- function() c("localhost", "127.0.0.1", Sys.info()[["nodename"]])

## Total RAM of this host in MB, or NA when it cannot be read (non-Linux).
.hostRamMB <- function() {
  mi <- tryCatch(readLines("/proc/meminfo", n = 1L, warn = FALSE), error = function(e) character())
  kb <- suppressWarnings(as.numeric(sub("^MemTotal:\\s*([0-9]+).*", "\\1", mi)))
  if (length(kb) != 1L || is.na(kb)) NA_real_ else kb / 1024
}

## For each worker, the number of workers of this launcher that share its host.
## `hosts` has one entry per worker (the launcher's `cores`, recycled); local
## aliases count as one host. Falls back to 1 when `hosts` is NULL.
.workersPerHost <- function(hosts, n_workers) {
  if (is.null(hosts)) return(rep(1L, n_workers))
  hosts <- rep_len(hosts, n_workers)
  hosts[hosts %in% .localHostNames()] <- "localhost"
  as.integer(table(hosts)[hosts])
}

#' Sensible terra / GDAL / PROJ settings for one of several workers on a host
#'
#' Called once in each worker R process started by [experimentTmux()],
#' [experimentFuture()] and [experimentSBATCH()], before `global_path` is
#' sourced. Each setting is skipped if the user has already chosen it:
#' `terra::terraOptions(todisk = TRUE)` (skipped if `getOption("terra_default")$todisk`
#' is set), `terra::gdalCache()` set to 5% of this host's RAM divided by the
#' number of workers on it and never raised above GDAL's current value (skipped if
#' environment variable `GDAL_CACHEMAX` is set), and `terra::projNetwork(FALSE)`
#' (skipped if `PROJ_NETWORK` is set). One line says what was set.
#'
#' @param nWorkersOnHost Number of workers the launcher runs on this host.
#' @param ramMB RAM of this host in MB; `NA` skips the cache setting.
#' @return Invisibly, the character vector of settings that were applied.
#' @keywords internal
#' @noRd
.workerResourceDefaults <- function(nWorkersOnHost = 1L, ramMB = .hostRamMB()) {
  if (!requireNamespace("terra", quietly = TRUE)) return(invisible(character()))
  set <- character()

  if (is.null(getOption("terra_default")$todisk)) {
    terra::terraOptions(todisk = TRUE)
    set <- c(set, "terra todisk = TRUE")
  }

  if (!nzchar(Sys.getenv("GDAL_CACHEMAX")) && !is.na(ramMB)) {
    newMB <- floor(.gdalCacheFraction * ramMB / max(1L, as.integer(nWorkersOnHost)))
    curMB <- suppressWarnings(as.numeric(terra::gdalCache()))
    if (newMB >= 1 && (is.na(curMB) || newMB < curMB)) {
      terra::gdalCache(newMB)
      set <- c(set, sprintf("gdalCache = %d MB (%d worker(s) on this host)",
                            as.integer(newMB), as.integer(nWorkersOnHost)))
    }
  }

  if (!nzchar(Sys.getenv("PROJ_NETWORK"))) {
    terra::projNetwork(FALSE)
    set <- c(set, "projNetwork = FALSE")
  }

  message("Worker resource defaults: ", if (length(set)) paste(set, collapse = "; ") else "none needed (set by user)")
  invisible(set)
}
