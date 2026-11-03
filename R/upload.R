#' Save a SpaDES simulation to an RDS file
#'
#' @description
#' Saves a `simList` to an RDS file via [SpaDES.core::saveSimList()].
#' Heavy ancillary data (inputs, outputs, cache, files) are excluded so the
#' file contains only the simulation state; pair with [outTar()] to bundle the
#' output files separately.
#'
#' @param sim A `simList` object.
#' @param runName Character scalar. Used as the base name for the saved sim
#'   file and tarball.
#' @param simFilename Character scalar. Full path for the `.rds` file.
#'   Defaults to `SpaDES.core::simFile(name = runName, path = outputPath(sim),
#'   time = end(sim), ext = "rds")`.
#' @param lazy Logical. Passed to [SpaDES.core::saveSimList()]. When `TRUE`
#'   (default), each user object in `sim@.xData` and each module's `mod` objects
#'   in `sim@.xData$.modObjs` are written to their own file in a sibling
#'   `<simFilename sans ext>_lazy/` directory, and lazily restored on load via
#'   `delayedAssign`. [outTar()] picks up that directory automatically.
#'
#' @param verbose Numeric message level, forwarded to
#'   [SpaDES.core::saveSimList()]. Defaults to `getOption("reproducible.verbose", 1)`
#'   -- note the fallback: `saveSimList()` resolves its own default as a bare
#'   `getOption("reproducible.verbose")`, and passes the result to
#'   `Require::messageVerbose()`, whose `if (verbose >= verboseLevel)` fails with
#'   "argument is of length zero" whenever that option happens to be unset.
#'
#' @return Invisibly returns `simFilename`.
#' @seealso [outTar()], [outUpload()], [outSaveTarUpload()]
#' @export
outSave <- function(sim, runName, simFilename = NULL, lazy = TRUE,
                    verbose = getOption("reproducible.verbose", 1)) {
  if (is.null(simFilename))
    simFilename <- SpaDES.core::simFile(
      name = runName,
      path = SpaDES.core::outputPath(sim),
      time = SpaDES.core::end(sim),
      ext  = "rds"   # do not use qs
    )
  dir.create(dirname(simFilename), showWarnings = FALSE, recursive = TRUE)
  elapsed <- system.time(SpaDES.core::saveSimList(
    sim      = sim,
    filename = simFilename,
    inputs   = FALSE,
    outputs  = FALSE,
    cache    = FALSE,
    files    = FALSE,
    lazy     = lazy,
    verbose  = verbose
  ))
  message("Saved sim to ", simFilename,
          " (", round(elapsed[["elapsed"]], 1), " s)")
  invisible(simFilename)
}


#' Bundle a sim file and output files into a tar.gz archive
#'
#' @description
#' Creates a `.tar.gz` archive containing `simFilename` and any additional
#' `outputFiles`.  Files that do not exist are silently skipped so a partially
#' completed simulation can still be archived.
#'
#' @inheritParams outSave
#' @param outputFiles Character vector of additional files to include (e.g.
#'   `SpaDES.core::outputs(sim)$file`).  Non-existent paths are dropped.
#'   Default `character(0)`.
#' @param tarDir Character scalar. Directory in which to create the tarball.
#'   Defaults to `dirname(simFilename)`.
#' @param verbose Logical. Pass `-v` to `tar` for file-by-file progress.
#'   Default `TRUE`.
#'
#' @return Invisibly returns the path to the created tarball.
#' @seealso [outSave()], [outUpload()], [outSaveTarUpload()]
#' @export
outTar <- function(simFilename, outputFiles = character(0), runName,
                   tarDir = dirname(simFilename), verbose = TRUE) {
  outputFiles <- outputFiles[nzchar(outputFiles) & file.exists(outputFiles)]

  ## Lazy-saved sims keep their objects -- user objects and every module's `mod`
  ## objects -- one per file in a sibling <simFilename sans ext>_lazy/ directory,
  ## with a _manifest.rds. Bundle the whole directory alongside the .rds, or the
  ## reloaded simList is a shell with nothing behind its promises.
  lazyDir   <- paste0(tools::file_path_sans_ext(simFilename), "_lazy")
  lazyFiles <- if (dir.exists(lazyDir)) dir(lazyDir, full.names = TRUE) else character(0)

  ## simFilename first: reUntar() takes the archive's first entry as the simList.
  allFiles  <- unique(c(simFilename, lazyFiles, outputFiles))
  tarball     <- file.path(tarDir, paste0(runName, ".tar.gz"))
  tar(tarball, files = allFiles,
      extra_flags = if (isTRUE(verbose)) "-v" else "")
  message("Created tarball: ", tarball)
  tarIndex(tarball)
  invisible(tarball)
}

#' Index an uncompressed tar archive without reading its members
#'
#' @description
#' Walks the 512-byte headers of an uncompressed tar by seeking from one to the
#' next, so the member bodies are never read (a 4.5 GB archive is indexed in
#' milliseconds), and writes `<runName>_index.rds` beside the tarball. That file
#' is what [reIndex()] fetches and [reGetMember()] uses to pull one member out of
#' the archive by byte range. [outTar()] calls this; use it directly for a
#' tarball made earlier.
#'
#' GNU long names (type `L`), pax headers (`x`, with `path` and `size`), ustar
#' prefixes and base-256 sizes are handled. Names are as the archive stores them
#' (tar drops the leading `/` of an absolute path). Only regular files are
#' indexed: directories and links have no bytes to fetch.
#'
#' @param tarball Path to an uncompressed tar (such as the `.tar.gz` that
#'   [outTar()] writes, which is not actually compressed).
#'
#' @return Invisibly, the path of the index file: a `data.frame` with `name`,
#'   `offset` (byte position of the member's first byte) and `size`, one row
#'   per member, saved with [base::saveRDS()].
#' @seealso [outTar()], [reIndex()], [reGetMember()]
#' @export
tarIndex <- function(tarball) {
  idx <- .tarWalk(tarball)
  out <- file.path(dirname(tarball), .indexName(basename(tarball)))
  saveRDS(idx, out)
  invisible(out)
}

## `<runName>_index.rds` for `<runName>.tar.gz`; shared by tarIndex() and reIndex().
.indexName <- function(tarName)
  paste0(tools::file_path_sans_ext(tools::file_path_sans_ext(tarName)), "_index.rds")

.tarWalk <- function(tarball) {
  con <- file(tarball, "rb")
  on.exit(close(con))
  total <- file.size(tarball)
  str <- function(b) rawToChar(b[seq_len(match(0L, c(as.integer(b), 0L)) - 1L)])
  num <- function(b) {                               # octal text, or base-256 when bit 7 is set
    if (as.integer(b[[1]]) >= 128L) {
      v <- as.integer(b); v[[1]] <- v[[1]] - 128L
      return(sum(v * 256^(rev(seq_along(v)) - 1)))
    }
    t <- trimws(str(b))
    if (nzchar(t)) strtoi(t, 8L) + 0 else 0
  }
  pax <- function(b) {                               # "len key=value\n" records
    txt <- strsplit(rawToChar(b), "\n", fixed = TRUE)[[1]]
    kv <- regmatches(txt, regexec("^[0-9]+ ([^=]+)=(.*)$", txt))
    stats::setNames(lapply(kv, `[`, 3L), vapply(kv, `[`, "", 2L))
  }
  pos <- 0; longName <- NULL; paxHdr <- list()
  name <- offset <- size <- vector("list", 0L)
  while (pos + 512 <= total) {
    seek(con, pos)
    h <- readBin(con, "raw", 512L)
    if (all(h == as.raw(0L))) break                  # end-of-archive block
    type <- if (h[157L] == as.raw(0L)) "0" else rawToChar(h[157L])   # NUL = old-style regular file
    sz <- num(h[125:136])
    body <- pos + 512
    next_pos <- body + ceiling(sz / 512) * 512
    if (type %in% c("L", "x", "g")) {
      b <- readBin(con, "raw", sz)
      if (type == "L") longName <- str(b)
      if (type == "x") paxHdr <- pax(b)
    } else {
      if (type %in% c("0", "7")) {
        nm <- if (!is.null(paxHdr$path)) paxHdr$path else if (!is.null(longName)) longName else {
          pre <- if (str(h[258:262]) == "ustar" && h[264L] == as.raw(0x30)) str(h[346:500]) else ""
          if (nzchar(pre)) paste0(pre, "/", str(h[1:100])) else str(h[1:100])
        }
        if (!is.null(paxHdr$size)) {
          sz <- as.numeric(paxHdr$size)
          next_pos <- body + ceiling(sz / 512) * 512
        }
        name[[length(name) + 1L]] <- nm
        offset[[length(offset) + 1L]] <- body
        size[[length(size) + 1L]] <- sz
      }
      longName <- NULL; paxHdr <- list()
    }
    pos <- next_pos
  }
  data.frame(name = as.character(unlist(name)), offset = as.numeric(unlist(offset)),
             size = as.numeric(unlist(size)), stringsAsFactors = FALSE)
}


#' Upload a file to Google Drive
#'
#' @description
#' Uploads a local file (typically a tarball produced by [outTar()]) to a
#' Google Drive folder via [googledrive::drive_upload()].
#'
#' @param tarball Character scalar. Path to the local file to upload.
#' @param gFolder A Google Drive folder identifier accepted by
#'   [googledrive::drive_upload()]  -- a `dribble`, a Drive URL, or a bare
#'   folder ID from [googledrive::as_id()].
#' @param overwrite Logical. Overwrite an existing file of the same name in
#'   the Drive folder.  Default `TRUE`.
#' @param cleanup Logical. Delete the local tarball after a successful upload.
#'   Default `FALSE`.
#'
#' @return Invisibly returns the `dribble` returned by
#'   [googledrive::drive_upload()] for the tarball. If `<runName>_index.rds`
#'   (from [tarIndex()]) is beside the tarball it is uploaded to the same folder.
#' @seealso [outSave()], [outTar()], [outSaveTarUpload()]
#' @export
outUpload <- function(tarball, gFolder, overwrite = TRUE, cleanup = FALSE) {
  if (is.null(gFolder))
    stop("gFolder must be supplied for Google Drive upload.", call. = FALSE)
  
  # Sometimes the tempdir() is deleted for one reason or another
  tempdir(check = TRUE)
  
  result <- googledrive::drive_upload(
    tarball,
    path      = gFolder,
    name      = basename(tarball),
    overwrite = overwrite
  )
  message("Uploaded ", basename(tarball), " to Google Drive")
  ## the index is what lets a user fetch single members of the tarball later
  idxFile <- file.path(dirname(tarball), .indexName(basename(tarball)))
  if (file.exists(idxFile)) {
    googledrive::drive_upload(idxFile, path = gFolder, name = basename(idxFile),
                              overwrite = overwrite)
    message("Uploaded ", basename(idxFile), " to Google Drive")
  }
  if (isTRUE(cleanup)) {
    unlink(c(tarball, if (file.exists(idxFile)) idxFile))
    message("Deleted local tarball: ", tarball)
  }
  invisible(result)
}


#' Save, tar, and upload a SpaDES simulation to Google Drive
#'
#' @description
#' Convenience wrapper that calls [outSave()], [outTar()], and [outUpload()]
#' in sequence.  The sim is saved to an RDS file, bundled with its output
#' files into a `.tar.gz` archive, and the archive is uploaded to a Google
#' Drive folder.
#'
#' @inheritParams outSave
#' @inheritParams outTar
#' @inheritParams outUpload
#' @param ledger Optional `reproducible::CacheGeoLedger()` whose `key` is
#'   `"runName"`. When given, after a successful upload one row is written with
#'   [reproducible::CacheGeoWrite()] (see [outLedgerRow()]), so the archive can
#'   be found by study area. The upload comes first, so a row never names a
#'   missing file. `NULL` (default) writes nothing.
#' @param area The study area (an `sf`, `sfc` or `SpatVector` object) used as
#'   the row's geometry. Required when `ledger` is given.
#' @param scenario Named list; each element becomes a column of the ledger row.
#'
#' @return Invisibly returns the `dribble` from [googledrive::drive_upload()].
#' @seealso [outSave()], [outTar()], [outUpload()], [outLedgerRow()]
#' @export
outSaveTarUpload <- function(runName, sim, gFolder = NULL, simFilename = NULL,
                              tarDir = NULL, tarball = NULL, overwrite = TRUE, cleanup = FALSE,
                              verbose = TRUE, lazy = TRUE,
                              ledger = NULL, area = NULL, scenario = list()) {
  if (!is.null(ledger)) {
    if (is.null(area))
      stop("`area` is required when a `ledger` is given.", call. = FALSE)
    if (!identical(ledger$key, "runName"))
      stop("The ledger's key must be \"runName\", not ",
           if (is.null(ledger$key)) "NULL" else encodeString(ledger$key, quote = "\""), ".",
           call. = FALSE)
  }
  t1 <- system.time(
    simFilename <- outSave(sim, runName, simFilename, lazy = lazy)
  )
  if (is.null(tarDir))
    tarDir <- dirname(simFilename)
  if (is.null(tarball))
    t2 <- system.time(
      tarball <- outTar(simFilename,
                        outputFiles = SpaDES.core::outputs(sim)$file,
                        runName     = runName,
                        tarDir      = tarDir,
                        verbose     = verbose)
    )
  t3 <- system.time(
    result <- outUpload(tarball, gFolder, overwrite = overwrite, cleanup = cleanup)
  )
  if (!is.null(ledger))
    reproducible::CacheGeoWrite(
      ledger,
      outLedgerRow(runName, driveId = result$id,
                   saveTimes = SpaDES.core::outputs(sim)$saveTime,
                   area = area, outputPath = SpaDES.core::outputPath(sim),
                   scenario = scenario))
  total <- t1[["elapsed"]] + t2[["elapsed"]] + t3[["elapsed"]]
  .fmt_elapsed <- function(s) {
    if (s < 90)        paste0(round(s, 1), " s")
    else if (s < 5400) paste0(round(s / 60, 1), " min")
    else               paste0(round(s / 3600, 2), " hr")
  }
  message("outSaveTarUpload times  -- save: ", .fmt_elapsed(t1[["elapsed"]]),
          "  tar: ",    .fmt_elapsed(t2[["elapsed"]]),
          "  upload: ", .fmt_elapsed(t3[["elapsed"]]),
          "  total: ",  .fmt_elapsed(total))
  invisible(result)
}


#' One ledger row for an uploaded archive
#'
#' @description
#' Builds the row that [outSaveTarUpload()] writes to a
#' `reproducible::CacheGeoLedger()`. Exported so a backfill can write the same
#' row for an archive that already exists, without a `simList`:
#' `reproducible::CacheGeoWrite(ledger, outLedgerRow(...))`.
#'
#' @param runName Character scalar; the ledger's key.
#' @param driveId The Google Drive id of the uploaded tarball.
#' @param saveTimes Numeric; the simulation times at which outputs were saved
#'   (`outputs(sim)$saveTime`). Stored sorted and unique, in a list-column.
#' @param area The study area: an `sf`, `sfc` or `SpatVector` object. Several
#'   features are merged into one geometry.
#' @param outputPath Character scalar; `outputPath(sim)` of the run, as an
#'   absolute path on the machine that made it. A reader uses it to build the
#'   `pathRemap` that moves the archive's members under its own folder.
#' @param scenario Named list; each element becomes a column.
#' @param uploaded Time of the upload. Default [Sys.time()].
#'
#' @return A one-row `sf` object with columns `runName`, `driveId`, `saveTimes`,
#'   `outputPath`, `uploaded`, the `scenario` columns, and the geometry.
#' @seealso [outSaveTarUpload()]
#' @export
outLedgerRow <- function(runName, driveId, saveTimes, area, outputPath,
                         scenario = list(), uploaded = Sys.time()) {
  geom <- if (inherits(area, "sf")) sf::st_geometry(area) else if (inherits(area, "sfc")) area else
    sf::st_geometry(sf::st_as_sf(area))
  row <- sf::st_sf(runName = runName, driveId = as.character(driveId),
                   outputPath = as.character(outputPath), uploaded = uploaded,
                   geometry = sf::st_union(geom))
  row$saveTimes <- list(sort(unique(saveTimes)))
  for (nm in names(scenario)) row[[nm]] <- scenario[[nm]]
  row[, c("runName", "driveId", "saveTimes", "outputPath", "uploaded", names(scenario), "geometry")]
}
