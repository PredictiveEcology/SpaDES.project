#' Download tarballs from Google Drive
#'
#' @description
#' Inverse of [outUpload()]. Downloads one or more tar.gz archives from a
#' Google Drive folder to a local directory, using
#' [reproducible::preProcess()] (so re-runs hit the local copy when present).
#' Vectorised: typically called with the multi-row `dribble` returned by
#' [outList()] / [outScenarios()].
#'
#' @param gFiles Either a Google Drive `dribble` (e.g. the output of
#'   [outList()] / [outScenarios()]) or a character vector of Drive file IDs
#'   or URLs.
#' @param destDir Character scalar. Local directory to write tarballs into.
#'   Created if it does not exist.
#' @param overwrite Logical. Force re-download even if the local file
#'   exists. Default `FALSE`.
#' @param verbose Logical. Print elapsed time per download. Default `TRUE`.
#'
#' @return A `data.table` with columns `name` and `local_path`, one row per
#'   downloaded file.
#' @seealso [reUntar()], [reLoad()], [reGetUntarLoad()], [outUpload()]
#' @export
reGet <- function(gFiles, destDir, overwrite = FALSE, verbose = TRUE) {
  reproducible::.requireNamespace("reproducible", stopOnFALSE = TRUE)
  dir.create(destDir, showWarnings = FALSE, recursive = TRUE)

  if (inherits(gFiles, "dribble")) {
    ids   <- gFiles$id
    names <- gFiles$name
  } else {
    ids   <- as.character(gFiles)
    names <- rep(NA_character_, length(ids))
  }

  if (isTRUE(verbose))
    message("Downloading ", length(ids), " archive(s) to ", destDir)

  rows <- lapply(seq_along(ids), function(i) {
    elapsed <- system.time({
      out <- reproducible::preProcess(
        url             = ids[[i]],
        archive         = NA,
        fun             = NA,
        destinationPath = destDir,
        overwrite       = overwrite
      )
    })
    nm <- if (is.na(names[[i]])) basename(out$targetFilePath) else names[[i]]
    if (isTRUE(verbose))
      message("  downloaded ", nm, " (", round(elapsed[["elapsed"]], 1), " s)")
    data.table::data.table(name = nm, local_path = out$targetFilePath)
  })
  data.table::rbindlist(rows)
}


#' Extract sim tarballs, optionally remapping a path prefix
#'
#' @description
#' Inverse of [outTar()]. Extracts one or more `.tar.gz` archives produced
#' by [outTar()] / [outSaveTarUpload()], which contain absolute paths. If
#' `pathRemap` is supplied, the leading path prefix is rewritten on
#' extraction (handy when the archive was created on another user's
#' machine, e.g. paths starting with `/home/emcintir/...`).
#'
#' Path rewriting uses GNU tar's `--transform`. On systems without GNU tar,
#' supply `pathRemap = NULL` and the archive's absolute paths are restored
#' as-is.
#'
#' @param tarballs Character vector of paths to local tarballs.
#' @param pathRemap Optional named character vector of length 2,
#'   `c(old = "/old/prefix", new = "/new/prefix")`, applied to all
#'   `tarballs`. If `NULL` (default), files are extracted to their original
#'   absolute paths (`tar --absolute-names`).
#' @param verbose Logical. Pass `-v` to `tar`. Default `FALSE`.
#'
#' @return A character vector (same length as `tarballs`) of absolute paths
#'   to the `.rds` simList file inside each archive (after any remap),
#'   suitable for [reLoad()].
#' @seealso [reGet()], [reLoad()], [reGetUntarLoad()], [outTar()]
#' @export
reUntar <- function(tarballs, pathRemap = NULL, verbose = FALSE) {
  if (!is.null(pathRemap)) {
    if (length(pathRemap) != 2L ||
        is.null(names(pathRemap)) ||
        !all(c("old", "new") %in% names(pathRemap)))
      stop("pathRemap must be a named character vector: c(old = ..., new = ...)",
           call. = FALSE)
    pathRemap[["old"]] <- path.expand(pathRemap[["old"]])
    pathRemap[["new"]] <- path.expand(pathRemap[["new"]])
  }
  vflag <- if (isTRUE(verbose)) "-v" else ""

  vapply(tarballs, function(tarball) {
    stopifnot(file.exists(tarball))
    ## -P (--absolute-names) only so GNU tar has nothing to strip: without it it
    ## writes "Removing leading `/' from member names" to stderr for every
    ## archive, which cannot be caught by suppressMessages() and says nothing
    ## useful. The returned member names are identical either way.
    entries <- utils::untar(tarball, list = TRUE, extras = "-P")
    if (!length(entries))
      stop("Tarball is empty: ", tarball, call. = FALSE)
    simEntry <- entries[[1L]]

    if (is.null(pathRemap)) {
      extras  <- paste("--absolute-names", vflag)
      simPath <- simEntry
    } else {
      old <- pathRemap[["old"]]
      new <- pathRemap[["new"]]
      extras <- sprintf("--absolute-names --transform=%s %s",
                        shQuote(sprintf("s|^%s|%s|", old, new)), vflag)
      simPath <- sub(paste0("^", old), new, simEntry)
      dir.create(dirname(simPath), showWarnings = FALSE, recursive = TRUE)
    }

    status <- utils::untar(tarball, extras = extras)
    if (!identical(status, 0L))
      stop("untar failed for ", tarball, " (status ", status, ")", call. = FALSE)
    message("untarred ", basename(tarball))
    simPath
  }, character(1L), USE.NAMES = FALSE)
}


#' Load saved SpaDES simLists
#'
#' @description
#' Inverse of [outSave()]. Loads one or more `simList`s from `.rds` files
#' produced by [outSave()]. Defaults to [SpaDES.core::loadSimList()];
#' set `method = "readRDS"` to bypass `.unwrap` entirely.
#'
#' Note that [SpaDES.core::saveSimList()] uses `.wrapResiliently` to NULL
#' out file-backed objects with inaccessible backing files at save time.
#' Load-time failures (e.g. backing files missing on this machine even
#' though they were present at save time) are independent of that, and are
#' handled by `loadSimList`'s pre-`.unwrap` resilient pass.
#'
#' @param simFilenames Character vector of paths to `.rds` files.
#' @param projectPath Character scalar. Passed to
#'   [SpaDES.core::loadSimList()] for relative-path resolution. Default
#'   `getwd()`.
#' @param remote Logical. If `FALSE` (default), the archive is downloaded and
#'   untarred as usual, and its objects are lazily read from the local sidecar.
#'   If `TRUE`, the archive is never downloaded: the `simList` shell, its
#'   manifest and the output files are fetched from within the remote archive by
#'   HTTP range request, and each object is fetched only when something touches
#'   it. Output files are fetched either way, since downstream code reads them
#'   through `outputs(sim)$file` and they are mostly needed. Requires an
#'   archive with an index beside it (see [reIndex()]) and
#'   `method = "loadSimList"`.
#' @param parse Logical. If `TRUE` (default), module source code is re-parsed
#'   on load. `FALSE` forwards `parse = FALSE` to
#'   [SpaDES.core::loadSimList()], which skips it -- worthwhile because
#'   reparsing dominates the load time of a lazily saved `simList` (on a
#'   19-module simulation, ~7 s of a ~9.5 s load). Objects are unaffected:
#'   user objects and each module's `mod` objects are still bound lazily. The
#'   result is inspect-only -- it has no module code, so it cannot be passed
#'   to [SpaDES.core::spades()]. Ignored when `method = "readRDS"`.
#' @param method One of `"loadSimList"` (default) or `"readRDS"`.
#' @param ... Additional args forwarded to [SpaDES.core::loadSimList()]
#'   (ignored when `method = "readRDS"`).
#'
#' @return A list of `simList` objects, named by `basename(simFilenames)`.
#' @seealso [reGet()], [reUntar()], [reGetUntarLoad()], [outSave()]
#' @export
reLoad <- function(simFilenames, projectPath = getwd(),
                   method = c("loadSimList", "readRDS"), parse = TRUE, ...) {
  method <- match.arg(method)
  if (method == "loadSimList")
    reproducible::.requireNamespace("SpaDES.core", stopOnFALSE = TRUE)

  ## `parse` is newer than some installed SpaDES.core versions; passing it to
  ## one that lacks it is an "unused argument" error. Only forward it when the
  ## installed loadSimList() actually accepts it, and say so if it cannot.
  extra <- list()
  if (method == "loadSimList" && !isTRUE(parse)) {
    if ("parse" %in% names(formals(SpaDES.core::loadSimList))) {
      extra <- list(parse = FALSE)
    } else {
      warning("parse = FALSE needs a SpaDES.core whose loadSimList() has a ",
              "`parse` argument; module code will be re-parsed as usual.",
              call. = FALSE)
    }
  }

  sims <- lapply(simFilenames, function(f) {
    stopifnot(file.exists(f))
    elapsed <- system.time({
      sim <- if (method == "loadSimList")
        do.call(SpaDES.core::loadSimList,
                c(list(f, projectPath = projectPath), extra, list(...)))
      else
        readRDS(f)
    })
    message("loaded ", basename(f),
            " via ", method, " (", round(elapsed[["elapsed"]], 1), " s)")
    sim
  })
  names(sims) <- basename(simFilenames)
  sims
}


#' Download, untar, and load SpaDES sims from Google Drive
#'
#' @description
#' Convenience wrapper around [reGet()], [reUntar()], and [reLoad()] -- the
#' inverse of [outSaveTarUpload()]. Operates on a batch: typically called
#' with the multi-row `dribble` returned by [outList()] / [outScenarios()].
#'
#' @inheritParams reGet
#' @inheritParams reUntar
#' @inheritParams reLoad
#'
#' @return A named list of `simList` objects, one per row of `gFiles`,
#'   named by the archive's `name` (sans `.tar.gz`).
#' @seealso [reGet()], [reUntar()], [reLoad()], [outSaveTarUpload()]
#' @export

reGetUntarLoad <- function(gFiles, destDir, pathRemap = NULL,
                           projectPath = getwd(),
                           method = c("loadSimList", "readRDS"),
                           parse = TRUE, remote = FALSE,
                           overwrite = FALSE, verbose = TRUE) {
  method <- match.arg(method)

  ## Remote: never download the archive. Fetch the shell, the manifest and the
  ## output files; leave every object to be range-fetched on first access.
  if (isTRUE(remote)) {
    if (!identical(method, "loadSimList"))
      stop("remote = TRUE requires method = 'loadSimList'.", call. = FALSE)
    gf <- if (inherits(gFiles, "dribble")) gFiles else googledrive::as_id(as.character(gFiles))
    nms <- if (inherits(gFiles, "dribble")) gFiles$name else rep(NA_character_, length(gf))
    t0 <- system.time(
      sims <- lapply(seq_along(nms), function(i) {
        g <- if (inherits(gFiles, "dribble")) gFiles[i, ] else gf[[i]]
        if (isTRUE(verbose)) message("remote: ", nms[[i]])
        .reLoadRemoteOne(g, pathRemap = pathRemap, projectPath = projectPath,
                         parse = parse, verbose = verbose)
      })
    )
    names(sims) <- sub("\\.tar\\.gz$", "", nms)
    sims <- .remapOutputs(sims, pathRemap)
    if (isTRUE(verbose))
      message("reGetUntarLoad (remote) -- total: ", .fmt_elapsed(t0[["elapsed"]]))
    return(sims)
  }

  t1 <- system.time(
    files <- reGet(gFiles, destDir, overwrite = overwrite, verbose = verbose)
  )
  t2 <- system.time(
    simPaths <- reUntar(files$local_path, pathRemap = pathRemap, verbose = FALSE)
  )
  t3 <- system.time(
    sims <- reLoad(simPaths, projectPath = projectPath, method = method,
                   parse = parse)
  )
  sims <- .remapOutputs(sims, pathRemap)
  names(sims) <- sub("\\.tar\\.gz$", "", files$name)

  total <- t1[["elapsed"]] + t2[["elapsed"]] + t3[["elapsed"]]
  message("reGetUntarLoad times -- get: ",   .fmt_elapsed(t1[["elapsed"]]),
          "  untar: ",  .fmt_elapsed(t2[["elapsed"]]),
          "  load: ",   .fmt_elapsed(t3[["elapsed"]]),
          "  total: ",  .fmt_elapsed(total))
  sims
}

## outputs(sim)$file records the paths from where the sim was written; after a
## remapped extraction they must be rewritten to match, or downstream code that
## reads those files (e.g. the *_summary modules) looks in the wrong place.
.remapOutputs <- function(sims, pathRemap) {
  if (is.null(pathRemap)) return(sims)
  old <- path.expand(pathRemap[["old"]]); new <- path.expand(pathRemap[["new"]])
  for (i in seq_along(sims)) {
    out <- SpaDES.core::outputs(sims[[i]])
    if (NROW(out) && "file" %in% names(out)) {
      out$file <- sub(paste0("^", old), new, out$file)
      SpaDES.core::outputs(sims[[i]]) <- out
    }
  }
  sims
}

.fmt_elapsed <- function(s) {
  if (s < 90) paste0(round(s, 1), " s")
  else if (s < 5400) paste0(round(s / 60, 1), " min")
  else paste0(round(s / 3600, 2), " hr")
}
