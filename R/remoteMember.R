#' Read one member out of a remote tar archive, without downloading it
#'
#' @description
#' [outTar()] writes an *uncompressed* tar (R's [utils::tar()] defaults to
#' `compression = "none"`), so every member sits at a known byte offset. Google
#' Drive answers HTTP `Range` requests with `206 Partial Content`, so a single
#' object can be pulled out of a multi-GB archive by fetching only its bytes.
#'
#' For a lazily saved `simList` this is the difference between downloading the
#' whole tarball and reading the one object you want: measured on a 4.63 GB
#' archive, one 6.2 MB object took **0.83 s** against ~304 s for the whole file.
#'
#' [reIndex()] fetches the `<runName>_index.rds` written beside the tarball;
#' [reGetMember()] uses it to fetch a single member.
#'
#' @param gFile A Google Drive `dribble` row, file id, or URL for the **tarball**.
#' @param destDir Directory for the downloaded index. Default [tempdir()].
#'
#' @return `reIndex()`: a `data.frame` with `name`, `offset` and `size`, one row
#'   per archive member.
#'
#' @seealso [reGetMember()], [outTar()], [reGetUntarLoad()]
#' @export
reIndex <- function(gFile, destDir = tempdir()) {
  reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
  d <- .driveFile(gFile)
  idxName <- paste0(tools::file_path_sans_ext(tools::file_path_sans_ext(d$name)), "_index.rds")
  parent <- googledrive::drive_reveal(d, "parent")$id_parent[[1]]
  hits <- googledrive::drive_ls(googledrive::as_id(parent), pattern = idxName, n_max = Inf)
  if (!NROW(hits))
    stop("No index found for ", d$name, "; expected '", idxName,
         "' beside it. Only archives written with an index can be read this way.",
         call. = FALSE)
  f <- file.path(destDir, idxName)
  ## `verbose` is deprecated in googledrive >= 2.0; quiet it locally instead
  googledrive::with_drive_quiet(
    googledrive::drive_download(hits[1, ], path = f, overwrite = TRUE))
  readRDS(f)
}

#' @param member Character. The archive member to fetch. Matched exactly, or as a
#'   regular expression when no exact match exists (it must then match one row).
#' @param index Optional `data.frame` from [reIndex()]. Fetched if not supplied;
#'   pass it when reading several members so the index is downloaded once.
#' @param file Optional path to write the raw member to. When `NULL` (default)
#'   the member is read back with [readRDS()] and returned.
#' @param progress Logical or `NULL`. Show a transfer progress bar. `NULL`
#'   (default) shows one in an interactive session for members over ~5 MB,
#'   where the wait is long enough to look like a hang.
#'
#' @return `reGetMember()`: the deserialized object, or the path written when
#'   `file` is supplied.
#'
#' @rdname reIndex
#' @export
reGetMember <- function(gFile, member, index = NULL, file = NULL, progress = NULL) {
  reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
  reproducible::.requireNamespace("httr", stopOnFALSE = TRUE)
  d <- .driveFile(gFile)
  if (is.null(index)) index <- reIndex(gFile)

  i <- which(index$name == member)
  if (!length(i)) i <- grep(member, index$name)
  if (!length(i)) stop("No archive member matching '", member, "'.", call. = FALSE)
  if (length(i) > 1L)
    stop("'", member, "' matches ", length(i), " members: ",
         paste(utils::head(basename(index$name[i]), 5), collapse = ", "), call. = FALSE)

  ## Byte range for exactly this member. -1 because Range is inclusive.
  from <- index$offset[[i]]
  to <- from + index$size[[i]] - 1
  url <- paste0("https://www.googleapis.com/drive/v3/files/", d$id, "?alt=media")
  ## A large member can take tens of seconds. Show a progress bar when there is
  ## someone to see it, so the wait is visibly a transfer rather than a hang.
  prog <- if (isTRUE(progress) ||
              (is.null(progress) && interactive() && index$size[[i]] > 5e6))
    httr::progress() else NULL
  resp <- httr::GET(url, googledrive::drive_token(),
                    httr::add_headers(Range = sprintf("bytes=%s-%s",
                                                      format(from, scientific = FALSE),
                                                      format(to, scientific = FALSE))),
                    prog)
  code <- httr::status_code(resp)
  ## 206 = the server honoured the range. 200 means it ignored it and is sending
  ## the whole file, which would be silently wrong as well as enormous.
  if (!identical(code, 206L))
    stop("Expected HTTP 206 (partial content), got ", code,
         ifelse(identical(code, 200L), ": the server ignored the Range request.", ""),
         call. = FALSE)
  bytes <- httr::content(resp, "raw")
  if (length(bytes) != index$size[[i]])
    stop("Short read: got ", length(bytes), " of ", index$size[[i]], " bytes.", call. = FALSE)

  out <- if (is.null(file)) tempfile(fileext = ".rds") else file
  writeBin(bytes, out)
  ## Written to a file rather than decompressed in memory so any compression
  ## saveRDS() used (gzip, bzip2, xz, none) is handled by readRDS() itself.
  if (is.null(file)) readRDS(out) else out
}

## A one-row dribble for a dribble / id / URL.
.driveFile <- function(gFile) {
  if (inherits(gFile, "dribble")) {
    if (NROW(gFile) != 1L) stop("gFile must identify exactly one file.", call. = FALSE)
    return(gFile)
  }
  googledrive::drive_get(googledrive::as_id(as.character(gFile)))
}

## ---------------------------------------------------------------------------
## Remote lazy loading: objects stay on the remote and arrive on first access;
## output files are downloaded, as they always were.
## ---------------------------------------------------------------------------

## Where an archive member should land locally, applying the same prefix remap
## reUntar() would. Members are stored without the leading "/".
.localPathFor <- function(member, pathRemap) {
  p <- paste0("/", sub("^/", "", member))
  if (!is.null(pathRemap)) p <- sub(paste0("^", path.expand(pathRemap[["old"]])),
                                    path.expand(pathRemap[["new"]]), p)
  p
}

## Load one simList whose objects stay remote. The shell, the manifest and the
## output files are fetched now -- the outputs because they are read anyway, and
## because `outputs(sim)$file` must point at real paths for downstream code.
## Every sidecar object is left to `fetch`, so it moves only if touched.
.reLoadRemoteOne <- function(gFile, pathRemap = NULL, projectPath = getwd(),
                             parse = TRUE, verbose = TRUE) {
  idx <- reIndex(gFile)
  ## outTar() writes the simList first, and reUntar() relies on that too.
  ## Derive the sidecar directory from the shell's own name, exactly as
  ## loadSimList() does. A looser pattern such as "[^/]+_lazy/" is wrong: the
  ## shell's PARENT directory is itself often named `<rep>_lazy`, so the shell
  ## gets misclassified as one of its own sidecar objects.
  shellIx <- 1L
  if (!grepl("\\.rds$", idx$name[shellIx]))
    stop("First archive member is not the simList: ", basename(idx$name[shellIx]),
         call. = FALSE)
  lazyDirName <- paste0(tools::file_path_sans_ext(basename(idx$name[shellIx])), "_lazy")
  isSide <- grepl(paste0("(^|/)", lazyDirName, "/"), idx$name)
  isMan  <- isSide & basename(idx$name) == "_manifest.rds"
  if (!any(isMan))
    stop("No lazy manifest in the archive; it was not saved with lazy = TRUE.",
         call. = FALSE)

  ## shell + manifest + every output file
  eager <- which(!isSide) 
  eager <- union(eager, which(isMan))
  for (i in eager) {
    dest <- .localPathFor(idx$name[i], pathRemap)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(dest) || file.size(dest) != idx$size[i])
      reGetMember(gFile, idx$name[i], index = idx, file = dest)
  }
  shell <- .localPathFor(idx$name[shellIx], pathRemap)
  if (isTRUE(verbose))
    message("  fetched shell + manifest + ", sum(!isSide) - 1L, " output file(s); ",
            sum(isSide) - sum(isMan), " object(s) left remote")

  ## Objects: `fetch` is handed the path loadSimList expects; find the member
  ## with that basename among the sidecar entries and range-fetch it there.
  sideNames <- idx$name[isSide]
  fetch <- function(path) {
    m <- sideNames[basename(sideNames) == basename(path)]
    if (length(m) != 1L)
      stop("Cannot resolve '", basename(path), "' in the remote archive.", call. = FALSE)
    sz <- idx$size[idx$name == m][[1L]]
    ## Announce BEFORE transferring: a promise forced deep inside other code
    ## otherwise looks like the session has hung. The object name is the one
    ## the user asked for, so they can see what they touched and how big it is.
    say <- isTRUE(getOption("SpaDES.project.remoteVerbose", TRUE))
    obj <- sub("^[0-9]+-", "", tools::file_path_sans_ext(basename(path)))
    if (say)
      message("fetching '", obj, "' (", .fmtBytes(sz), ") from the remote archive ...")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    el <- system.time(reGetMember(gFile, m, index = idx, file = path))[["elapsed"]]
    if (say)
      message("  ... got '", obj, "' in ", round(el, 1), " s (",
              .fmtBytes(sz / max(el, 1e-9)), "/s)")
    invisible(path)
  }
  args <- list(shell, projectPath = projectPath, fetch = fetch)
  if ("parse" %in% names(formals(SpaDES.core::loadSimList)))
    args$parse <- parse
  do.call(SpaDES.core::loadSimList, args)
}

.fmtBytes <- function(b) {
  if (is.na(b)) return("?")
  u <- c("B", "KB", "MB", "GB"); i <- max(1L, min(length(u), floor(log(max(b, 1), 1024)) + 1L))
  paste0(round(b / 1024^(i - 1L), 1), " ", u[[i]])
}
