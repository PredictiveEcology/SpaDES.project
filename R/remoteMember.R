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
  googledrive::drive_download(hits[1, ], path = f, overwrite = TRUE, verbose = FALSE)
  readRDS(f)
}

#' @param member Character. The archive member to fetch. Matched exactly, or as a
#'   regular expression when no exact match exists (it must then match one row).
#' @param index Optional `data.frame` from [reIndex()]. Fetched if not supplied;
#'   pass it when reading several members so the index is downloaded once.
#' @param file Optional path to write the raw member to. When `NULL` (default)
#'   the member is read back with [readRDS()] and returned.
#'
#' @return `reGetMember()`: the deserialized object, or the path written when
#'   `file` is supplied.
#'
#' @rdname reIndex
#' @export
reGetMember <- function(gFile, member, index = NULL, file = NULL) {
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
  resp <- httr::GET(url, googledrive::drive_token(),
                    httr::add_headers(Range = sprintf("bytes=%s-%s",
                                                      format(from, scientific = FALSE),
                                                      format(to, scientific = FALSE))))
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
