## File-descriptor diagnostics ------------------------------------------------
##
## R's socket layer uses select(2), whose FD_SETSIZE is compiled at 1024 on
## Linux/macOS. Once a process holds an fd numbered >= 1024, any new socket
## allocated above that number fails with
##   "file descriptor is too large for select()"
## which is most visible when parallelly::makeClusterPSOCK() tries to spawn.
##
## Long-running SpaDES sessions tend to hit this because terra leaves GDAL
## handles open for scratch tifs in its tmp dir even after the source files are
## unlinked, and reproducible::Cache replays many tile-loading calls in a row.
## Raising `ulimit -n` does NOT help -- it raises RLIMIT_NOFILE, not FD_SETSIZE.
## The fix is either restart R, build the parallel cluster before fds climb, or
## (upstream) wait for R to migrate from select() to poll().
##
## These helpers let setupProject (or any caller) inspect the open-fd state and
## see which holders -- terra scratch, sockets, sqlite, etc. -- are eating slots.

#' FD_SETSIZE on this platform (the `select()` ceiling).
#'
#' Hard-coded to 1024 -- the value compiled into glibc and used by R's socket
#' layer on Linux, macOS, and most BSDs. Not user-configurable without
#' rebuilding R.
#'
#' @return integer scalar.
#' @export
fdSelectLimit <- function() 1024L

#' Inspect open file descriptors.
#'
#' On Linux, reads `/proc/self/fd` and resolves each symlink to identify what
#' the fd points to. Returns a `data.frame` with one row per open fd; useful
#' for diagnosing the cryptic `"file descriptor is too large for select()"`
#' failure from [parallelly::makeClusterPSOCK()].
#'
#' Returns an empty `data.frame` on non-Linux systems or if `/proc/self/fd` is
#' unreadable.
#'
#' @return `data.frame` with columns `fd` (integer), `target` (character, with
#'   any Linux ` (deleted)` suffix preserved), and `bucket` (character holder
#'   category: `"socket"`, `"pipe"`, `"terra scratch"`,
#'   `"terra scratch (deleted)"`, `"tif (other)"`, `"vrt"`, `"sqlite"`,
#'   `"qs/qs2"`, `"anon_inode"`, `"other file"`, `"unknown"`).
#'
#' @seealso [openFdsReport()] for a printable summary; [fdSelectLimit()].
#' @export
openFds <- function() {
  fdDir <- "/proc/self/fd"
  empty <- data.frame(
    fd = integer(),
    target = character(),
    bucket = character(),
    stringsAsFactors = FALSE
  )
  if (!dir.exists(fdDir)) {
    return(empty)
  }

  fds <- suppressWarnings(as.integer(list.files(fdDir)))
  fds <- sort(fds[!is.na(fds)])
  if (length(fds) == 0L) {
    return(empty)
  }

  targets <- vapply(
    fds,
    function(fd) {
      tryCatch(Sys.readlink(file.path(fdDir, as.character(fd))), error = function(e) "?")
    },
    character(1)
  )

  data.frame(
    fd = fds,
    target = targets,
    bucket = .classifyFds(targets),
    stringsAsFactors = FALSE
  )
}

#' Printable summary of open file descriptors.
#'
#' Wraps [openFds()] and returns a multi-line string suitable for warning or
#' error messages. By default summarizes only fds at or above
#' [fdSelectLimit()] (the `select()` failure threshold); pass `threshold = 0L`
#' to summarize everything.
#'
#' @param threshold integer. Only fds at or above this number are bucketed.
#'   Defaults to [fdSelectLimit()].
#'
#' @return character scalar; `""` if `/proc/self/fd` is not available.
#'
#' @examples
#' cat(openFdsReport())
#' cat(openFdsReport(threshold = 0L))
#' @export
openFdsReport <- function(threshold = fdSelectLimit()) {
  df <- openFds()
  if (nrow(df) == 0L) {
    return("")
  }

  high <- df[df$fd >= threshold, , drop = FALSE]
  summary <- sprintf(
    "Open fds: %d total, max = %d, %d at or above threshold (%d).",
    nrow(df),
    max(df$fd),
    nrow(high),
    threshold
  )
  if (nrow(high) == 0L) {
    return(paste0(summary, "\n"))
  }

  tbl <- sort(table(high$bucket), decreasing = TRUE)
  samples <- vapply(
    names(tbl),
    function(b) high$target[high$bucket == b][1L],
    character(1)
  )
  rows <- sprintf("  %-25s %5d   e.g. %s", names(tbl), as.integer(tbl), samples)
  paste0(
    summary,
    "\nHigh-fd holders (count, example):\n",
    paste(rows, collapse = "\n"),
    "\n"
  )
}

## Classify /proc/self/fd symlink targets into holder-type buckets.
##
## - targets : character vector of Sys.readlink() results, with any Linux
##             " (deleted)" suffix preserved.
## Returns a character vector of bucket labels, parallel to targets.
.classifyFds <- function(targets) {
  bare <- sub(" \\(deleted\\)$", "", targets)
  deleted <- grepl(" \\(deleted\\)$", targets)

  vapply(seq_along(targets), function(i) {
    t <- bare[[i]]
    if (grepl("^socket:\\[", t)) {
      "socket"
    } else if (grepl("^pipe:\\[", t)) {
      "pipe"
    } else if (grepl("^anon_inode:", t)) {
      "anon_inode"
    } else if (grepl("/terra/spat_.*\\.tif$", t)) {
      if (deleted[[i]]) "terra scratch (deleted)" else "terra scratch"
    } else if (grepl("\\.tif$", t, ignore.case = TRUE)) {
      "tif (other)"
    } else if (grepl("\\.vrt$", t, ignore.case = TRUE)) {
      "vrt"
    } else if (grepl("\\.sqlite($|-wal$|-shm$)|\\.db($|-wal$|-shm$)", t, ignore.case = TRUE)) {
      "sqlite"
    } else if (grepl("\\.qs2?$", t, ignore.case = TRUE)) {
      "qs/qs2"
    } else if (grepl("^/", t)) {
      "other file"
    } else {
      "unknown"
    }
  }, character(1))
}
