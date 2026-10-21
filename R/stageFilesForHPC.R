#' Copy the files a run used to an HPC system
#'
#' Builds one `rsync` command per folder from the files a run read (e.g. the
#' output of `reproducible::filesUsedTrace()`), so a Slurm job can run without
#' internet access (see [experimentSBATCH()]). Beside each used file, the sidecars
#' that exist are added: `CHECKSUMS.txt` in the same directory, `<file>.aux.xml`,
#' and reproducible's hash sidecars `.<basename>_*.hash`. The relative paths of
#' each folder are written to a list file and copied with
#' `rsync -aH --files-from=<list> <folder>/ <dest>/`.
#'
#' @param files A `data.frame` with columns `path` (absolute path of a used file)
#'   and `folder` (the folder it is under, a prefix of `path`).
#' @param dest Named character vector, one destination per folder: names are the
#'   folders in `files$folder`, values the `rsync` destinations, e.g.
#'   `c("/mnt/fast/inputs" = "gpsc:/project/x/inputs")`.
#' @param dryRun Logical (default `TRUE`). Add `--dry-run` to each command and
#'   only print it; `FALSE` runs the commands with [system2()] (Linux or macOS: `rsync` reads
#'   the colon in a Windows path such as `C:/...` as a remote host).
#'
#' @return Invisibly, a list with one element per folder: `folder`, `dest`,
#'   `listFile` (the `--files-from` file), `files` (its relative paths) and
#'   `command` (the `rsync` command).
#'
#' @examples
#' folder <- file.path(tempdir(), "inputs")
#' dir.create(folder, showWarnings = FALSE)
#' writeLines("1", file.path(folder, "a.csv"))
#' writeLines("x", file.path(folder, "CHECKSUMS.txt"))
#' files <- data.frame(path = file.path(folder, "a.csv"), folder = folder)
#' dest <- c("host:/project/inputs")
#' names(dest) <- folder
#' staged <- stageFilesForHPC(files, dest)   # prints the dry-run rsync command
#' readLines(staged[[1]]$listFile)           # a.csv and CHECKSUMS.txt
#'
#' @seealso [experimentSBATCH()]
#' @export
stageFilesForHPC <- function(files, dest, dryRun = TRUE) {
  stopifnot(is.data.frame(files), all(c("path", "folder") %in% names(files)),
            is.character(dest), !is.null(names(dest)))
  folders <- unique(files$folder)
  noDest <- setdiff(folders, names(dest))
  if (length(noDest))
    stop("`dest` has no destination for: ", paste(noDest, collapse = ", "), call. = FALSE)

  listDir <- tempfile("stageFilesForHPC_")
  dir.create(listDir)
  out <- lapply(folders, function(folder) {
    paths <- .withSidecars(unique(files$path[files$folder == folder]))
    rel <- as.character(fs::path_rel(paths, start = folder))
    outside <- startsWith(rel, "..")
    if (any(outside))
      stop("Not under `folder` ", folder, ": ", paste(paths[outside], collapse = ", "), call. = FALSE)
    listFile <- file.path(listDir, paste0(gsub("[^A-Za-z0-9]+", "_", folder), ".txt"))
    writeLines(rel, listFile)
    args <- c("-aH", if (dryRun) "--dry-run", paste0("--files-from=", shQuote(listFile)),
              shQuote(paste0(folder, "/")), shQuote(paste0(dest[[folder]], "/")))
    command <- paste("rsync", paste(args, collapse = " "))
    message(command)
    if (!dryRun) {
      status <- system2("rsync", args)
      if (status != 0L) stop("rsync exited with status ", status, ": ", command, call. = FALSE)
    }
    list(folder = folder, dest = dest[[folder]], listFile = listFile, files = rel, command = command)
  })
  invisible(out)
}

# The files plus the sidecars beside each that exist: CHECKSUMS.txt in the same
# directory, <file>.aux.xml, and reproducible's .<basename>_*.hash.
.withSidecars <- function(paths) {
  dirs <- dirname(paths)
  bns <- basename(paths)
  hashes <- unlist(lapply(seq_along(paths), function(i) {
    cand <- list.files(dirs[i], pattern = "\\.hash$", all.files = TRUE, full.names = TRUE)
    cand[startsWith(basename(cand), paste0(".", bns[i], "_"))]
  }))
  sidecars <- c(file.path(dirs, "CHECKSUMS.txt"), paste0(paths, ".aux.xml"), hashes)
  unique(c(paths, sidecars[file.exists(sidecars)]))
}
