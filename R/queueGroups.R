# Queue groups: one experiment*() call runs a whole campaign (fit -> hindcast -> score, say).
#
# Three optional queue columns, documented in ?experimentGroups:
#   .group      the row's group; each group can have its own workers (and, on Slurm, its
#               own #SBATCH options)
#   .after      comma-separated groups that must finish before any row of this group runs
#   .afterType  "afterok" (default: every row of those groups DONE) or "afterany" (none of
#               them still PENDING, INTERRUPTED or RUNNING)
# On Slurm a group's jobs get --dependency=<.afterType>:<job ids of the .after groups>; in
# every runner a worker does not claim a row whose .after groups have not finished.

.groupCol     <- ".group"
.afterCol     <- ".after"
.afterTypeCol <- ".afterType"
.afterTypes   <- c("afterok", "afterany")
.openStatus   <- function() c(txtPending, txtInterrupted)
.activeStatus <- function() c(txtPending, txtInterrupted, txtRunning)

## A queue column by its R name; the Google Sheet stores `.col` as `dotcol`.
.qcol <- function(q, nm) {
  x <- q[[nm]]
  if (is.null(x)) x <- q[[sub("^\\.", dotTxt, nm)]]
  x
}

## `.after` as a list of group names per row ("" and NA: none).
.afterOf <- function(q) {
  a <- .qcol(q, .afterCol)
  if (is.null(a)) return(rep(list(character()), NROW(q)))
  lapply(strsplit(ifelse(is.na(a), "", as.character(a)), ","), function(x) {
    x <- trimws(x)
    x[nzchar(x)]
  })
}

.afterTypeOf <- function(q) {
  t <- .qcol(q, .afterTypeCol)
  if (is.null(t)) return(rep(.afterTypes[1L], NROW(q)))
  t <- as.character(t)
  t[is.na(t) | !nzchar(t)] <- .afterTypes[1L]
  t
}

#' Queue groups: one call runs a whole campaign
#'
#' @description
#' [experimentSBATCH()], [experimentTmux()] and [experimentFuture()] read three optional
#' queue columns. Without them nothing changes.
#' \describe{
#'   \item{`.group`}{Character. The row's group. `n_workers` can be a named vector giving
#'     the workers of each group (`c(fit = 1, hindcast = 4)`); a worker given a group
#'     claims only that group's rows. In [experimentSBATCH()], `sbatch_opts` can be an
#'     unnamed list of options for every group followed by lists named by group, which
#'     add to or replace them: `list(list(partition = "standard"), fit = list(mem = "300G"))`.}
#'   \item{`.after`}{Character, comma-separated group names. No row of the group is
#'     claimed until those groups have finished. The same for every row of a group.}
#'   \item{`.afterType`}{`"afterok"` (default: every row of the `.after` groups is
#'     `DONE`) or `"afterany"` (none of them is `PENDING`, `INTERRUPTED` or `RUNNING`; a
#'     `HOLD` or `ERROR` row counts as finished). The same for every row of a group.}
#' }
#' The dev library (`devPackages`) is built once per call, and every group's workers
#' start on it. There is one queue file for all rows, so the status and resume tools work
#' as before.
#'
#' How `n_workers` is read when the queue has groups: a named entry gives that group's
#' workers (at most one per row still to run); the unnamed entry (or an unnamed scalar) is,
#' in [experimentSBATCH()], the number of workers of each group not named, and in
#' [experimentTmux()] and [experimentFuture()], a pool of workers that take rows of any
#' group (a pool shares one machine, a Slurm job does not). A group with rows to run and no
#' worker is an error.
#'
#' On Slurm, each group's jobs are submitted with
#' `--dependency=<.afterType>:<job ids of the .after groups>`, after those groups' jobs
#' (groups are submitted in dependency order). A worker that would wait for an unfinished
#' group checks the queue again every 30 seconds (`SPADES_AFTER_POLL_SECONDS`). A row whose
#' `"afterok"` group ended with a row not `DONE` never runs; its worker ends.
#' @name experimentGroups
#' @examples
#' \dontrun{
#' ## two ELFs: each fit, then its hindcast (one row per year), then its score
#' df <- do.call(rbind, lapply(c("6.6.2", "14.3"), function(elf) {
#'   fit <- paste0("fit_", elf); hc <- paste0("hc_", elf)
#'   rbind(
#'     data.frame(.ELFind = elf, .set = "refit", .year = NA, .group = fit, .after = "",
#'                .afterType = "afterok"),
#'     data.frame(.ELFind = elf, .set = "hindcast", .year = 2016:2020, .group = hc,
#'                .after = fit, .afterType = "afterok"),
#'     data.frame(.ELFind = elf, .set = "score", .year = NA, .group = paste0("score_", elf),
#'                .after = hc, .afterType = "afterany"))
#' }))
#' experimentSBATCH(df, n_workers = c(fit_6.6.2 = 1, fit_14.3 = 1, 4),
#'                  sbatch_opts = list(list(partition = "standard", mem = "64G"),
#'                                     fit_6.6.2 = list(mem = "320G", cpus_per_task = 42),
#'                                     fit_14.3 = list(mem = "200G", cpus_per_task = 42)))
#' }
NULL

## The queue's groups, in dependency order (a group after every group in its `.after`), or
## NULL when the queue has no `.group` column. Stops on an unknown or cyclic `.after`, or on
## rows of one group that disagree about `.after` / `.afterType`.
## Columns: group, after (list), afterType, nOpen (rows PENDING or INTERRUPTED).
.queueGroups <- function(q) {
  g <- .qcol(q, .groupCol)
  hasAfter <- !is.null(.qcol(q, .afterCol)) || !is.null(.qcol(q, .afterTypeCol))
  if (is.null(g)) {
    if (hasAfter && any(lengths(.afterOf(q)) > 0L))
      stop("The queue has a `.after` column but no `.group` column; `.after` names groups.",
           call. = FALSE)
    return(NULL)
  }
  g <- as.character(g)
  if (anyNA(g) || !all(nzchar(g)))
    stop("Every row needs a `.group` when the queue has that column.", call. = FALSE)
  after <- .afterOf(q)
  type  <- .afterTypeOf(q)
  bad <- setdiff(type, .afterTypes)
  if (length(bad))
    stop("`.afterType` must be one of ", paste(.afterTypes, collapse = ", "), ", not ",
         paste(bad, collapse = ", "), call. = FALSE)
  key <- paste(vapply(after, paste, "", collapse = ","), type)
  groups <- unique(g)
  for (gr in groups) if (length(unique(key[g == gr])) > 1L)
    stop("The rows of group '", gr, "' differ in `.after` or `.afterType`; they must agree.",
         call. = FALSE)
  first <- match(groups, g)
  afterG <- stats::setNames(after[first], groups)
  unknown <- setdiff(unlist(afterG), groups)
  if (length(unknown))
    stop("`.after` names group(s) not in the queue: ", paste(unknown, collapse = ", "), call. = FALSE)

  ord <- character()
  while (length(ord) < length(groups)) {
    ready <- setdiff(groups[vapply(afterG, function(a) all(a %in% ord), logical(1))], ord)
    if (!length(ready))
      stop("`.after` has a cycle among group(s): ", paste(setdiff(groups, ord), collapse = ", "),
           call. = FALSE)
    ord <- c(ord, ready)
  }
  open <- as.character(q$status) %in% .openStatus()
  out <- data.frame(group = ord, afterType = type[first][match(ord, groups)],
                    nOpen = vapply(ord, function(gr) sum(open & g == gr), integer(1), USE.NAMES = FALSE),
                    stringsAsFactors = FALSE)
  out$after <- unname(afterG[ord])
  out[c("group", "after", "afterType", "nOpen")]
}

## The group each worker serves, in launch order; NA: any group. See ?experimentGroups for
## how `n_workers` is read. `pool = FALSE` (Slurm): the unnamed entry is per group;
## `pool = TRUE` (tmux, future): it is that many workers for any group.
.workerGroups <- function(n_workers, groups, pool) {
  nms <- names(n_workers)
  if (is.null(nms)) nms <- rep("", length(n_workers))
  if (is.null(groups)) {
    if (any(nzchar(nms)))
      stop("`n_workers` is named, but the queue has no `.group` column.", call. = FALSE)
    return(rep(NA_character_, sum(n_workers)))
  }
  unknown <- setdiff(nms[nzchar(nms)], groups$group)
  if (length(unknown))
    stop("`n_workers` names group(s) not in the queue: ", paste(unknown, collapse = ", "), call. = FALSE)
  default <- n_workers[!nzchar(nms)]
  if (length(default) > 1L)
    stop("`n_workers` may have only one unnamed entry (the default).", call. = FALSE)
  named <- groups$group %in% nms
  per <- vapply(groups$group, function(gr)
    if (gr %in% nms) as.integer(n_workers[[gr]]) else if (!pool && length(default)) as.integer(default) else 0L,
    integer(1), USE.NAMES = FALSE)
  per <- pmin(per, groups$nOpen)
  unserved <- groups$group[groups$nOpen > 0L & !named & per == 0L]
  if (length(unserved) && !(pool && length(default) && default > 0L))
    stop("No worker for group(s) ", paste(unserved, collapse = ", "),
         ": name them in `n_workers` or give an unnamed default.", call. = FALSE)
  c(rep(groups$group, per), if (pool && length(default)) rep(NA_character_, default))
}

## The row a worker claims next: the first PENDING or INTERRUPTED row of its `group` (NULL:
## any) whose `.after` groups have finished. list(i = row or NA, wait = TRUE when no row is
## ready but one will be once a group still running finishes).
.claimableRow <- function(q, group = NULL) {
  status <- as.character(q$status)
  open <- status %in% .openStatus()
  g <- .qcol(q, .groupCol)
  mine <- open
  if (!is.null(group) && !is.null(g)) mine <- mine & as.character(g) %in% group
  cand <- which(mine)
  if (!length(cand) || is.null(g)) return(list(i = cand[1L], wait = FALSE))
  g <- as.character(g)
  after <- .afterOf(q)
  type  <- .afterTypeOf(q)
  ## for each row: "ready", "wait" or "never". A group is active while a row runs or can still
  ## run; an open row whose own `.after` groups cannot finish ("never") stays PENDING for good
  ## and is not active, or an afterany row would wait on it forever.
  rowState <- function(r) {
    s <- vapply(after[[r]], function(a) {
      rows <- which(g == a)
      active <- any(status[rows] == txtRunning) ||
        any(vapply(rows[open[rows]], function(x) rowState(x) != "never", logical(1)))
      ok <- if (identical(type[r], "afterany")) !active else all(status[rows] == txtDone)
      if (ok) "ready" else if (active) "wait" else "never"
    }, character(1))
    if (all(s == "ready")) "ready" else if (any(s == "never")) "never" else "wait"
  }
  state <- vapply(cand, rowState, character(1))
  list(i = cand[state == "ready"][1L], wait = any(state == "wait"))
}

## Seconds a worker waits before looking again for a row whose `.after` groups are running.
.afterPollSeconds <- function() as.numeric(Sys.getenv("SPADES_AFTER_POLL_SECONDS", "30"))

## A group name as it appears in worker file and Slurm job names.
.groupFileName <- function(group) gsub("[^[:alnum:]_.-]", "-", group)

## Each worker's index within its group (1, 2, ... per group; NA counts as one group).
.indexInGroup <- function(groups)
  stats::ave(seq_along(groups), ifelse(is.na(groups), "\r", groups), FUN = seq_along)

## Each worker's file in `log_dir` (log, stop file, script): worker_<group>_01<ext>, or
## worker_01<ext> for a worker of no group (the names used before groups).
.workerFiles <- function(log_dir, worker_groups, ext) {
  idx <- .indexInGroup(worker_groups)
  file.path(log_dir, paste0(ifelse(is.na(worker_groups), sprintf("worker_%02d", idx),
                                   sprintf("worker_%s_%02d", .groupFileName(worker_groups), idx)), ext))
}

## The `group` argument of a generated worker call: "" for no group (the call as it was
## before groups), else `fmt` with the deparsed group.
.groupArg <- function(group, fmt)
  if (length(group) && !is.na(group)) sprintf(fmt, deparse1(group)) else ""
