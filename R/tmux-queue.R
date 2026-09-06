#' Initialize a file-backed queue from a data.frame
#' @description Mirrors `df` into a queue RDS and adds status columns:
#' `status`, `claimed_by`, `started_at`, `finished_at`.
#' @param df data.frame
#' @param queue_path character path to `.rds`
#' @return Invisibly returns `queue_path`.
#' @export
tmuxPrepareQueueFromDF <- function(df, queue_path) {
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1)
  q <- cbind(
    df,
    status      = "PENDING",
    claimed_by  = NA_character_,
    started_at  = as.character(NA),
    finished_at = as.character(NA)
  )
  saveRDS(q, queue_path)
   invisible(queue_path)
}
#' Reconcile an existing file-backed queue with a freshly built `df`
#'
#' @description
#' [experimentTmux()] materialises the queue from `df` only when `queue_path`
#' does not yet exist; an existing queue is authoritative, so a resumed run keeps
#' its `DONE`/`RUNNING` state instead of restarting finished work. That rule is
#' right, but it used to be applied *silently*: a caller who had carefully rebuilt
#' `df` -- new scenarios added, finished ones dropped -- would watch the previous
#' queue run instead, with nothing to distinguish that from success. The only
#' way to act on the new `df` was to notice, and delete or rename the file.
#'
#' This keeps the rule and makes it visible, and adds the middle option that was
#' missing: take the existing queue *and* the rows `df` adds.
#'
#' Rows are compared on the non-`meta_cols`, non-list columns -- the scenario
#' fields. List columns (`.modules`, `.times`) are payload, not identity.
#'
#' @param df data.frame of runs, as passed to [experimentTmux()].
#' @param queue_path Character path to the queue `.rds`.
#' @param onExistingQueue What to do when `queue_path` already exists:
#'   * `"resume"` (default) -- keep the existing queue; warn if `df` holds rows it
#'     does not, naming them and saying how to act on them;
#'   * `"append"` -- add those rows as `PENDING`, leaving existing rows and their
#'     status untouched;
#'   * `"rebuild"` -- discard the existing queue and start again from `df`.
#' @return Invisibly, `queue_path`.
#' @export
tmuxReconcileQueueWithDF <- function(df, queue_path,
                                     onExistingQueue = c("resume", "append", "rebuild")) {
  onExistingQueue <- match.arg(onExistingQueue)
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1L)

  if (!file.exists(queue_path) || identical(onExistingQueue, "rebuild")) {
    if (file.exists(queue_path))
      message("onExistingQueue = 'rebuild': discarding the existing queue at ",
              queue_path, " and starting from `df`.")
    tmuxPrepareQueueFromDF(df, queue_path)
    return(invisible(queue_path))
  }

  q <- readRDS(queue_path)

  ## Identity is the scenario fields only.
  idCols <- function(x)
    setdiff(names(x)[!vapply(x, is.list, logical(1))], meta_cols)
  qCols  <- idCols(q)
  dfCols <- idCols(df)

  shared <- intersect(qCols, dfCols)
  if (!length(shared)) {
    warning("The existing queue at ", queue_path, " shares no scenario columns with `df` ",
            "(queue: ", paste(qCols, collapse = ", "), "; df: ", paste(dfCols, collapse = ", "),
            "). Using the existing queue unchanged; pass onExistingQueue = 'rebuild' if `df` ",
            "is the one you want.", call. = FALSE)
    return(invisible(queue_path))
  }

  keyOf <- function(x)
    do.call(paste, c(lapply(shared, function(c1) as.character(x[[c1]])), sep = "\r"))
  isNew <- !keyOf(df) %in% keyOf(q)

  if (!any(isNew)) {
    message("The existing queue at ", basename(queue_path), " already covers every row in `df` (",
            NROW(q), " rows); resuming it.")
    return(invisible(queue_path))
  }

  newRows <- df[isNew, , drop = FALSE]
  label <- paste(utils::head(keyOf(newRows), 10L), collapse = ", ")
  if (sum(isNew) > 10L) label <- paste0(label, ", ... (", sum(isNew), " total)")

  if (identical(onExistingQueue, "append")) {
    qNew <- tmuxPrepareQueueFromDF(newRows, tempfile(fileext = ".rds"))
    added <- readRDS(qNew); unlink(qNew)
    ## rbind tolerantly: the existing queue may carry meta columns this one lacks.
    saveRDS(data.table::rbindlist(list(q, added), fill = TRUE, use.names = TRUE), queue_path)
    message("Appended ", sum(isNew), " row(s) from `df` to the existing queue at ",
            basename(queue_path), ": ", label)
    return(invisible(queue_path))
  }

  warning("The queue at ", queue_path, " already exists, so it is being used and `df` is ",
          "ignored -- this preserves DONE/RUNNING state on a resume, which is usually what ",
          "you want.\n  Existing queue: ", NROW(q), " row(s). `df`: ", NROW(df), " row(s), of ",
          "which ", sum(isNew), " are not in the queue: ", label,
          "\n  To act on them, call experimentTmux() with one of:",
          "\n    onExistingQueue = 'append'  -- add just those rows, keeping existing status",
          "\n    onExistingQueue = 'rebuild' -- start over from `df`, discarding existing status",
          call. = FALSE)
  invisible(queue_path)
}
