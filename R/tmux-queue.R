#' Initialize a file-backed queue from a data.frame
#' @description Mirrors `df` into a queue RDS and adds status columns:
#' `status`, `claimed_by`, `started_at`, `finished_at`.
#' @param df data.frame
#' @param queue_path character path to `.rds`
#' @return Invisibly returns `queue_path`.
#' @export
tmux_prepare_queue_from_df <- function(df, queue_path) {
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