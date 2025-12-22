# tests/testthat/helper-tmux.R

skip_if_no_tmux <- function() {
  if (Sys.getenv("TMUX") == "") {
    testthat::skip("Not in a tmux session; skipping.")
  }
}

waitwait_for <- function(predicate, timeout_s = 60, poll_every = 0.5) {
  start <- Sys.time()
  repeat {
    if (isTRUE(predicate())) return(TRUE)
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_s) {
      return(FALSE)
    }
    Sys.sleep(poll_every)
  }
