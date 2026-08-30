# tests/testthat/helper-tmux.R


skip_if_no_tmux <- function() {
  if (Sys.getenv("TMUX") == "") testthat::skip("Not in a tmux session; skipping.")
}
## covr instruments every call in the package, so a worker -- which starts a
## fresh R and loads the instrumented build -- is far slower than normal, and CI
## runners have 2 cores against a workstation's many. Scale the budget rather
## than pick a single number that is either flaky there or slow here.
.waitScale <- function() if (nzchar(Sys.getenv("USING_COVR"))) 5L else 1L

wait_for <- function(predicate, timeout_s = 60, poll_every = 0.5) {
  timeout_s <- timeout_s * .waitScale()
  start <- Sys.time()
  repeat {
    if (isTRUE(predicate())) return(TRUE)
       if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_s) return(FALSE)
    Sys.sleep(poll_every)
  }
}
