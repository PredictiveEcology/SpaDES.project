# tests/testthat/helper-tmux.R


skip_if_no_tmux <- function() {
  if (Sys.getenv("TMUX") == "") testthat::skip("Not in a tmux session; skipping.")
}

## Panes of the DEFAULT tmux server (the caller's own, if any), as "<id> <start command>"; NULL when none runs.
## TMUX is dropped so the query cannot be redirected to a private server.
defaultTmuxPanes <- function() {
  out <- withr::with_envvar(c(TMUX = NA, TMUX_PANE = NA),
    tryCatch(suppressWarnings(system2("tmux", c("list-panes", "-a", "-F", shQuote("#{pane_id} #{pane_start_command}")),
                                      stdout = TRUE, stderr = FALSE)), error = function(e) NULL))
  if (!is.null(attr(out, "status"))) NULL else out
}

## For a test that runs experimentTmux() for real: skip unless in tmux (as skip_if_no_tmux()), then put the test on
## its own tmux server (`-L`), so experimentTmux()'s split-window / send-keys / select-pane (which follow $TMUX and
## $TMUX_PANE) never reach the caller's server. The server is killed through its own socket when the test ends, and
## the test fails if the default server gained a pane that starts in this test's temp dir.
local_private_tmux <- function(.local_envir = parent.frame()) {
  skip_if_no_tmux()
  testthat::skip_if_not_installed("processx")
  before <- defaultTmuxPanes()
  sock <- paste0("sptestpriv", Sys.getpid(), "_", basename(tempfile("")))
  tm <- function(...) processx::run("tmux", c("-L", sock, ...), error_on_status = FALSE)
  withr::defer({
    tm("kill-server")
    after <- defaultTmuxPanes()
    if (!is.null(before)) {
      added <- setdiff(after, before)
      testthat::expect_false(any(grepl(tempdir(), added, fixed = TRUE)),
                             info = paste("default tmux server gained panes:", paste(added, collapse = "; ")))
    }
  }, envir = .local_envir)
  testthat::expect_equal(tm("-f", "/dev/null", "new-session", "-d", "-x", "200", "-y", "60", "-s", "t", "sleep 3600")$status, 0L)
  withr::local_envvar(c(TMUX = trimws(tm("display", "-p", "-t", "t", "#{socket_path},#{pid},0")$stdout),
                        TMUX_PANE = trimws(tm("display", "-p", "-t", "t", "#{pane_id}")$stdout)),
                      .local_envir = .local_envir)
  invisible(sock)
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
