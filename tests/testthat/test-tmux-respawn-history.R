## `tmux respawn-pane -k` discards the pane's visible screen without pushing it to
## scrollback, so a failed job's "Error ..." / "Job finished ..." lines vanished.
## .pushScreenToHistory() scrolls the screen into history first. Runs on a private tmux
## server (-L), never the default one.

test_that("the last screen of a pane survives respawn-pane", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("tmux")), "tmux not available")
  skip_if_not_installed("processx")
  skip_if_not_installed("pkgload")

  sock <- paste0("sptest", Sys.getpid())
  tm <- function(...) processx::run("tmux", c("-L", sock, ...), error_on_status = FALSE)
  withr::defer(tm("kill-server"))

  load <- if (pkgload::is_dev_package("SpaDES.project"))
    sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(system.file(package = "SpaDES.project")))
  else "library(SpaDES.project)"
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    load,
    "for (i in 1:30) cat('line', i, '\\n', sep = '')",
    ## the pre-respawn step; $TMUX makes its tmux calls reach the private server
    "SpaDES.project:::.pushScreenToHistory()",
    "Sys.sleep(60)"), script)

  r <- tm("-f", "/dev/null", "new-session", "-d", "-x", "80", "-y", "10", "-s", "t",
          paste("Rscript", shQuote(script)))
  expect_equal(r$status, 0L)
  pane <- trimws(tm("display", "-p", "-t", "t", "#{pane_id}")$stdout)
  ## the pane is done pushing once its screen no longer shows the first lines
  ok <- wait_for(function() any(grepl("line30", tm("capture-pane", "-p", "-S", "-", "-t", pane)$stdout)) &&
                   !any(grepl("^line30", tm("capture-pane", "-p", "-t", pane)$stdout)),
                 timeout_s = 60)
  expect_true(ok)

  tm("respawn-pane", "-k", "-t", pane, "sleep 60")
  Sys.sleep(1)
  hist <- tm("capture-pane", "-p", "-S", "-", "-t", pane)$stdout
  expect_match(hist, "line30", fixed = TRUE)
})
