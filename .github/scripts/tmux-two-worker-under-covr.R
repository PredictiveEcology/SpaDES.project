## Run the two-worker reproduction under covr instrumentation, inside testthat,
## the way the coverage job does.
##
## Why: the plain reproduction PASSES on CI (2 of 2 result files in 5s), and the
## same test passes locally under covr, inside the full suite. The only
## untested combination left is CI + covr + testthat, which is exactly the job
## that fails. covr runs `code` in a subprocess whose stdout is not forwarded
## even with quiet = FALSE, so the inner run sinks everything to a file that the
## workflow prints afterwards.

outdirRoot <- Sys.getenv("OUTDIR", unset = file.path(tempdir(), "tmuxdbg"))
dir.create(outdirRoot, recursive = TRUE, showWarnings = FALSE)
innerOut <- file.path(outdirRoot, "covr-inner.txt")
unlink(innerOut)

repro <- normalizePath(".github/scripts/tmux-two-worker-debug.R")

inner <- sprintf('
  con <- file("%s", open = "wt")
  sink(con, type = "output"); sink(con, type = "message")
  cat("libPaths[1]:", .libPaths()[1L], "\\n")
  cat("R_COVR     :", Sys.getenv("R_COVR"), "\\n")
  cat("CI         :", Sys.getenv("CI"), "\\n")
  cat("TMUX       :", Sys.getenv("TMUX"), "\\n")

  ## (a) the standalone reproduction, now under instrumentation
  cat("\\n### standalone reproduction under covr ###\\n")
  try(source("%s", echo = FALSE))

  ## (b) the real test, with skip_on_ci defeated so it actually runs
  cat("\\n### test-single-shot.R under covr ###\\n")
  ci <- Sys.getenv("CI"); Sys.setenv(CI = "")
  ## FULL suite: single-shot on its own passes here, so the remaining variable
  ## is what the rest of the suite leaves behind -- note that worker panes from
  ## an earlier test stay alive and idling in the same tmux server.
  try(testthat::test_dir("tests/testthat", package = "SpaDES.project",
                         load_package = "installed", filter = NULL,
                         reporter = "summary", stop_on_failure = FALSE))
  Sys.setenv(CI = ci)

  cat("\n### tmux panes after the full suite ###\n")
  cat(paste(suppressWarnings(system2("tmux",
      c("list-panes", "-a", "-F",
        shQuote("#{session_name}:#{window_index}.#{pane_index} id=#{pane_id} dead=#{pane_dead} cmd=#{pane_current_command} title=#{pane_title}")),
      stdout = TRUE, stderr = TRUE)), collapse = "\n"), "\n")

  sink(type = "message"); sink(type = "output"); close(con)
', innerOut, repro)

covr::package_coverage(".", type = "none", code = inner)
cat("outer wrapper finished\n")
