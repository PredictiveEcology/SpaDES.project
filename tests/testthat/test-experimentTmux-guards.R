## experimentTmux() argument validation.
##
## These are the guards that fire before any tmux server work, so they need no
## tmux session and run on every platform. The launch path itself is covered by
## test-single-shot.R and test-queue-mode.R, which do use a real tmux.

mkDf <- function(n = 1) {
  data.frame(.ELFind = paste0("job", seq_len(n)), .rep = seq_len(n),
             check.names = FALSE)
}

mkGlobal <- function(dir) {
  f <- file.path(dir, "global.R")
  writeLines("invisible(NULL)", f)
  f
}

test_that("experimentTmux rejects a df that is not a data.frame", {
  skip_if_not_installed("processx")
  td <- withr::local_tempdir()

  expect_error(
    experimentTmux(df = list(a = 1), global_path = mkGlobal(td)),
    "'df' must be a data.frame"
  )
})

test_that("experimentTmux rejects n_workers below one", {
  skip_if_not_installed("processx")
  td <- withr::local_tempdir()

  expect_error(
    experimentTmux(df = mkDf(), global_path = mkGlobal(td), n_workers = 0L),
    "'n_workers' must be >= 1"
  )
})

test_that("experimentTmux warns when global_path does not exist", {
  skip_if_not_installed("processx")
  td <- withr::local_tempdir()
  gone <- file.path(td, "no-such-global.R")

  # the warning is raised, then n_workers = 0 stops it before anything launches
  expect_warning(
    try(experimentTmux(df = mkDf(), global_path = gone, n_workers = 0L),
        silent = TRUE),
    "global_path not found"
  )
})
