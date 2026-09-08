## Window capacity for a headless tmux session (#161). A detached session keeps
## the default 80x24 window and "no space for new pane" appears after three or
## four splits. Pure geometry is tested directly; the tmux calls are mocked.

test_that(".tmux_window_geometry: attached client -> leave the window alone", {
  expect_null(SpaDES.project:::.tmux_window_geometry(6, 80, 24, attached = 1))
})

test_that(".tmux_window_geometry: headless 80x24 grows to fit the panes", {
  g <- SpaDES.project:::.tmux_window_geometry(6, 80, 24, attached = 0)
  expect_identical(g, list(width = 200L, height = 36L))   # ceiling(6/2) * 12 rows
  g8 <- SpaDES.project:::.tmux_window_geometry(8, 80, 24, attached = 0)
  expect_identical(g8$height, 48L)
})

test_that(".tmux_window_geometry: an already-large headless window is left alone", {
  expect_null(SpaDES.project:::.tmux_window_geometry(6, 240, 90, attached = 0))
})

test_that(".tmux_ensure_window_capacity issues set-option + resize-window only when headless", {
  calls <- list()
  testthat::local_mocked_bindings(
    .tmux_out = function(...) { calls[[length(calls) + 1L]] <<- c(...); "0 80 24 fits" },
    .tmux_run = function(...) { calls[[length(calls) + 1L]] <<- c(...); invisible(NULL) },
    .package = "SpaDES.project")
  g <- suppressMessages(SpaDES.project:::.tmux_ensure_window_capacity("fits:0", n_panes = 7))
  expect_identical(g, list(width = 200L, height = 48L))
  cmds <- vapply(calls, `[`, "", 1L)
  expect_identical(cmds, c("display-message", "set-option", "resize-window"))
  expect_true(all(c("window-size", "manual", "fits") %in% calls[[2L]]))
  expect_true(all(c("-x", "200", "-y", "48") %in% as.character(calls[[3L]])))

  calls <- list()
  testthat::local_mocked_bindings(
    .tmux_out = function(...) { calls[[length(calls) + 1L]] <<- c(...); "1 80 24 fits" },
    .package = "SpaDES.project")
  expect_null(SpaDES.project:::.tmux_ensure_window_capacity("fits:0", n_panes = 7))
  expect_identical(vapply(calls, `[`, "", 1L), "display-message")   # attached: no resize
})
