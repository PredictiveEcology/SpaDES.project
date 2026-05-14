# tests/testthat/test-kill-helper.R

testthat::test_that("tmuxKillPanes returns targeted panes", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_no_tmux()

  td <- tempfile("tmux_kill")
  dir.create(td)
  global <- file.path(td, "global.R")

  # global sleeps long so we can kill before it runs
  writeLines("Sys.sleep(30)", global)

  # one worker, small delay
  workers <- experimentTmux(
    df                  = data.frame(.ELFind="X", .rep=1, check.names=FALSE),
    global_path         = global,
    n_workers           = 1,
    start_cmd           = "R",
    delay_before_source = 5,
    continue            = FALSE
  )

  # kill immediately
  killed <- tmuxKillPanes(workers)
  testthat::expect_equal(killed, workers)
})
