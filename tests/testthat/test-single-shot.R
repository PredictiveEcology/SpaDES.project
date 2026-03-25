# tests/testthat/test-single-shot.R

testthat::test_that("experimentTmux single-shot assigns all columns and sources once", {
  skip_if_no_tmux()
  td <- tempfile("tmux_single"); unlink(td, recursive = TRUE); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  outdir <- file.path(td, "out"); dir.create(outdir)

  writeLines(sprintf(
    'res <- as.list(mget(ls(.GlobalEnv, all.names = TRUE), .GlobalEnv))
     saveRDS(res, file.path("%s", paste0("res_", res$.ELFind, ".rds")))',
    outdir
  ), global)

  expt <- data.frame(.ELFind = c("6.1.1","6.2.2"), .rep = c(1,1), check.names = FALSE)

  workers <- experimentTmux(
    df                  = expt,
    global_path         = global,
    n_workers           = 2,
    start_cmd           = "R",
    delay_before_source = 2,
    stagger_by          = 1,
    set_mouse           = TRUE,
    continue            = FALSE,
    activeRunningPath   = file.path(td, "logs")
  )
  on.exit(try(tmux_kill_panes(workers), silent = TRUE), add = TRUE)

  ok <- wait_for(function() length(list.files(outdir, "^res_.*\\.rds$", full.names = TRUE)) == 2,
                 timeout_s = 120)
  testthat::expect_true(ok)
  testthat::expect_equal(readRDS(file.path(outdir,"res_6.1.1.rds"))$.rep, 1)
  testthat::expect_equal(readRDS(file.path(outdir,"res_6.2.2.rds"))$.rep, 1)
})