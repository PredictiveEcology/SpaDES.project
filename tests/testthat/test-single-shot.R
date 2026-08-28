# tests/testthat/test-single-shot.R

testthat::test_that("experimentTmux single-shot assigns all columns and sources once", {
  # Skipped on CI, unlike the other two tmux tests, which do run there. This is
  # the only one that starts TWO staggered workers. Investigated 2026-08-28:
  #   * it is NOT a defect in the staggered two-worker path -- run locally under
  #     a real tmux, both workers write their result files in ~2 seconds;
  #   * it is NOT a timeout -- it still produced no result files on CI with
  #     wait_for() scaled 5x (600s) and the job's own limit at 3600s.
  # So something about the runner environment stops the second worker's R from
  # reaching the queue. Diagnosing it needs the activeRunningPath worker logs
  # off the runner, which the coverage job truncates (see below).
  testthat::skip_on_ci()

  testthat::skip_on_cran()
  skip_if_no_tmux()
  td <- tempfile("tmux_single"); unlink(td, recursive = TRUE); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  global <- file.path(td, "global.R")
  outdir <- file.path(td, "out"); dir.create(outdir)

  # Data columns are published to the source()-local scenario env, not
  # .GlobalEnv (see commit refactoring tmuxRunNextWorker). Inside source()
  # `environment()` at top-level returns that scn_env, so we capture from it.
  writeLines(sprintf(
    'res <- as.list(mget(ls(envir = environment(), all.names = TRUE), envir = environment()))
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
  on.exit(try(tmuxKillPanes(workers), silent = TRUE), add = TRUE)

  ok <- wait_for(function() length(list.files(outdir, "^res_.*\\.rds$", full.names = TRUE)) == 2,
                 timeout_s = 120)
  if (!ok) {
    # Surface what the workers actually did. Note the coverage job only echoes
    # the tail of testthat.Rout.fail, so this dump is visible when running
    # locally but was truncated away on CI -- getting it off a runner needs an
    # artifact upload, not more message()s.
    message("--- worker logs (", file.path(td, "logs"), ") ---")
    for (f in list.files(file.path(td, "logs"), recursive = TRUE, full.names = TRUE)) {
      message("== ", f)
      message(paste(utils::tail(readLines(f, warn = FALSE), 40), collapse = "\n"))
    }
    message("--- outdir contents: ",
            paste(list.files(outdir), collapse = ", "), " ---")
  }
  testthat::expect_true(ok)
  testthat::expect_equal(readRDS(file.path(outdir,"res_6.1.1.rds"))$.rep, 1)
  testthat::expect_equal(readRDS(file.path(outdir,"res_6.2.2.rds"))$.rep, 1)
})