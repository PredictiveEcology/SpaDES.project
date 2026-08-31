# tests/testthat/test-single-shot.R

testthat::test_that("experimentTmux single-shot assigns all columns and sources once", {
  # This is the only tmux test that starts TWO staggered workers, and the only
  # one that fails on CI. Ruled out with evidence: a defect in the staggered
  # path (both workers finish in ~2s locally), a timeout (600s changed nothing),
  # the runner itself (a standalone reproduction passes on CI in 5s), covr
  # instrumentation (passes locally and on CI under covr), the worker inheriting
  # covr's temp .libPaths()[1] (it does, and copes), and leftover idle panes
  # from an earlier test (disproved directly).
  #
  # What has never been captured is what the workers do inside the real
  # coverage job. Earlier attempts printed it with message(), but that job
  # echoes only the tail of testthat.Rout.fail and truncated it away. testthat
  # prints a failing expectation's `info` in the final summary, which is the
  # part that does survive -- so the diagnosis rides on the assertion instead.

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
  ## Kept deliberately: this test failed on CI for weeks with nothing but
  ## "expected TRUE" to go on. The cause was the workers being handed only
  ## .libPaths()[1] -- under R CMD check that is the test library, not the one
  ## holding SpaDES.project -- so they died with "there is no package called".
  ## Diagnostics go in expect_true()'s `info`, not message(): the coverage job
  ## echoes only the tail of testthat.Rout.fail, and truncates from the front,
  ## so the most useful lines go LAST.
  diagnose <- function() {
    panes <- tryCatch(
      system2("tmux", c("list-panes", "-a", "-F",
                        shQuote("#{pane_id} dead=#{pane_dead} cmd=#{pane_current_command}")),
              stdout = TRUE, stderr = TRUE),
      error = function(e) "tmux list-panes failed")
    bits <- paste("PANES:", paste(panes, collapse = " | "))
    for (w in unlist(workers)) {
      cap <- tryCatch(system2("tmux", c("capture-pane", "-p", "-t", w),
                              stdout = TRUE, stderr = TRUE),
                      error = function(e) "capture failed")
      cap <- cap[nzchar(trimws(cap))]
      idx <- grep("rror|annot|not found|unable|failed", cap)
      idx <- sort(unique(c(idx, idx + 1L)))
      idx <- idx[idx >= 1L & idx <= length(cap)]
      hit <- if (length(idx)) cap[idx] else utils::tail(cap, 3)
      bits <- c(bits, paste0("ERR ", w, ": ",
                             paste(utils::tail(hit, 4), collapse = " ~ ")))
    }
    lp <- .libPaths()
    bits <- c(bits,
      paste("RESULTS:", length(list.files(outdir, "^res_.*\\.rds$")), "of 2"),
      paste("LIB1HAS:", dir.exists(file.path(lp[1], "SpaDES.project")),
            "NLIBS:", length(lp)))
    paste(bits, collapse = "\n")
  }

  testthat::expect_true(ok, info = if (!ok) diagnose())
  # only meaningful once the files exist; otherwise readRDS errors and hides
  # the diagnosis above
  if (ok) {
    testthat::expect_equal(readRDS(file.path(outdir,"res_6.1.1.rds"))$.rep, 1)
    testthat::expect_equal(readRDS(file.path(outdir,"res_6.2.2.rds"))$.rep, 1)
  }
})