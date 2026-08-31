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
  ## Compact enough to survive the coverage job's log truncation, and aimed at
  ## the one hypothesis still standing: that the two worker R sessions are
  ## killed (memory) rather than never starting. `pane_dead` and the pane's
  ## last lines distinguish "never got going" from "died partway".
  tmuxF <- function(fmt) tryCatch(
    system2("tmux", c("list-panes", "-a", "-F", shQuote(fmt)),
            stdout = TRUE, stderr = TRUE),
    error = function(e) paste("tmux failed:", conditionMessage(e)))

  diagnose <- function() {
    ## ORDER MATTERS: the coverage job echoes only the TAIL of
    ## testthat.Rout.fail, so the most diagnostic items go LAST. A first
    ## attempt dumped every file in logs/ -- including the long generated
    ## worker_startup_*.R scripts -- and pushed everything useful out of the
    ## surviving window.
    bits <- character()

    ## least important first: the marker files, excluding the generated scripts
    lf <- list.files(file.path(td, "logs"), recursive = TRUE, full.names = TRUE)
    lf <- lf[!grepl("\\.R$", lf)]
    bits <- c(bits, paste("logfiles:", paste(basename(lf), collapse = ", ")))
    for (f in lf) {
      ln <- tryCatch(utils::tail(readLines(f, warn = FALSE), 2),
                     error = function(e) "unreadable")
      bits <- c(bits, paste0(basename(f), ": ", paste(ln, collapse = " / ")))
    }

    bits <- c(bits,
      paste("PANESTATE:", paste(tmuxF("#{pane_id} dead=#{pane_dead} cmd=#{pane_current_command}"),
                                collapse = " | ")),
      paste("MEM:", paste(tryCatch(readLines("/proc/meminfo", n = 1),
                                   error = function(e) "n/a"), collapse = "; ")))

    ## LAST and therefore most likely to survive: the panes showed the worker's
    ## error handler ("q(status=1L) to restart loop"), so the workers are alive
    ## and erroring rather than dying. The error text is what is actually needed,
    ## so pull just those lines out of the capture instead of its raw tail.
    for (w in unlist(workers)) {
      cap <- tryCatch(system2("tmux", c("capture-pane", "-p", "-t", w),
                              stdout = TRUE, stderr = TRUE),
                      error = function(e) "capture failed")
      cap <- cap[nzchar(trimws(cap))]
      hit <- grep("rror|annot|not found|unable|failed|Warning", cap, value = TRUE)
      if (!length(hit)) hit <- utils::tail(cap, 4)
      bits <- c(bits, paste0("ERR ", w, ": ",
                             paste(utils::tail(hit, 5), collapse = " ~ ")))
    }
    bits <- c(bits, paste("RESULTS:", length(list.files(outdir, "^res_.*\\.rds$")), "of 2"))
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