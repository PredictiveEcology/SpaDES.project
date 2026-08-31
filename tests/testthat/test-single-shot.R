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
                                   error = function(e) "n/a"), collapse = "; ")),
      ## the leading suspect: a worker's fresh R sees a different library set
      ## than the parent, because a tmux pane inherits the tmux SERVER's
      ## environment rather than the caller's.
      paste("R_LIBS_USER:", Sys.getenv("R_LIBS_USER")))

    ## LAST and therefore most likely to survive: the panes showed the worker's
    ## error handler ("q(status=1L) to restart loop"), so the workers are alive
    ## and erroring rather than dying. The error text is what is actually needed,
    ## so pull just those lines out of the capture instead of its raw tail.
    for (w in unlist(workers)) {
      cap <- tryCatch(system2("tmux", c("capture-pane", "-p", "-t", w),
                              stdout = TRUE, stderr = TRUE),
                      error = function(e) "capture failed")
      cap <- cap[nzchar(trimws(cap))]
      ## R wraps the message, so the useful half ("there is no package called
      ## 'x'") lands on the line AFTER "Error in loadNamespace(x) :". Take the
      ## match plus the two lines following it.
      idx <- grep("rror|annot|not found|unable|failed", cap)
      idx <- sort(unique(c(idx, idx + 1L, idx + 2L)))
      idx <- idx[idx >= 1L & idx <= length(cap)]
      hit <- if (length(idx)) cap[idx] else utils::tail(cap, 4)
      bits <- c(bits, paste0("ERR ", w, ": ",
                             paste(utils::tail(hit, 6), collapse = " ~ ")))
    }
    ## The workers report "there is no package called 'SpaDES.project'", so what
    ## matters is the library the generated startup script actually hands them,
    ## and whether the package is really there. lib_path comes from the parent's
    ## .libPaths()[1L], which under covr / R CMD check is a temp library.
    lp <- .libPaths()
    bits <- c(bits,
      paste("LIB1:", lp[1], "| dir?", dir.exists(lp[1]),
            "| haspkg?", dir.exists(file.path(lp[1], "SpaDES.project"))),
      paste("NLIBS:", length(lp)))
    sf <- list.files(file.path(td, "logs"), pattern = "worker_startup.*\\.R$",
                     full.names = TRUE)
    if (length(sf)) {
      l1 <- tryCatch(grep("libPaths", readLines(sf[[1]], warn = FALSE), value = TRUE)[1],
                     error = function(e) "unreadable")
      bits <- c(bits, paste("STARTUP_LIBPATHS:", substr(l1, 1, 300)))
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