## The worker profile (R_PROFILE_USER) runs the whole worker loop, and R reads the user profile BEFORE it
## attaches the default packages. Jobs therefore ran without grDevices, graphics, stats and datasets on
## the search path and failed at their first base-graphics Plots() call: "object 'png' not found"
## (FireSense Mackenzie phase 1, 2026-09-23).

test_that("code run from a worker profile sees the default packages", {
  ## Queue workers are tmux panes, which exist only on Unix; on Windows CI this launch writes nothing.
  skip_on_os("windows")
  prof <- withr::local_tempfile(fileext = ".R")
  out <- withr::local_tempfile(fileext = ".txt")
  writeLines(c(SpaDES.project:::.tmux_profile_head(),
               sprintf("writeLines(search(), %s)", deparse(normalizePath(out, winslash = "/", mustWork = FALSE)))), prof)
  ## Rscript reads R_PROFILE_USER too; shQuote() so the expression survives Windows' shell as well
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  withr::with_envvar(c(R_PROFILE_USER = prof, R_DEFAULT_PACKAGES = "NULL"),
                     system2(rscript, c("-e", shQuote("invisible(0)")), stdout = FALSE, stderr = FALSE))
  expect_true(file.exists(out), info = "the profile did not run")
  s <- readLines(out)
  expect_true(all(paste0("package:", c("grDevices", "graphics", "stats", "datasets", "utils", "methods")) %in% s))
})

## pak's private processx installs a SIGCHLD handler; parallel saves it at its first fork. At exit pak's
## finalizer unloads processx.so, then parallel's finalizer reinstalls the saved handler, and R's tempdir
## cleanup (system("rm ...")) segfaults. The profile's .Last restores parallel's handler first.
test_that("a worker profile session does not segfault at exit after pak's processx and a fork", {
  skip_on_os("windows")
  skip_if_not_installed("pak")
  prof <- withr::local_tempfile(fileext = ".R")
  writeLines(SpaDES.project:::.tmux_profile_head(), prof)
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c("loadNamespace('parallel')",
               "pak:::load_private_package('processx', 'c_')",
               "p <- pak:::pkg_data$ns$processx$process$new('true'); p$wait()",
               "j <- parallel::mcparallel(1); invisible(parallel::mccollect(j))"), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- withr::with_envvar(c(R_PROFILE_USER = prof),
                               suppressWarnings(system2(rscript, shQuote(script), stdout = FALSE, stderr = FALSE)))
  expect_identical(status, 0L)
})
