## Worker profile leak: the respawn script is sourced through R_PROFILE_USER and
## must not be inherited by the job's own child R processes (makeClusterPSOCK,
## callr, mirai). A respawned pane once leaked it, and each climateData PSOCK
## worker claimed a queue row and ran a whole simulation.

test_that(".build_worker_r_expr unsets R_PROFILE_USER before anything else", {
  expr <- SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "global.R", on_interrupt = "requeue",
    runNameLabel = quote(colnames(q)[1]), activeRunningPath = "logs",
    ss_id = NULL, pane_mode = "killAndNewPane", email = NULL, cache_path = NULL,
    dots_path = NULL, lib_path = "/lib"
  )
  expect_true(startsWith(expr, "Sys.unsetenv('R_PROFILE_USER'); "))
  expect_match(expr, "SpaDES.project::tmuxRunWorkerLoop\\(", fixed = FALSE)
})

test_that(".isParallelWorkerProcess recognises PSOCK workers and nothing else", {
  f <- SpaDES.project:::.isParallelWorkerProcess
  parallelly <- c("/opt/R/bin/exec/R", "--no-echo", "--no-restore", "-e",
    "try(suppressWarnings(cat(Sys.getpid(),file=\"/tmp/Rtmp/worker.rank=1.parallelly.parent=123.pid\")))")
  base <- c("R", "--no-echo", "--no-restore", "-e", "parallel:::.workRSOCK()")
  expect_true(f(parallelly))
  expect_true(f(base))
  expect_false(f(c("R", "--quiet", "--no-save", "--no-restore", "--interactive")))
  expect_false(f(c("R", "--no-echo", "--no-restore", "-e", "testthat::test_check('SpaDES.project')")))
  expect_false(f(character()))
})

test_that("tmuxRunWorkerLoop refuses to run inside a parallel worker", {
  testthat::local_mocked_bindings(.isParallelWorkerProcess = function(args = NULL) TRUE,
                                  .package = "SpaDES.project")
  expect_message(
    res <- tmuxRunWorkerLoop(queue_path = tempfile(fileext = ".rds"), global_path = "global.R"),
    "parallel worker")
  expect_false(res)
})
