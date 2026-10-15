## .workerResourceDefaults(): per-worker terra / GDAL / PROJ settings. terra is
## mocked, so nothing here changes the test session's own settings.

mockTerra <- function(gdalCurrent = 51480, env = parent.frame()) {
  testthat::skip_if_not_installed("terra")   # a Suggests; absent in the nosuggests check
  rec <- new.env()
  rec$todisk <- NULL; rec$cache <- NULL; rec$proj <- NULL
  testthat::local_mocked_bindings(
    terraOptions = function(todisk, ...) rec$todisk <- todisk,
    gdalCache    = function(size = NA) { if (is.na(size)) gdalCurrent else rec$cache <- size },
    projNetwork  = function(enable, url = "") rec$proj <- enable,
    .package = "terra", .env = env)
  rec
}

test_that(".workerResourceDefaults sets each value when the user has not", {
  withr::local_options(terra_default = NULL)
  withr::local_envvar(GDAL_CACHEMAX = NA, PROJ_NETWORK = NA)
  rec <- mockTerra()
  expect_message(SpaDES.project:::.workerResourceDefaults(1L, ramMB = 1e6), "todisk")
  expect_true(rec$todisk)
  expect_identical(rec$cache, 50000)        # 5% of 1e6 MB, one worker
  expect_false(rec$proj)
})

test_that(".workerResourceDefaults divides the cache by the workers on the host", {
  withr::local_options(terra_default = NULL)
  withr::local_envvar(GDAL_CACHEMAX = NA, PROJ_NETWORK = NA)
  rec <- mockTerra()
  suppressMessages(SpaDES.project:::.workerResourceDefaults(10L, ramMB = 1e6))
  expect_identical(rec$cache, 5000)
})

test_that(".workerResourceDefaults never raises the cache above GDAL's current value", {
  withr::local_options(terra_default = NULL)
  withr::local_envvar(GDAL_CACHEMAX = NA, PROJ_NETWORK = NA)
  rec <- mockTerra(gdalCurrent = 100)
  suppressMessages(SpaDES.project:::.workerResourceDefaults(1L, ramMB = 1e6))
  expect_null(rec$cache)
})

test_that(".workerResourceDefaults leaves alone what the user set", {
  withr::local_options(terra_default = list(todisk = FALSE))
  withr::local_envvar(GDAL_CACHEMAX = "256", PROJ_NETWORK = "ON")
  rec <- mockTerra()
  expect_message(SpaDES.project:::.workerResourceDefaults(4L, ramMB = 1e6), "none needed")
  expect_null(rec$todisk)
  expect_null(rec$cache)
  expect_null(rec$proj)
})

test_that(".workersPerHost counts the workers sharing a host, local aliases as one", {
  f <- SpaDES.project:::.workersPerHost
  expect_identical(f(NULL, 3L), c(1L, 1L, 1L))
  expect_identical(f(c("localhost", "127.0.0.1", "nodeB"), 3L), c(2L, 2L, 1L))
  expect_identical(f(c("nodeA", "nodeB"), 4L), c(2L, 2L, 2L, 2L))
})

test_that("every launcher's worker path passes the per-host worker count to tmuxRunWorkerLoop", {
  ## tmuxRunWorkerLoop applies the defaults before sourcing global_path
  src <- paste(deparse(body(SpaDES.project::tmuxRunWorkerLoop)), collapse = "\n")
  expect_match(src, ".workerResourceDefaults(n_workers_on_host)", fixed = TRUE)

  ## experimentTmux (and its pane respawn): the generated worker call
  expr <- SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "g.R", on_interrupt = "requeue", runNameLabel = quote(a),
    activeRunningPath = NULL, ss_id = NULL, pane_mode = "reuse", email = NULL, cache_path = NULL,
    dots_path = NULL, snapshot_library = FALSE, n_workers_on_host = 3L)
  expect_match(expr, "n_workers_on_host=3L", fixed = TRUE)

  ## experimentSBATCH: the job script
  td <- withr::local_tempdir()
  writeLines("invisible(NULL)", file.path(td, "global.R"))
  es <- suppressMessages(experimentSBATCH(
    df = data.frame(.ELFind = c("a", "b", "c"), .rep = 1:3, check.names = FALSE),
    global_path = file.path(td, "global.R"), n_workers = 3L,
    queue_path = file.path(td, "q.rds"), log_dir = file.path(td, "logs"),
    sbatch_opts = list("ntasks-per-node" = 2), dry_run = TRUE))
  expect_match(paste(readLines(unlist(es$job_scripts)[[1]]), collapse = "\n"),
               "n_workers_on_host = 2L", fixed = TRUE)

  ## experimentFuture: the callr worker, and the cluster worker via runWorkerLoopFuture
  for (fn in c("experimentFuture", "runWorkerLoopFuture")) {
    src <- paste(deparse(getFromNamespace(fn, "SpaDES.project")), collapse = "\n")
    expect_match(src, "n_workers_on_host", fixed = TRUE, info = fn)
  }
  expect_true("n_workers_on_host" %in% names(formals(SpaDES.project::runWorkerLoopFuture)))
})
