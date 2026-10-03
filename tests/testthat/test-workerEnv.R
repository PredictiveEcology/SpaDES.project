test_that("every worker launch command disables package installation", {
  ## A worker must never install packages. Every worker sources the user's global.R at the
  ## start of every job, so N workers means N processes resolving and writing one shared
  ## library, which corrupts the lazy-load databases of the workers already running.
  ## Installation belongs to whoever launches the fleet, once, before any worker exists.
  ##
  ## Source-level on purpose: these commands are assembled inside sprintf() and handed to
  ## tmux or ssh, so there is nothing to interrogate at runtime short of starting a real
  ## worker. The regression to prevent is a launch site being added or edited without the
  ## variable, which is exactly what this sees.
  ##
  ## Count the launch FORM, `R_PROFILE_USER=%s`, not every mention of R_PROFILE_USER --
  ## the latter also matches Sys.unsetenv() and the `.started` flag path, which are not
  ## launches. My first version of this test used the loose pattern and failed on those.
  for (fn in c("experimentTmux", "tmuxRunWorkerLoop")) {
    f <- tryCatch(getFromNamespace(fn, "SpaDES.project"), error = function(e) NULL)
    skip_if(is.null(f), paste(fn, "not found"))
    src <- paste(deparse(f), collapse = "\n")

    nLaunch <- lengths(regmatches(src, gregexpr("R_PROFILE_USER=%s", src, fixed = TRUE)))
    if (identical(nLaunch, 0L)) next
    nGuard <- lengths(regmatches(src, gregexpr(".workerEnvArg()", src, fixed = TRUE)))

    expect_gte(nGuard, nLaunch,
               label = paste0(fn, " has ", nLaunch, " worker launch command(s) and ",
                              nGuard, " built with .workerEnvArg()"))
  }
  expect_identical(SpaDES.project:::.workerEnvArg(), "SPADES_USE_REQUIRE=false")
})

test_that("SPADES_USE_REQUIRE is the documented lever, so a user global.R needs no change", {
  skip_if_not_installed("SpaDES.core")
  ## The whole point of using the environment rather than an option: a global.R that says
  ## nothing about spades.useRequire installs when run by hand and does not install when
  ## run by a worker. If SpaDES.core stops deriving the option from this variable, the
  ## guard above becomes silently ineffective, so pin the contract here.
  deflt <- formals(SpaDES.core::setPaths)   # cheap probe that the package is loadable
  expect_true(is.list(deflt))
  optSrc <- paste(deparse(SpaDES.core::spadesOptions), collapse = " ")
  expect_match(optSrc, "SPADES_USE_REQUIRE", fixed = TRUE)
})

test_that(".useRequire() follows SPADES_USE_REQUIRE unless the option is set", {
  uR <- SpaDES.project:::.useRequire
  withr::with_options(list(spades.useRequire = NULL), {
    withr::with_envvar(c(SPADES_USE_REQUIRE = "false"), expect_false(uR()))
    withr::with_envvar(c(SPADES_USE_REQUIRE = "FALSE"), expect_false(uR()))
    withr::with_envvar(c(SPADES_USE_REQUIRE = NA), expect_true(uR()))
    withr::with_envvar(c(SPADES_USE_REQUIRE = "true"), expect_true(uR()))
  })
  withr::with_envvar(c(SPADES_USE_REQUIRE = "false"),
                     withr::with_options(list(spades.useRequire = TRUE), expect_true(uR())))
  withr::with_envvar(c(SPADES_USE_REQUIRE = NA),
                     withr::with_options(list(spades.useRequire = FALSE), expect_false(uR())))
})

test_that("a worker (SPADES_USE_REQUIRE=false, option unset) skips Require in setupPackages()", {
  ## In a worker SpaDES.core is not loaded yet, so spades.useRequire is NULL here.
  nRequire <- 0L
  testthat::local_mocked_bindings(
    Require = function(packages, ...) { nRequire <<- nRequire + 1L; seq_along(packages) },
    .package = "Require")
  withr::local_options(spades.useRequire = NULL, spades.useRequireOverride = NULL)
  withr::local_envvar(SPADES_USE_REQUIRE = "false")
  lib <- withr::local_tempdir()
  expect_message(
    setupPackages("fakeA", modulePackages = list(), require = list(), paths = list(),
                  libPaths = lib, standAlone = FALSE, setLinuxBinaryRepo = FALSE,
                  envir = new.env(), callingEnv = new.env(), verbose = 1),
    "skipping setupPackages")
  expect_identical(nRequire, 0L)
})

test_that("experimentFuture() workers and experimentSBATCH() jobs get SPADES_USE_REQUIRE=false", {
  expect_identical(SpaDES.project:::.workerEnv, c(SPADES_USE_REQUIRE = "false"))

  ## each callr::r_bg() that starts an experimentFuture worker passes .workerEnv in `env`
  for (fn in c("experimentFuture", "runWorkerLoopFuture")) {
    src <- paste(deparse(getFromNamespace(fn, "SpaDES.project")), collapse = "\n")
    nStart <- lengths(regmatches(src, gregexpr("callr::r_bg(", src, fixed = TRUE)))
    nEnv <- lengths(regmatches(src, gregexpr(".workerEnv", src, fixed = TRUE)))
    expect_gte(nEnv, nStart, label = paste0(fn, ": r_bg launches with .workerEnv"))
  }

  f <- withr::local_tempfile(fileext = ".sh")
  SpaDES.project:::.sbatch_write_script(
    script_path = f, worker_idx = 1L, log_file = "/tmp/w.log", stop_file = "/tmp/stop",
    queue_path = "/tmp/queue.rds", global_path = "/tmp/global.R", on_interrupt = NULL,
    ss_id = NULL, email = NULL, cache_path = "/tmp/cache", runNameLabel = "run",
    activeRunningPath = "/tmp/active", dots_path = NULL, sbatch_opts = list(),
    r_cmd = "Rscript", r_libs = "/tmp/lib")
  body <- readLines(f)
  iExport <- match("export SPADES_USE_REQUIRE=false", body)
  expect_false(is.na(iExport))
  expect_lt(iExport, grep("Rscript", body)[1])
})
