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
    nGuard <- lengths(regmatches(src, gregexpr("SPADES_USE_REQUIRE=false", src, fixed = TRUE)))

    expect_gte(nGuard, nLaunch,
               label = paste0(fn, " has ", nLaunch, " worker launch command(s) and ",
                              nGuard, " carrying SPADES_USE_REQUIRE=false"))
  }
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
