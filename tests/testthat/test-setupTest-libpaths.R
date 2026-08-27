## Regression guard for setupTest()'s library isolation.
##
## setupTest() widens .libPaths() so packages can be *loaded* from the session's
## real libraries, then narrows it to a temp library so anything a test
## *installs* is thrown away. That used to be done with two overlapping
## withr::local_libpaths() calls: the first was scoped to setupTest()'s own
## frame, so its restore ran when setupTest() returned -- after the second --
## and handed callers the real library back. Every
## setupProject(paths = list(packagePath = .libPaths()[1L])) then installed into
## the developer's real library. These tests pin the fixed behaviour.

test_that("setupTest leaves the caller pointed at a temp library", {
  realLib <- .libPaths()

  setupTest()

  # tempdir() is the only place tests may write packages. Asserted as a property
  # rather than as `!= realLib[1]`, which depends on whatever an earlier test
  # left on the path.
  expect_true(startsWith(normalizePath(.libPaths()[1], mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))

  # ... and the real libraries stay reachable behind it, so tests can still LOAD
  # packages they did not install. Replacing the path outright took s2 / curl /
  # terra off the search path and broke tests that install nothing.
  expect_true(all(realLib %in% .libPaths()))
})

test_that("setupTest hands every test the same shared library", {
  first  <- local({ setupTest(); .libPaths()[1] })
  second <- local({ setupTest(); .libPaths()[1] })

  # one library per run, not one per test -- otherwise nothing installed by a
  # test can be reused and every setupProject() test rebuilds the whole tree
  expect_identical(first, second)
})

test_that("setupTest points Require.cloneFrom at the shared library", {
  setupTest()

  # Compare normalised paths, not raw strings: the option holds tempdir()'s
  # value verbatim while .libPaths() normalises. On Windows that is the same
  # directory spelled two ways -- "C:\\Users\\RUNNER~1\\...\\Rtmp.../x" vs
  # "C:/Users/runneradmin/.../Rtmp.../x" (8.3 short name, mixed separators).
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  expect_identical(norm(getOption("Require.cloneFrom")), norm(.libPaths()[1]))
})

test_that("setupTest's library change is undone when the caller exits", {
  before  <- .libPaths()
  inside  <- local({ setupTest(); .libPaths() })

  # the caller-scoped local_libpaths() is released with the calling frame
  expect_identical(.libPaths(), before)

  # ... and while it was in force, writes went to a temp library. Asserted as a
  # property rather than as `inside != before`: whether those two differ depends
  # on what state an earlier test happened to leave behind, which is not what
  # this test is about.
  expect_true(startsWith(normalizePath(inside[1], mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))
})

test_that("setupTest restores the real libraries even when they were already gone", {
  # setupProject() narrows .libPaths() to the project library as part of its job,
  # so a later test can inherit a path with the real libraries already missing.
  # setupTest() must put them back, not preserve their absence -- otherwise
  # packages that tests only ever LOAD (curl, s2, terra) become invisible for
  # the rest of the run.
  # same lookup setupTest() uses: setup.R's bindings live in testthat's env,
  # reachable through the calling frame's parents, not in .GlobalEnv
  realLib <- get0("origLibPaths", inherits = TRUE)
  skip_if(is.null(realLib), "origLibPaths not set (setup.R did not run)")

  realLib <- Filter(dir.exists, realLib)
  skip_if(length(realLib) == 0L, "no real libraries to check")

  narrowed <- withr::local_tempdir()
  withr::local_libpaths(narrowed)          # simulate the post-setupProject state

  # Precondition, not the thing under test: some environments (covr's
  # instrumented run, R_LIBS settings) keep those paths on the search path no
  # matter what we set, and then there is nothing here to restore. Skip rather
  # than fail -- a precondition that cannot be established is not a defect.
  skip_if(all(realLib %in% .libPaths()),
          "could not narrow .libPaths() in this environment")

  local({
    setupTest()
    expect_true(all(realLib %in% .libPaths()))
    # and the temp library still leads, so installs stay isolated
    expect_true(startsWith(normalizePath(.libPaths()[1], mustWork = FALSE),
                           normalizePath(tempdir(), mustWork = FALSE)))
  })
})
