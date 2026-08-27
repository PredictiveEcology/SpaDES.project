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

  expect_false(identical(.libPaths()[1], realLib[1]))
  # tempdir() is the only place tests may write packages
  expect_true(startsWith(normalizePath(.libPaths()[1], mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))
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

  expect_identical(getOption("Require.cloneFrom"), .libPaths()[1])
})

test_that("setupTest restores the real libraries when the caller exits", {
  realLib <- .libPaths()

  local({
    setupTest()
    expect_false(identical(.libPaths(), realLib))
  })

  expect_identical(.libPaths(), realLib)
})
