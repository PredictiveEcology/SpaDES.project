## setupRestart(): decide whether to restart RStudio inside the project, and
## warn when updateRprofile cannot take effect.
##
## Two seams make this testable without an RStudio session: isRstudio() (already
## present) and isInteractive() (added alongside it). Everything past the guard
## at the top of the restart block drives rstudioapi -- opening projects,
## navigating files, restarting the session -- so these tests stay on the
## decision logic and the early exits, and never let the function reach it.

mkPaths <- function(dir) {
  list(projectPath = dir, packagePath = file.path(dir, "packages"),
       modulePath = file.path(dir, "modules"))
}

test_that("setupRestart does nothing when not in RStudio and not using git", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(isRstudio = function() FALSE,
                                  isInteractive = function() TRUE)

  # not RStudio -> Restart is forced FALSE; useGit FALSE -> guard is FALSE
  expect_no_error(
    SpaDES.project:::setupRestart(updateRprofile = FALSE, paths = mkPaths(td),
                                  name = basename(td), inProject = TRUE,
                                  Restart = TRUE, useGit = FALSE,
                                  origGetWd = td, verbose = -1)
  )
})

test_that("setupRestart does nothing when non-interactive even if Restart is TRUE", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(isRstudio = function() TRUE,
                                  isInteractive = function() FALSE)

  # isInteractive() FALSE collapses the first half of the guard; useGit FALSE
  # collapses the second, so rstudioapi is never reached
  expect_no_error(
    SpaDES.project:::setupRestart(updateRprofile = FALSE, paths = mkPaths(td),
                                  name = basename(td), inProject = TRUE,
                                  Restart = TRUE, useGit = FALSE,
                                  origGetWd = td, verbose = -1)
  )
})

test_that("setupRestart warns when updateRprofile cannot be honoured", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    isRstudio = function() TRUE,
    isInteractive = function() FALSE,
    inTempProject = function(...) FALSE,
    isInRstudioProj = function(...) FALSE
  )

  # RStudio, but not an RStudio project and Restart is FALSE: .Rprofile would
  # never be read, so the user is told rather than left guessing
  expect_warning(
    SpaDES.project:::setupRestart(updateRprofile = TRUE, paths = mkPaths(td),
                                  name = basename(td), inProject = FALSE,
                                  Restart = FALSE, useGit = FALSE,
                                  origGetWd = td, verbose = -1),
    "projectPath is not an Rstudio project"
  )
})

test_that("setupRestart does not warn when already in the right RStudio project", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    isRstudio = function() TRUE,
    isInteractive = function() FALSE,
    inTempProject = function(...) FALSE,
    isInRstudioProj = function(...) TRUE
  )

  expect_no_warning(
    SpaDES.project:::setupRestart(updateRprofile = TRUE, paths = mkPaths(td),
                                  name = basename(td), inProject = TRUE,
                                  Restart = FALSE, useGit = FALSE,
                                  origGetWd = td, verbose = -1)
  )
})

test_that("setupRestart warns when updateRprofile is used in a temp project", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    isRstudio = function() TRUE,
    isInteractive = function() FALSE,
    inTempProject = function(...) TRUE
  )

  # a temp project's .Rprofile would be discarded, so this is its own warning
  expect_warning(
    SpaDES.project:::setupRestart(updateRprofile = TRUE, paths = mkPaths(td),
                                  name = basename(td), inProject = TRUE,
                                  Restart = FALSE, useGit = FALSE,
                                  origGetWd = td, verbose = -1)
  )
})

test_that("setupRestart leaves updateRprofile = FALSE alone entirely", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    isRstudio = function() TRUE,
    isInteractive = function() FALSE,
    inTempProject = function(...) stop("must not be consulted")
  )

  # the whole updateRprofile block is skipped, so inTempProject is never called
  expect_no_error(
    SpaDES.project:::setupRestart(updateRprofile = FALSE, paths = mkPaths(td),
                                  name = basename(td), inProject = FALSE,
                                  Restart = FALSE, useGit = FALSE,
                                  origGetWd = td, verbose = -1)
  )
})

test_that("isInteractive mirrors base::interactive by default", {
  # the seam exists to be mocked; unmocked it must not change behaviour
  expect_identical(SpaDES.project:::isInteractive(), interactive())
})
