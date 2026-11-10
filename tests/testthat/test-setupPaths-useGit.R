## With useGit set and a new, empty packagePath, setupPaths() looked up usethis's
## dependencies with Require::pkgDep() after .libPaths() had moved to that empty
## library. pkgDep() needs pak, which was not there yet, so a fresh project stopped
## with "Please install pak" (and, with pak present, usethis was installed again
## instead of copied).

test_that("useGit: usethis's dependencies are found in the user's library, then copied to the new one", {
  setupTest()
  skip_if_not_installed("usethis")
  prj <- withr::local_tempdir()
  newLib <- file.path(prj, "packages")
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  withr::local_options(spades.useRequire = TRUE)
  userLibs <- .libPaths()

  libsAtPkgDep <- NULL
  local_mocked_bindings(
    pkgDep = function(...) {
      libsAtPkgDep <<- .libPaths()
      list(usethis = c("gert", "gh"))
    },
    .package = "Require")
  copied <- NULL
  local_mocked_bindings(setupSpaDES.ProjectDeps = function(paths, deps, ...) copied <<- deps)

  suppressWarnings(setupPaths(paths = list(projectPath = prj, packagePath = newLib),
                              useGit = "PredictiveEcology", Restart = FALSE,
                              updateRprofile = FALSE, verbose = -2))

  expect_identical(libsAtPkgDep, userLibs)
  expect_true(all(c("usethis", "gert", "gh") %in% copied))
})
