# setupPackages() skips Require when the package requirements are identical to a previous call.
# Require::Require is mocked, so there is no network; "installed" packages are fake
# entries (Meta/package.rds) in a temporary library.

fakeInstall <- function(lib, pkg, version = "1.0", imports = NULL, remoteSha = NULL) {
  d <- file.path(lib, pkg)
  dir.create(file.path(d, "Meta"), recursive = TRUE, showWarnings = FALSE)
  desc <- c(Package = pkg, Version = version, Built = paste0("R ", getRversion()))
  if (!is.null(imports)) desc[["Imports"]] <- imports
  if (!is.null(remoteSha)) desc[["RemoteSha"]] <- remoteSha
  saveRDS(list(DESCRIPTION = desc, Built = list(R = getRversion(), Platform = "", Date = "", OStype = "unix"),
               Rdepends = list(), Rdepends2 = list(), Depends = list(), Suggests = list(),
               Imports = list(), LinkingTo = list()),
          file.path(d, "Meta", "package.rds"))
  writeLines(paste0(names(desc), ": ", desc), file.path(d, "DESCRIPTION"))
}

skipTestSetup <- function(env = parent.frame()) {
  skip_if_not_installed("reproducible")
  skip_if_not_installed("qs2")
  lib <- withr::local_tempdir(.local_envir = env)
  withr::local_options(list(reproducible.cachePath = withr::local_tempdir(.local_envir = env),
                            reproducible.cacheSaveFormat = "qs2",
                            spades.useRequire = TRUE, spades.useRequireOverride = NULL,
                            SpaDES.project.skipIdenticalRequire = TRUE),
                       .local_envir = env)
  fakeInstall(lib, "fakeA", imports = "fakeB")
  fakeInstall(lib, "fakeB")
  fakeInstall(lib, "SpaDES.core") # setupPackages() always adds it
  lib
}

callSP <- function(lib, packages = "fakeA", ...) {
  setupPackages(packages, modulePackages = list(), require = list(), paths = list(),
                libPaths = lib, standAlone = FALSE, setLinuxBinaryRepo = FALSE,
                envir = new.env(), callingEnv = new.env(), verbose = 1, ...)
}

countRequire <- function(env = parent.frame()) {
  n <- 0L
  testthat::local_mocked_bindings(
    Require = function(packages, ...) { n <<- n + 1L; seq_along(packages) },
    .package = "Require", .env = env)
  function() n
}

test_that("identical setupPackages calls run Require once", {
  lib <- skipTestSetup()
  calls <- countRequire()
  callSP(lib); expect_equal(calls(), 1L)
  expect_message(callSP(lib), "skipping Require")
  expect_equal(calls(), 1L)
})

test_that("a changed version of a requested package or its dependency runs Require", {
  lib <- skipTestSetup()
  calls <- countRequire()
  callSP(lib); callSP(lib)
  expect_equal(calls(), 1L)
  fakeInstall(lib, "fakeB", version = "2.0") # a dependency
  callSP(lib)
  expect_equal(calls(), 2L)
  callSP(lib)
  expect_equal(calls(), 2L) # recorded after the previous call
  fakeInstall(lib, "fakeA", version = "1.1")
  callSP(lib)
  expect_equal(calls(), 3L)
})

test_that("an unrelated package added to the library is still a hit", {
  lib <- skipTestSetup()
  calls <- countRequire()
  callSP(lib)
  fakeInstall(lib, "fakeUnrelated")
  callSP(lib)
  expect_equal(calls(), 1L)
})

test_that("a missing package always runs Require, and is not recorded", {
  lib <- skipTestSetup()
  calls <- countRequire()
  callSP(lib, c("fakeA", "fakeMissing"))
  callSP(lib, c("fakeA", "fakeMissing"))
  expect_equal(calls(), 2L)
})

test_that("(HEAD) packages still go to Require", {
  lib <- skipTestSetup()
  seen <- list()
  testthat::local_mocked_bindings(
    Require = function(packages, ...) { seen[[length(seen) + 1L]] <<- packages; seq_along(packages) },
    .package = "Require")
  callSP(lib, c("fakeA", "fakeB (HEAD)"))
  callSP(lib, c("fakeA", "fakeB (HEAD)"))
  expect_length(seen, 2L)
  expect_equal(seen[[2]], "fakeB (HEAD)")
})

test_that("the option turns the shortcut off", {
  lib <- skipTestSetup()
  options(SpaDES.project.skipIdenticalRequire = FALSE)
  calls <- countRequire()
  callSP(lib); callSP(lib)
  expect_equal(calls(), 2L)
})
