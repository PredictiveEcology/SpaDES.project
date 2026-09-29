## A parent module lists its children in `childModules`, as GitHub specs with branches
## ("owner/repo@branch") or as plain names. getModule() fetches them after the parent:
## a spec as written, a plain name from the parent's account and branch. Offline: a fake
## "GitHub" of local folders stands in for downloadGHRepoOuter().

## remote/<acct>/<repo>@<br>/<repo>/<repo>.R
mkRemoteModule <- function(remote, spec, childModules = character(), reqdPkgs = '"fs"',
                           version = "1.0.0") {
  name <- Require::extractPkgName(spec)
  d <- file.path(remote, sub("/", "__", spec, fixed = TRUE), name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf('  version = list(%s = "%s"),', name, version),
      sprintf("  childModules = %s,", paste(deparse(childModules), collapse = "")),
      sprintf("  reqdPkgs = list(%s)", reqdPkgs),
      "))"),
    file.path(d, paste0(name, ".R"))
  )
}

## Replace the GitHub download with a copy from `remote`; record what was asked for.
localFakeGitHub <- function(remote, envir = parent.frame()) {
  calls <- new.env()
  calls$specs <- character()
  local_mocked_bindings(
    downloadGHRepoOuter = function(modToDL, verbose, overwrite, modulePath) {
      calls$specs <- c(calls$specs, modToDL)
      src <- file.path(remote, sub("/", "__", modToDL, fixed = TRUE))
      if (dir.exists(src))
        file.copy(dir(src, full.names = TRUE), modulePath, recursive = TRUE)
    },
    .env = envir
  )
  calls
}

versionOf <- function(modulePath, name)
  metadataInModules(modules = name, metadataItem = "version", modulePath = modulePath)[[1]]

## A family: the parent lists kidA by spec+branch, kidB by plain name; kidB is itself a
## parent of kidC (plain name). Other branches hold different versions, to tell them apart.
mkFamily <- function(remote) {
  mkRemoteModule(remote, "Acct/par@dev", childModules = c("Other/kidA@feat", "kidB"))
  mkRemoteModule(remote, "Other/kidA@feat", version = "2.0.0", reqdPkgs = '"data.table"')
  mkRemoteModule(remote, "Other/kidA@main", version = "3.0.0")
  mkRemoteModule(remote, "Acct/kidB@dev", version = "2.0.0", childModules = "kidC")
  mkRemoteModule(remote, "Acct/kidB@main", version = "3.0.0")
  mkRemoteModule(remote, "Acct/kidC@dev", version = "2.0.0")
}

test_that("getModule fetches a parent's children: a spec as written, a plain name from the parent", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkFamily(remote)
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par@dev", modulePath = mp, verbose = -1)

  expect_setequal(calls$specs, c("Acct/par@dev", "Other/kidA@feat", "Acct/kidB@dev", "Acct/kidC@dev"))
  ## each lands in <modulePath>/<module name>, from the right branch
  expect_setequal(dir(mp), c("par", "kidA", "kidB", "kidC"))
  expect_identical(versionOf(mp, "kidA"), "2.0.0")
  expect_identical(versionOf(mp, "kidB"), "2.0.0")
  expect_identical(versionOf(mp, "kidC"), "2.0.0")
  expect_setequal(out$success, c("Acct/par@dev", "Other/kidA@feat", "Acct/kidB@dev", "Acct/kidC@dev"))
  expect_length(out$failed, 0L)
})

test_that("an explicit module entry wins over the parent's spec for it", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkFamily(remote)
  calls <- localFakeGitHub(remote)

  out <- getModule(c("Acct/par@dev", "Other/kidA@main", "Acct/kidB@main"), modulePath = mp,
                   verbose = -1)

  expect_false("Other/kidA@feat" %in% calls$specs)
  expect_false("Acct/kidB@dev" %in% calls$specs)
  expect_identical(versionOf(mp, "kidA"), "3.0.0")
  expect_identical(versionOf(mp, "kidB"), "3.0.0")
  ## kidB@main has no children, so kidC is not fetched
  expect_false(dir.exists(file.path(mp, "kidC")))
  expect_length(out$failed, 0L)
})

test_that("a cycle in childModules stops", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkRemoteModule(remote, "Acct/par@dev", childModules = "Acct/kid@dev")
  mkRemoteModule(remote, "Acct/kid@dev", childModules = c("par", "kid"))
  calls <- localFakeGitHub(remote)

  expect_no_error(out <- getModule("Acct/par@dev", modulePath = mp, verbose = -1))
  expect_identical(calls$specs, c("Acct/par@dev", "Acct/kid@dev"))
  expect_setequal(dir(mp), c("par", "kid"))
})

test_that("children already on disk are not fetched again", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkFamily(remote)
  calls <- localFakeGitHub(remote)
  getModule("Acct/par@dev", modulePath = mp, verbose = -1)
  calls$specs <- character()

  out <- getModule("Acct/par@dev", modulePath = mp, verbose = -1)
  expect_length(calls$specs, 0L)
  expect_length(out$failed, 0L)
})

test_that("setupModules gives a parent its children's packages, under the parent's name only", {
  withr::local_options(Require.updateRprofile = NULL)
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  proj <- withr::local_tempdir()
  mkFamily(remote)
  calls <- localFakeGitHub(remote)

  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = proj),
                       modules = "Acct/par@dev", inProject = TRUE, useGit = FALSE,
                       updateRprofile = FALSE, verbose = -1)
  expect_identical(names(pkgs), "Acct/par@dev")
  expect_setequal(unique(pkgs[["Acct/par@dev"]]), c("fs", "data.table"))
})

test_that("setupParams keeps params for a parent's children", {
  mp <- withr::local_tempdir()
  remote <- withr::local_tempdir()
  mkFamily(remote)
  calls <- localFakeGitHub(remote)
  getModule("Acct/par@dev", modulePath = mp, verbose = -1)

  params <- setupParams(params = list(kidA = list(a = 1), kidC = list(c = 3), notAModule = list(z = 1)),
                        paths = list(modulePath = mp), modules = c("Acct/par@dev" = "par"),
                        times = list(start = 0, end = 1), verbose = -1)
  expect_setequal(setdiff(names(params), ".globals"), c("kidA", "kidC"))
})

test_that("setupProject passes the parent, not its children, on to simInit", {
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  remote <- withr::local_tempdir()
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  mkFamily(remote)
  calls <- localFakeGitHub(remote)

  out <- suppressMessages(
    setupProject(modules = c("Acct/par@dev", "Other/kidA@main"),
                 paths = list(modulePath = mp, projectPath = file.path(root, "proj"),
                              packagePath = .libPaths()[1L]),
                 params = list(kidA = list(a = 1), kidB = list(b = 2)),
                 useGit = FALSE, updateRprofile = FALSE, verbose = -1))

  ## kidA was listed explicitly, but its parent brings it: simInit gets it once
  expect_identical(unname(out$modules), "par")
  expect_identical(versionOf(mp, "kidA"), "3.0.0")
  expect_setequal(setdiff(names(out$params), ".globals"), c("kidA", "kidB"))
})
