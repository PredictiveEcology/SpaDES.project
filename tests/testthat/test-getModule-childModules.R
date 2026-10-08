## A parent module lists its children in `childModules`, as GitHub specs with branches
## ("owner/repo@branch") or as plain names. getModule() fetches them after the parent:
## a spec as written, a plain name from the parent's account and branch. Offline: a fake
## "GitHub" of local folders stands in for downloadGHRepoOuter().

## remote/<acct>/<repo>@<br>/<repo>/<repo>.R
mkRemoteModule <- function(remote, spec, childModules = character(), reqdPkgs = '"fs"',
                           version = "1.0.0", childVersions = character()) {
  name <- Require::extractPkgName(spec)
  d <- file.path(remote, sub("/", "__", spec, fixed = TRUE), name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  vers <- c(setNames(version, name), childVersions)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf("  version = list(%s),", paste0(names(vers), ' = "', vers, '"', collapse = ", ")),
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

test_that("a 'name@branch' child keeps its branch and takes the parent's account", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkRemoteModule(remote, "Acct/par@dev", childModules = "kidC@modsForX")
  mkRemoteModule(remote, "Acct/kidC@modsForX", version = "2.0.0")
  mkRemoteModule(remote, "Acct/kidC@dev", version = "3.0.0")
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par@dev", modulePath = mp, verbose = -1)

  expect_setequal(calls$specs, c("Acct/par@dev", "Acct/kidC@modsForX"))
  expect_identical(versionOf(mp, "kidC"), "2.0.0")
  expect_length(out$failed, 0L)
})

test_that("a module listed with its own branch overrides the parent's entry, and simInit gets the parent only", {
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  remote <- withr::local_tempdir()
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  mkRemoteModule(remote, "Acct/par@dev", childModules = "kidB")
  mkRemoteModule(remote, "Acct/kidB@dev", version = "2.0.0")
  mkRemoteModule(remote, "Acct/kidB@testing", version = "4.0.0")
  calls <- localFakeGitHub(remote)

  out <- suppressMessages(
    setupProject(modules = c("Acct/par@dev", "Acct/kidB@testing"),
                 paths = list(modulePath = mp, projectPath = file.path(root, "proj"),
                              packagePath = .libPaths()[1L]),
                 useGit = FALSE, updateRprofile = FALSE, verbose = -1))

  expect_true("Acct/kidB@testing" %in% calls$specs)
  expect_false("Acct/kidB@dev" %in% calls$specs)
  expect_identical(versionOf(mp, "kidB"), "4.0.0")
  expect_identical(unname(out$modules), "par")
})

## A real (parsable, runnable) minimal SpaDES module in the fake remote.
mkSpadesModule <- function(remote, spec, childModules = character()) {
  name <- Require::extractPkgName(spec)
  d <- file.path(remote, sub("/", "__", spec, fixed = TRUE), name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      '  description = "test", keywords = "test", authors = person("A", "B", role = c("aut", "cre")),',
      sprintf('  childModules = %s,', paste(deparse(childModules), collapse = "")),
      '  version = list(x = "1.0.0"), timeframe = as.POSIXlt(c(NA, NA)),',
      '  timeunit = "year", citation = list(), documentation = list(),',
      '  reqdPkgs = list(),',
      '  parameters = bindrows(defineParameter(".plots", "character", "screen", NA, NA, "")),',
      '  inputObjects = bindrows(), outputObjects = bindrows()',
      "))",
      sprintf("doEvent.%s <- function(sim, eventTime, eventType) {", name),
      "  if (eventType == 'init') sim <- scheduleEvent(sim, time(sim) + 1, currentModule(sim), 'step')",
      "  if (eventType == 'step') sim <- scheduleEvent(sim, time(sim) + 1, currentModule(sim), 'step')",
      "  invisible(sim)",
      "}"),
    file.path(d, paste0(name, ".R"))
  )
}

test_that("simInit on a fetched parent runs its leaf children only, as if they were listed", {
  skip_if_not_installed("SpaDES.core")
  skip_if(packageVersion("SpaDES.core") < "3.2.1.9026")
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  remote <- withr::local_tempdir()
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  mkSpadesModule(remote, "Acct/par@dev", childModules = c("kidA", "kidB@feat", "Other/kidD@main"))
  mkSpadesModule(remote, "Acct/kidA@dev")
  mkSpadesModule(remote, "Acct/kidB@feat")
  mkSpadesModule(remote, "Other/kidD@main")
  calls <- localFakeGitHub(remote)

  out <- suppressMessages(
    setupProject(modules = "Acct/par@dev",
                 paths = list(modulePath = mp, projectPath = file.path(root, "proj"),
                              packagePath = .libPaths()[1L]),
                 times = list(start = 0, end = 2),
                 useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_setequal(calls$specs, c("Acct/par@dev", "Acct/kidA@dev", "Acct/kidB@feat", "Other/kidD@main"))

  kids <- c("kidA", "kidB", "kidD")
  sim <- suppressMessages(SpaDES.core::simInit(modules = out$modules, times = out$times,
                                               paths = list(modulePath = mp)))
  simKids <- suppressMessages(SpaDES.core::simInit(modules = kids, times = out$times,
                                                   paths = list(modulePath = mp)))

  expect_setequal(unlist(SpaDES.core::modules(sim)), kids)
  expect_false("par" %in% names(SpaDES.core::depends(sim)@dependencies))
  expect_false("par" %in% names(SpaDES.core::params(sim)))
  expect_false("par" %in% SpaDES.core::events(sim)$moduleName)
  expect_setequal(names(SpaDES.core::depends(sim)@dependencies), kids)
  expect_identical(SpaDES.core::events(sim), SpaDES.core::events(simKids))
  expect_identical(sort(unlist(SpaDES.core::modules(sim))), sort(unlist(SpaDES.core::modules(simKids))))
})

## A parent release: fetched at a version tag, its plain-named children come at the version
## its own `version` list gives them, so "v1.1.0" of the parent means one set of child releases.
mkReleasedFamily <- function(remote, parentRef) {
  mkRemoteModule(remote, paste0("Acct/par@", parentRef), version = "1.1.0",
                 childModules = c("kidA", "kidB", "kidC@modsForX", "kidD"),
                 childVersions = c(kidA = "2.2.0", kidB = "1.2.0", kidC = "9.9.9"))
  mkRemoteModule(remote, "Acct/kidA@v2.2.0", version = "2.2.0")
  mkRemoteModule(remote, "Acct/kidB@v1.2.0", version = "1.2.0")
  mkRemoteModule(remote, "Acct/kidC@modsForX", version = "5.0.0")
  mkRemoteModule(remote, "Acct/kidD@v1.1.0", version = "0.3.0")
  for (k in c("kidA", "kidB", "kidD")) mkRemoteModule(remote, paste0("Acct/", k, "@dev"), version = "7.0.0")
}

test_that("a parent at a version tag fetches each plain-named child at its version in the parent's list", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkReleasedFamily(remote, "v1.1.0")
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par@v1.1.0", modulePath = mp, verbose = -1)

  ## kidC keeps its own branch; kidD is not in the list, so it takes the parent's tag
  expect_setequal(calls$specs, c("Acct/par@v1.1.0", "Acct/kidA@v2.2.0", "Acct/kidB@v1.2.0",
                                 "Acct/kidC@modsForX", "Acct/kidD@v1.1.0"))
  expect_identical(versionOf(mp, "kidA"), "2.2.0")
  expect_identical(versionOf(mp, "kidB"), "1.2.0")
  expect_identical(versionOf(mp, "kidC"), "5.0.0")
  expect_length(out$failed, 0L)
})

test_that("a parent at a branch still passes its branch to its children, whatever its version list says", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkReleasedFamily(remote, "dev")
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par@dev", modulePath = mp, verbose = -1)

  expect_setequal(calls$specs, c("Acct/par@dev", "Acct/kidA@dev", "Acct/kidB@dev",
                                 "Acct/kidC@modsForX", "Acct/kidD@dev"))
  expect_identical(versionOf(mp, "kidA"), "7.0.0")
})

test_that("a parent with no ref fetches its children at the releases its list names", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkReleasedFamily(remote, "HEAD")  # no ref is fetched as @HEAD, the default branch
  mkRemoteModule(remote, "Acct/kidD@HEAD", version = "0.4.0")
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par", modulePath = mp, verbose = -1)

  expect_setequal(calls$specs, c("Acct/par@HEAD", "Acct/kidA@v2.2.0", "Acct/kidB@v1.2.0",
                                 "Acct/kidC@modsForX", "Acct/kidD@HEAD"))
  expect_identical(versionOf(mp, "kidA"), "2.2.0")
  expect_length(out$failed, 0L)
})

test_that("a child listed at a development version falls back to the parent's ref", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  mkRemoteModule(remote, "Acct/par@v1.1.0", version = "1.1.0", childModules = "kidA",
                 childVersions = c(kidA = "2.2.0.9000"))
  mkRemoteModule(remote, "Acct/kidA@v1.1.0", version = "1.0.0")
  calls <- localFakeGitHub(remote)

  out <- getModule("Acct/par@v1.1.0", modulePath = mp, verbose = -1)

  expect_setequal(calls$specs, c("Acct/par@v1.1.0", "Acct/kidA@v1.1.0"))
})

test_that(".isVersionTag tells a release tag from a branch", {
  expect_true(.isVersionTag("v1.1.0"))
  expect_true(.isVersionTag("v2"))
  expect_false(.isVersionTag("development"))
  expect_false(.isVersionTag("main"))
  expect_false(.isVersionTag("fireSense-1.1.0"))
  expect_false(.isVersionTag("v1.1.0-beta"))
  expect_false(.isVersionTag(NA_character_))
  expect_true(.isReleaseVersion("2.2.0"))
  expect_false(.isReleaseVersion("2.2.0.9000"))
  expect_false(.isReleaseVersion(NA_character_))
  expect_false(.isReleaseVersion(NULL))
})
