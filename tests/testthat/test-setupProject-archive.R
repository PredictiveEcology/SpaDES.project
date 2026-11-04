## setupProject(archive = ): the modules an archived run already ran are neither fetched nor
## have their packages installed. Offline: a local CacheGeo ledger, the archived simList's
## module list mocked, GitHub mocked.

## A ledger of three runs in `dir`, as outSaveTarUpload() writes them
archiveLedger <- function(dir) {
  sq <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
                                crs = 4326))
  led <- reproducible::CacheGeoLedger("runs.rds", destinationPath = dir, key = "runName")
  for (r in list(list("A_NRV_1", "A", 1), list("A_NRV_2", "A", 2), list("B_NRV_1", "B", 1)))
    reproducible::CacheGeoWrite(led, outLedgerRow(r[[1]], driveId = paste0("id_", r[[1]]), saveTimes = 0,
                                                  area = sf::st_geometry(sq), outputPath = "/o",
                                                  scenario = list(ELF = r[[2]], rep = r[[3]])),
                                verbose = 0)
  led
}

ranModules <- function(...) data.frame(module = c(...), version = "1.0.0")
## the saved simList shell stands in as a list; its "fetchOutputs" attribute is what simInit() gets
archiveShell <- function() structure(list("shell"), fetchOutputs = function(files) files)

test_that("archive$scenario chooses one ledger run; none or several stop, listing them", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  d <- withr::local_tempdir()
  archiveLedger(d)
  local_mocked_bindings(.archiveShell = function(run, destDir) archiveShell(),
                        .archiveRanModules = function(sim) ranModules("k1"))

  out <- .archiveRun(list(ledger = "runs.rds", scenario = list(ELF = "A", rep = 2)), destDir = d, verbose = 0)
  expect_identical(out$run$runName, "A_NRV_2")
  expect_identical(out$modules$module, "k1")
  expect_identical(out$sim, archiveShell(), ignore_attr = TRUE)
  expect_identical(out$fetch, attr(out$sim, "fetchOutputs"))

  expect_error(.archiveRun(list(ledger = "runs.rds", scenario = list(ELF = "A")), destDir = d, verbose = 0),
               "matches 2.*A_NRV_1.*A_NRV_2")
  err <- tryCatch(.archiveRun(list(ledger = "runs.rds", scenario = list(ELF = "C")), destDir = d, verbose = 0),
                  error = conditionMessage)
  expect_match(err, "matches 0")
  expect_match(err, "B_NRV_1")
  expect_error(.archiveRun(list(ledger = "runs.rds", scenario = list(GCM = "x")), destDir = d, verbose = 0),
               "does not have: GCM")
})

## par -> k1, k2 (both ran); par2 -> k1, k3 (k3 did not); x ran; birds did not
archiveModules <- function(mp) {
  diWriteModule(mp, "par", children = c("k1", "k2"))
  diWriteModule(mp, "par2", children = c("k1", "k3"))
  for (m in c("k1", "k2", "k3", "birds")) diWriteModule(mp, m)
}

test_that("setupModules skips a module the archive ran, by name, and a parent whose children all ran", {
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  archiveModules(mp)
  local_mocked_bindings(downloadGHRepoOuter = function(modToDL, ...) stop("fetched ", modToDL))
  arch <- list(run = data.frame(runName = "r1"), modules = ranModules("k1", "k2", "x"))

  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("par", "par2", "Acct/x@dev", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, archive = arch)
  expect_identical(names(pkgs), c("par2", "birds"))
  expect_identical(attr(pkgs, "archived"), c("par", "Acct/x@dev"))
  expect_false(dir.exists(file.path(mp, "x")))
})

test_that("a parent not held locally is judged from its .R file, fetched outside modulePath", {
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  remote <- withr::local_tempdir()
  ## top -> mid (a parent too) -> k1, k2
  diWriteModule(remote, "top", children = "Acct/mid@dev")
  diWriteModule(remote, "mid", children = c("k1", "k2"))
  diWriteModule(mp, "birds")
  asked <- new.env()
  local_mocked_bindings(
    getGithubFile = function(gitRepoFile, overwrite = FALSE, destDir = ".", verbose = 1) {
      asked$files <- c(asked$files, gitRepoFile)
      dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(remote, sub("\\.R$", "", basename(gitRepoFile)), basename(gitRepoFile)), destDir)
    },
    downloadGHRepoOuter = function(modToDL, ...) stop("fetched ", modToDL))
  arch <- list(run = data.frame(runName = "r1"), modules = ranModules("k1", "k2"))

  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("Acct/top@dev", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, archive = arch)
  expect_identical(names(pkgs), "birds")
  expect_identical(attr(pkgs, "archived"), "Acct/top@dev")
  expect_identical(asked$files, c("Acct/top@dev/top.R", "Acct/mid@dev/mid.R"))
  expect_identical(dir(mp), "birds")
})

test_that("setupProject(archive = NULL) is unchanged; with an archive it returns the run and the rest", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  archiveModules(mp)
  paths <- list(modulePath = mp, projectPath = file.path(root, "proj"), inputPath = file.path(root, "inputs"),
                packagePath = .libPaths()[1L])

  plain <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                         useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_identical(unname(plain$modules), c("par", "birds"))
  expect_false("archive" %in% names(plain))

  archiveLedger(file.path(root, "inputs", "archive"))
  local_mocked_bindings(.archiveShell = function(run, destDir) archiveShell(),
                        .archiveRanModules = function(sim) ranModules("k1", "k2"))
  out <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                       archive = list(ledger = "runs.rds", scenario = list(ELF = "B")),
                                       useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_identical(unname(out$modules), "birds")
  expect_identical(out$archive$run$runName, "B_NRV_1")
  expect_identical(out$archive$modules$module, c("k1", "k2"))
  expect_identical(out$archive$matched, "par")
  ## what SpaDES.core::simInit(archive = ) replays from
  expect_true(is.function(out$archive$fetch))
  expect_false(is.null(out$archive$sim))
})
