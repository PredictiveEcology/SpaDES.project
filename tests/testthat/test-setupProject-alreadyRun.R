## setupProject(alreadyRun = ): the modules a run already ran are neither fetched nor
## have their packages installed. Offline: a local CacheGeo ledger, the archived simList's
## module list mocked, GitHub mocked.

## A ledger of three runs in `dir`, as outSaveTarUpload() writes them
alreadyRunLedger <- function(dir) {
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
## the saved simList shell stands in as a list, with the parent map SpaDES.core's simInit()
## records; its "fetchOutputs" attribute is what simInit() gets
alreadyRunShell <- function()
  structure(list(shell = "shell", ._modulesGiven = list(parents = list(top = "mid", mid = c("k1", "k2")))),
            fetchOutputs = function(files) files)

test_that("alreadyRun$scenario chooses one ledger run; none or several stop, listing them", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  d <- withr::local_tempdir()
  alreadyRunLedger(d)
  local_mocked_bindings(.alreadyRunShell = function(run, destDir) alreadyRunShell(),
                        .alreadyRunModules = function(sim) ranModules("k1"))

  out <- .alreadyRunRead(list(ledger = "runs.rds", scenario = list(ELF = "A", rep = 2), rerun = "k1"),
                         destDir = d, verbose = 0)
  expect_identical(out$rerun, "k1")
  expect_identical(out$run$runName, "A_NRV_2")
  expect_identical(out$modules$module, "k1")
  expect_identical(out$sim, alreadyRunShell(), ignore_attr = TRUE)
  expect_identical(out$fetch, attr(out$sim, "fetchOutputs"))
  expect_identical(out$parents, list(top = "mid", mid = c("k1", "k2")))

  expect_error(.alreadyRunRead(list(ledger = "runs.rds", scenario = list(ELF = "A")), destDir = d, verbose = 0),
               "matches 2.*A_NRV_1.*A_NRV_2")
  err <- tryCatch(.alreadyRunRead(list(ledger = "runs.rds", scenario = list(ELF = "C")), destDir = d, verbose = 0),
                  error = conditionMessage)
  expect_match(err, "matches 0")
  expect_match(err, "B_NRV_1")
  expect_error(.alreadyRunRead(list(ledger = "runs.rds", scenario = list(GCM = "x")), destDir = d, verbose = 0),
               "does not have: GCM")
})

## par -> k1, k2 (both ran); par2 -> k1, k3 (k3 did not); x ran; birds did not
alreadyRunModules <- function(mp) {
  diWriteModule(mp, "par", children = c("k1", "k2"))
  diWriteModule(mp, "par2", children = c("k1", "k3"))
  for (m in c("k1", "k2", "k3", "birds")) diWriteModule(mp, m)
}

test_that("setupModules skips a module the run ran, by name, and a parent whose children all ran", {
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  alreadyRunModules(mp)
  local_mocked_bindings(downloadGHRepoOuter = function(modToDL, ...) stop("fetched ", modToDL))
  arch <- list(run = data.frame(runName = "r1"), modules = ranModules("k1", "k2", "x"))

  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("par", "par2", "Acct/x@dev", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch)
  ## par2's k1 ran, so par2 is replaced by k3 only
  expect_identical(names(pkgs), c("k3", "birds"))
  expect_identical(attr(pkgs, "alreadyRun"), c("par", "k1", "Acct/x@dev"))
  expect_false(dir.exists(file.path(mp, "x")))
})

test_that("alreadyRun$rerun runs a module the run ran: alone, or the rest of its parent skipped", {
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  alreadyRunModules(mp)
  local_mocked_bindings(downloadGHRepoOuter = function(modToDL, ...) stop("fetched ", modToDL))
  arch <- list(run = data.frame(runName = "r1"), modules = ranModules("k1", "k2"), rerun = "k1")
  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("par", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch)
  expect_identical(names(pkgs), c("k1", "birds"))
  expect_identical(attr(pkgs, "alreadyRun"), "k2")
  ## every child rerun: the parent is kept whole
  arch$rerun <- c("k1", "k2")
  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("par", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch)
  expect_identical(names(pkgs), c("par", "birds"))
  expect_length(attr(pkgs, "alreadyRun"), 0)
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
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch)
  expect_identical(names(pkgs), "birds")
  expect_identical(attr(pkgs, "alreadyRun"), "Acct/top@dev")
  expect_identical(asked$files, c("Acct/top@dev/top.R", "Acct/mid@dev/mid.R"))
  expect_identical(dir(mp), "birds")
})

test_that("a parent not held locally is judged from the run's record of its children, not fetched", {
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  diWriteModule(mp, "birds")
  local_mocked_bindings(
    getGithubFile = function(gitRepoFile, ...) stop("fetched ", gitRepoFile),
    downloadGHRepoOuter = function(modToDL, ...) stop("fetched ", modToDL))
  arch <- list(run = data.frame(runName = "r1"), modules = ranModules("k1", "k2"),
               parents = alreadyRunShell()$._modulesGiven$parents)

  pkgs <- setupModules(paths = list(modulePath = mp, projectPath = root),
                       modules = c("Acct/top@dev", "birds"), inProject = TRUE,
                       useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch)
  expect_identical(names(pkgs), "birds")
  expect_identical(attr(pkgs, "alreadyRun"), "Acct/top@dev")
  expect_identical(dir(mp), "birds")

  ## a parent the record does not name is still fetched (a run saved before the record)
  arch$parents <- list(other = "k1")
  expect_error(setupModules(paths = list(modulePath = mp, projectPath = root),
                            modules = c("Acct/top@dev", "birds"), inProject = TRUE,
                            useGit = FALSE, updateRprofile = FALSE, verbose = -1, alreadyRun = arch),
               "fetched Acct/top@dev/top.R")
})

test_that("setupProject(alreadyRun = NULL) is unchanged; with a run already done it returns the run and the rest", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  alreadyRunModules(mp)
  paths <- list(modulePath = mp, projectPath = file.path(root, "proj"), inputPath = file.path(root, "inputs"),
                packagePath = .libPaths()[1L])

  plain <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                         useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_identical(unname(plain$modules), c("par", "birds"))
  expect_false("alreadyRun" %in% names(plain))
  ## an expression that gives NULL (e.g. `if (report) <runs>` in a run script) is none
  report <- FALSE
  plain <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                         alreadyRun = if (report) "run.rds",
                                         useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_false("alreadyRun" %in% names(plain))

  alreadyRunLedger(file.path(root, "inputs", "alreadyRun"))
  local_mocked_bindings(.alreadyRunShell = function(run, destDir) alreadyRunShell(),
                        .alreadyRunModules = function(sim) ranModules("k1", "k2"))
  out <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                       alreadyRun = list(ledger = "runs.rds", scenario = list(ELF = "B")),
                                       useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_identical(unname(out$modules), "birds")
  expect_identical(out$alreadyRun$run$runName, "B_NRV_1")
  expect_identical(out$alreadyRun$modules$module, c("k1", "k2"))
  expect_identical(out$alreadyRun$matched, "par")
  ## what SpaDES.core::simInit(alreadyRun = ) replays from
  expect_true(is.function(out$alreadyRun$fetch))
  expect_false(is.null(out$alreadyRun$sim))
})

test_that("arguments after paths can use alreadyRun, e.g. its studyArea and rasterToMatch", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  alreadyRunModules(mp)
  paths <- list(modulePath = mp, projectPath = file.path(root, "proj"), inputPath = file.path(root, "inputs"),
                packagePath = .libPaths()[1L])
  alreadyRunLedger(file.path(root, "inputs", "alreadyRun"))
  sa <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
  local_mocked_bindings(.alreadyRunShell = function(run, destDir)
                          c(alreadyRunShell(), list(studyArea = sa, rasterToMatch = "rtm")),
                        .alreadyRunModules = function(sim) ranModules("k1", "k2"))
  out <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                       alreadyRun = list(ledger = "runs.rds", scenario = list(ELF = "B")),
                                       studyArea = alreadyRun$sim$studyArea,
                                       rasterToMatch = alreadyRun$sim$rasterToMatch,
                                       useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  expect_identical(out$studyArea, sa)
  expect_identical(out$rasterToMatch, "rtm")
})

test_that("several runs: a list of ledger runs or saved simList files; one ledger run is not several", {
  expect_false(.alreadyRunIsMulti(list(ledger = "runs.rds", scenario = list(ELF = "A"))))
  expect_true(.alreadyRunIsMulti(c("a.rds", "b.rds")))
  expect_true(.alreadyRunIsMulti(list(a = "a.rds", b = list(file = "b.rds", pathRemap = c(old = "/x", new = "/y")))))
  expect_true(.alreadyRunIsMulti(list(list(ledger = "runs.rds", scenario = list(rep = 1)), "b.rds")))
  expect_false(.alreadyRunIsMulti(list(ledger = "runs.rds")))
  expect_error(.alreadyRunRead(list(ledger = "runs.rds"), destDir = tempdir(), verbose = 0),
               "or a list of runs")
})

test_that("setupProject with several ledger runs reads each, skips no module and returns them by name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  skip_if_not_installed("SpaDES.core")
  skip_if_not(exists(".alreadyRunMulti", envir = asNamespace("SpaDES.core"), inherits = FALSE),
              "SpaDES.core without several-run alreadyRun")
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL)
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  alreadyRunModules(mp)
  paths <- list(modulePath = mp, projectPath = file.path(root, "proj"), inputPath = file.path(root, "inputs"),
                packagePath = .libPaths()[1L])
  alreadyRunLedger(file.path(root, "inputs", "alreadyRun"))
  local_mocked_bindings(.alreadyRunShell = function(run, destDir) structure(list(run = run$runName)),
                        .alreadyRunModules = function(sim) ranModules("k1", "k2"))
  runs <- lapply(1:2, function(r) list(ledger = "runs.rds", scenario = list(ELF = "A", rep = r)))
  out <- suppressMessages(setupProject(modules = c("par", "birds"), paths = paths,
                                       alreadyRun = setNames(runs, c("r1", "r2")),
                                       useGit = FALSE, updateRprofile = FALSE, verbose = -1))
  ## par's children k1 and k2 ran in both runs; with several runs nothing is skipped
  expect_identical(unname(out$modules), c("par", "birds"))
  expect_identical(names(out$alreadyRun), c("r1", "r2"))
  expect_null(attr(out$alreadyRun, "multi"))
  expect_identical(vapply(out$alreadyRun, function(r) r$run$runName, ""), c(r1 = "A_NRV_1", r2 = "A_NRV_2"))
  expect_identical(out$alreadyRun$r2$sim$run, "A_NRV_2")
})

## End to end, with SpaDES.core: three real runs of `makesX`, saved as lazy simLists; one
## moved after it ran (pathRemap). setupProject(alreadyRun = <their files>), then
## simInitAndSpades2(): the summary module, which also ran in each run, reads every run's
## last saved x through outputsDF.
test_that("several saved runs: setupProject + simInitAndSpades2 summarise them", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not(exists(".alreadyRunMulti", envir = asNamespace("SpaDES.core"), inherits = FALSE),
              "SpaDES.core without several-run alreadyRun")
  withr::local_options(reproducible.verbose = 0, spades.moduleCodeChecks = FALSE)
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  diWriteModule(mp, "makesX", inputs = 'expectsInput("repId", "numeric", "")',
                outputs = c('createsOutput("x", "numeric", "")', 'createsOutput("w", "character", "")'),
                code = c('doEvent.makesX <- function(sim, eventTime, eventType) {',
                         '  if (eventType == "init") { sim$w <- "same"; sim <- scheduleEvent(sim, start(sim), "makesX", "make") }',
                         '  if (eventType == "make") { sim$x <- time(sim) * sim$repId',
                         '    sim <- scheduleEvent(sim, time(sim) + 1, "makesX", "make") }',
                         '  invisible(sim) }'))
  diWriteModule(mp, "summ",
                inputs = c('expectsInput("x", "numeric", "")', 'expectsInput("w", "character", "")',
                           'expectsInput("outputsDF", "data.table", "")'),
                code = c('doEvent.summ <- function(sim, eventTime, eventType) {',
                         '  if (eventType == "init" && NROW(sim$outputsDF)) {',
                         '    files <- sim$outputsDF$file',
                         '    dirs <- SpaDES.core::dirnamesFromSet(files, 1:9, leafOnly = FALSE)',
                         '    sim$lastX <- vapply(dirs, function(d) {',
                         '      f <- sort(grep(paste0("^", d, "/x_year"), files, value = TRUE))',
                         '      readRDS(f[length(f)]) }, numeric(1), USE.NAMES = FALSE)',
                         '    sim$wSeen <- sim$w }',
                         '  invisible(sim) }'))
  for (m in c("makesX", "summ")) { # a version under the module's own name, as a real module has
    f <- file.path(mp, m, paste0(m, ".R"))
    writeLines(sub("version = list(x = ", paste0("version = list(", m, " = "), readLines(f), fixed = TRUE), f)
  }
  files <- vapply(1:3, function(r) {
    op <- file.path(root, "runs", r)
    s <- SpaDES.core::simInitAndSpades(times = list(start = 0, end = 4), modules = c("makesX", "summ"),
                                       objects = list(repId = r),
                                       paths = list(modulePath = mp, outputPath = op,
                                                    inputPath = file.path(root, "inputs"),
                                                    cachePath = file.path(root, "cache"),
                                                    scratchPath = file.path(root, "scratch"),
                                                    rasterPath = file.path(root, "scratch", "raster"),
                                                    terraPath = file.path(root, "scratch", "terra")),
                                       outputs = data.frame(objectName = "x", saveTime = c(0, 2, 4)))
    f <- file.path(op, "run.rds")
    SpaDES.core::saveSimList(s, f, projectPath = op, files = FALSE, lazy = TRUE)
    f
  }, character(1))
  ## run 3 moved after it ran: its outputs() still name the old folder
  moved <- file.path(root, "moved", "3")
  dir.create(dirname(moved))
  file.rename(dirname(files[3]), moved)
  runs <- list(files[1], files[2], list(file = file.path(moved, "run.rds"),
                                         pathRemap = c(old = dirname(files[3]), new = moved)))
  setupTest()
  withr::local_options(spades.useRequire = FALSE, Require.updateRprofile = NULL, reproducible.verbose = 0,
                       spades.moduleCodeChecks = FALSE)
  paths <- list(modulePath = mp, projectPath = file.path(root, "proj"), inputPath = file.path(root, "inputs"),
                outputPath = file.path(root, "summary"), packagePath = .libPaths()[1L])
  out <- suppressMessages(setupProject(modules = "summ", paths = paths, times = list(start = 0, end = 1),
                                       alreadyRun = runs, useGit = FALSE, updateRprofile = FALSE,
                                       verbose = -1))
  expect_identical(unname(out$modules), "summ") # summ ran in each run, and is not skipped
  expect_identical(names(out$alreadyRun), c("1", "2", "3"))
  expect_true(all(startsWith(SpaDES.core::outputs(out$alreadyRun[["3"]]$sim)$file, moved)))
  sim <- suppressMessages(SpaDES.core::simInitAndSpades2(out))
  expect_identical(sim$lastX, c(4, 8, 12))
  expect_identical(sim$wSeen, "same")
  expect_identical(sort(unique(sim$outputsDF$run)), c("1", "2", "3"))
  expect_false("makesX" %in% SpaDES.core::completed(sim)$moduleName)
})
