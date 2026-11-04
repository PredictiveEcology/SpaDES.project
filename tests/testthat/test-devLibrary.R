## Per-job libraries: an install builds a new dev library beside the frozen stable one, and a job is
## given c(<dev>, <stable>). Tiny local packages, installed with R CMD INSTALL; no network.

mkTinyPkg <- function(name, version, root = withr::local_tempdir(.local_envir = parent.frame())) {
  src <- file.path(root, paste0(name, "_", version), name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(paste0("Package: ", name), paste0("Version: ", version), "Title: Tiny",
               "Description: Tiny.", "License: GPL-3", "Authors@R: person('A', 'B', role = c('aut', 'cre'), email = 'a@b.c')"),
             file.path(src, "DESCRIPTION"))
  writeLines(paste0("export(f_", name, ")"), file.path(src, "NAMESPACE"))
  writeLines(sprintf("f_%s <- function() '%s'", name, version), file.path(src, "R", "f.R"))
  src
}

installTiny <- function(srcs, lib) {
  for (s in srcs) {
    st <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-docs", "--no-test-load", "-l", shQuote(lib), shQuote(s)),
                  stdout = FALSE, stderr = FALSE)
    if (st != 0L) stop("R CMD INSTALL failed for ", s)
  }
}
## Also the installFun given to newDevLibrary() (`packages` are source directories here). It is sent
## to another process, so it carries nothing of the test's environment.
environment(installTiny) <- globalenv()

mkStable <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  stable <- file.path(root, "packages", "4.6")
  dir.create(stable, recursive = TRUE)
  installTiny(mkTinyPkg("tinyS", "1.0.0", root), stable)
  normalizePath(stable)
}

inodeOf <- function(p) as.numeric(fs::file_info(p)$inode)
versionIn <- function(pkg, lib) unname(read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")[1, 1])

test_that("newDevLibrary builds a new library beside stable and leaves the previous one as it was", {
  skip_if_not_installed("callr")
  skip_if_not_installed("fs")
  skip_on_os("windows")
  stable <- mkStable()
  devRoot <- paste0(stable, "_dev")
  expect_identical(currentDevLibrary(stable, devRoot), stable)

  first <- newDevLibrary(c(mkTinyPkg("tinyA", "1.0.0"), mkTinyPkg("tinyB", "1.0.0")),
                         stable = stable, devRoot = devRoot, installFun = installTiny, verbose = 0)
  dev1 <- first[1]
  expect_identical(first[2], stable)
  expect_identical(normalizePath(dirname(dev1)), normalizePath(devRoot))
  expect_match(basename(dev1), "^[0-9]{8}-[0-9]{6}-")
  expect_identical(currentDevLibrary(stable, devRoot), first)
  expect_false(dir.exists(file.path(stable, "tinyA")))  # stable is never written to

  rdbA1 <- file.path(dev1, "tinyA", "R", "tinyA.rdb")
  rdbB1 <- file.path(dev1, "tinyB", "R", "tinyB.rdb")
  inoA1 <- inodeOf(rdbA1); inoB1 <- inodeOf(rdbB1)
  md5A1 <- unname(tools::md5sum(rdbA1))

  second <- newDevLibrary(mkTinyPkg("tinyA", "2.0.0"), stable = stable, devRoot = devRoot,
                          installFun = installTiny, verbose = 0)
  dev2 <- second[1]
  expect_false(identical(dev2, dev1))
  expect_identical(readLines(file.path(devRoot, "current")), dev2)
  expect_identical(currentDevLibrary(stable, devRoot), second)
  ## the changed package is new only in the new library; the old one is untouched, file for file
  expect_identical(versionIn("tinyA", dev2), "2.0.0")
  expect_identical(versionIn("tinyA", dev1), "1.0.0")
  expect_identical(inodeOf(rdbA1), inoA1)
  expect_identical(unname(tools::md5sum(rdbA1)), md5A1)
  expect_false(identical(inodeOf(file.path(dev2, "tinyA", "R", "tinyA.rdb")), inoA1))
  ## the unchanged package is the same files, linked
  expect_identical(inodeOf(file.path(dev2, "tinyB", "R", "tinyB.rdb")), inoB1)
})

test_that("newDevLibrary with nothing to install keeps the current library and leaves no directory", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  stable <- mkStable()
  devRoot <- paste0(stable, "_dev")
  libs <- newDevLibrary(mkTinyPkg("tinyA", "1.0.0"), stable = stable, devRoot = devRoot,
                        installFun = installTiny, verbose = 0)
  before <- list.files(devRoot, all.files = TRUE, no.. = TRUE)
  same <- newDevLibrary("tinyA", stable = stable, devRoot = devRoot,
                        installFun = function(packages, libPath) NULL, verbose = 0)
  expect_identical(same, libs)
  expect_identical(list.files(devRoot, all.files = TRUE, no.. = TRUE), before)
  expect_identical(currentDevLibrary(stable, devRoot), libs)
  ## a failed install likewise
  expect_error(newDevLibrary("tinyZ", stable = stable, devRoot = devRoot,
                             installFun = function(packages, libPath) stop("no tinyZ"), verbose = 0))
  expect_identical(list.files(devRoot, all.files = TRUE, no.. = TRUE), before)
})

test_that("newDevLibrary's default install sees what stable already has and installs nothing", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  skip_if_offline()
  stable <- mkStable()
  devRoot <- paste0(stable, "_dev")
  ## A project's stable library has pak, which Require uses; without it Require links pak into the new library
  if (requireNamespace("pak", quietly = TRUE))
    SpaDES.project:::linkOrCopyFiles(find.package("pak"), toBaseDir = stable)
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))
  expect_identical(newDevLibrary("tinyS (>= 0.5)", stable = stable, devRoot = devRoot, verbose = 0), stable)
  expect_length(list.dirs(devRoot, recursive = FALSE), 0L)
})

test_that("sweepDevLibraries removes dev libraries not kept, never the current one", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  stable <- mkStable()
  devRoot <- paste0(stable, "_dev")
  mk <- function(v) newDevLibrary(mkTinyPkg("tinyA", v), stable = stable, devRoot = devRoot,
                                  installFun = installTiny, verbose = 0)
  d1 <- mk("1.0.0"); d2 <- mk("2.0.0"); d3 <- mk("3.0.0")
  other <- file.path(devRoot, "notADevLibrary"); dir.create(other)
  gone <- sweepDevLibraries(keep = d2, devRoot = devRoot)  # a pair, as a queue row names it
  expect_identical(normalizePath(gone, mustWork = FALSE), normalizePath(d1[1], mustWork = FALSE))
  expect_true(dir.exists(d2[1])); expect_true(dir.exists(d3[1])); expect_true(dir.exists(other))
  expect_identical(currentDevLibrary(stable, devRoot), d3)
})

test_that("a vector packagePath becomes .libPaths() and is not written to .Rprofile", {
  setupTest()
  stable <- mkStable()
  dev <- file.path(paste0(stable, "_dev"), "20261104-000000-abc")
  dir.create(dev, recursive = TRUE)
  writeLines(stable, file.path(dev, ".devLibrary_of"))
  prj <- withr::local_tempdir()
  rprof <- c("# project profile", "options(foo = 1)")
  writeLines(rprof, file.path(prj, ".Rprofile"))
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  paths <- suppressWarnings(setupPaths(paths = list(projectPath = prj, packagePath = c(dev, stable),
                                                    modulePath = "modules"),
                                       updateRprofile = TRUE, verbose = -2))
  expect_identical(normalizePath(.libPaths()), normalizePath(c(dev, stable, .Library)))
  expect_identical(normalizePath(paths$packagePath), normalizePath(c(dev, stable)))
  expect_identical(normalizePath(paths$modulePath), normalizePath(file.path(prj, "modules")))
  expect_identical(readLines(file.path(prj, ".Rprofile")), rprof)
})

test_that("packagePath given as one separated string equals the vector", {
  setupTest()
  stable <- mkStable()
  dev <- file.path(paste0(stable, "_dev"), "20261104-000000-abc")
  dir.create(dev, recursive = TRUE)
  prj <- withr::local_tempdir()
  rprof <- c("# project profile", "options(foo = 1)")
  writeLines(rprof, file.path(prj, ".Rprofile"))
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  seps <- c(";", if (.Platform$path.sep == ":") ":")
  for (sep in seps) {
    withr::local_libpaths(.libPaths())
    paths <- suppressWarnings(setupPaths(
      paths = list(projectPath = prj, packagePath = paste0(dev, sep, stable, sep),
                   modulePath = "modules"),
      updateRprofile = TRUE, verbose = -2))
    expect_identical(normalizePath(.libPaths()), normalizePath(c(dev, stable, .Library)))
    expect_identical(normalizePath(paths$packagePath), normalizePath(c(dev, stable)))
    expect_identical(readLines(file.path(prj, ".Rprofile")), rprof)
  }
})

test_that("splitPathList keeps a single path and drops empty pieces", {
  expect_identical(splitPathList("a"), "a")
  expect_identical(splitPathList(c("a", "b")), c("a", "b"))
  expect_identical(splitPathList(";a;;b;"), c("a", "b"))
  expect_identical(splitPathList("C:/a;D:/b"), c("C:/a", "D:/b"))
})

test_that("projectLibrary is the setupPaths default and honours spades.packagePath", {
  setupTest()
  prj <- withr::local_tempdir()
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  withr::local_options(spades.packagePath = NULL)
  expect_identical(projectLibrary("zzProj"), .libPathDefault("zzProj"))
  expect_identical(projectLibrary(), .libPathDefault(basename(getwd())))
  paths <- suppressWarnings(setupPaths(name = basename(prj), paths = list(projectPath = prj),
                                       updateRprofile = FALSE, verbose = -2))
  expect_identical(normalizePath(paths$packagePath), normalizePath(projectLibrary(basename(prj))))
  opt <- file.path(prj, "optLib")
  withr::local_options(spades.packagePath = opt)
  expect_identical(projectLibrary("zzProj"), opt)
})

test_that("worker launchers take no library snapshot of a dev library", {
  stable <- mkStable()
  dev <- file.path(paste0(stable, "_dev"), "20261104-000000-abc")
  dir.create(dev, recursive = TRUE)
  writeLines(stable, file.path(dev, ".devLibrary_of"))
  mk <- function(lib) SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "global.R", on_interrupt = "requeue",
    runNameLabel = quote(colnames(q)[1]), activeRunningPath = "logs/q",
    ss_id = NULL, pane_mode = "killAndNewPane", email = NULL, cache_path = NULL,
    dots_path = NULL, lib_path = lib, snapshot_library = TRUE)
  expect_false(grepl(".snapBase", mk(c(dev, stable)), fixed = TRUE))
  expect_true(grepl(".snapBase", mk(stable), fixed = TRUE))
})

test_that("setupPackages installs into no library when packagePath is several", {
  seen <- list()
  testthat::local_mocked_bindings(
    Require = function(packages, ...) { seen[[length(seen) + 1L]] <<- list(...); seq_along(packages) },
    .package = "Require")
  withr::local_options(spades.useRequire = TRUE, spades.useRequireOverride = NULL,
                       SpaDES.project.skipIdenticalRequire = FALSE)
  sp <- function(libPaths) setupPackages("tinyA", modulePackages = list(), require = list(), paths = list(),
                                         libPaths = libPaths, standAlone = TRUE, setLinuxBinaryRepo = FALSE,
                                         envir = new.env(), callingEnv = new.env(), verbose = -1)
  sp(c("/dev", "/stable"))
  expect_false(seen[[1]]$install)
  expect_false(seen[[1]]$standAlone)
  sp("/stable")                      # one library: as before
  expect_identical(seen[[2]]$install, getOption("Require.install", TRUE))
  expect_true(seen[[2]]$standAlone)
})
