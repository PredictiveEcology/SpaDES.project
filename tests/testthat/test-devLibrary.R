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

## A dev library as newDevLibrary() makes it, without installing anything into it
mkDevDir <- function(stable, envir = parent.frame()) {
  dev <- file.path(paste0(stable, "_dev"), "20261104-000000-abc")
  dir.create(dev, recursive = TRUE)
  dev
}
filesIn <- function(...) sort(list.files(c(...), recursive = TRUE, all.files = TRUE, full.names = TRUE))

test_that("a packagePath of two paths is written to .Rprofile and paths$packagePath is the first one", {
  setupTest()
  stable <- mkStable()
  dev <- mkDevDir(stable)
  prj <- withr::local_tempdir()
  writeLines("# project profile", file.path(prj, ".Rprofile"))
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  paths <- suppressWarnings(setupPaths(paths = list(projectPath = prj, packagePath = c(dev, stable),
                                                    modulePath = "modules"),
                                       updateRprofile = TRUE, verbose = -2))
  expect_true(any(grepl("setLibPaths", readLines(file.path(prj, ".Rprofile")))))
  expect_identical(normalizePath(paths$packagePath), normalizePath(dev))
})

test_that("a worker's spades.packagePath is its .libPaths(); nothing is written to the libraries or .Rprofile", {
  setupTest()
  stable <- mkStable()
  dev <- mkDevDir(stable)
  prj <- withr::local_tempdir()
  rprof <- c("# project profile", "options(foo = 1)")
  writeLines(rprof, file.path(prj, ".Rprofile"))
  withr::local_dir(prj)
  withr::local_libpaths(.libPaths())
  ## what the launcher's worker start-up lines set (.devLibraryStartup()), and the worker's environment
  withr::local_options(spades.packagePath = c(dev, stable), SpaDES.project.updateRprofile = FALSE,
                       spades.useRequire = NULL)
  withr::local_envvar(SPADES_USE_REQUIRE = "false")
  before <- filesIn(dev, stable)
  paths <- suppressWarnings(setupPaths(paths = list(projectPath = prj, modulePath = "modules"), verbose = -2))
  expect_identical(normalizePath(.libPaths()[1:2]), normalizePath(c(dev, stable)))
  expect_identical(normalizePath(paths$packagePath), normalizePath(dev))
  expect_identical(readLines(file.path(prj, ".Rprofile")), rprof)
  expect_identical(filesIn(dev, stable), before)
})

test_that("worker start-up lines carry the dev library: options and .libPaths()", {
  stable <- mkStable()
  dev <- mkDevDir(stable)
  libs <- c(dev, stable)
  expect_identical(SpaDES.project:::.devLibraryStartup(NULL), "")
  expect_identical(SpaDES.project:::.devLibraryStartup(stable), "")
  code <- SpaDES.project:::.devLibraryStartup(libs)
  withr::local_libpaths(.libPaths())
  withr::local_options(spades.packagePath = NULL, SpaDES.project.updateRprofile = NULL)
  eval(parse(text = code))
  expect_identical(getOption("spades.packagePath"), libs)
  expect_false(getOption("SpaDES.project.updateRprofile"))
  expect_identical(normalizePath(.libPaths()[1:2]), normalizePath(libs))

  ## tmux: first start and respawn go through .build_worker_r_expr()
  mk <- function(...) SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "global.R", on_interrupt = "requeue",
    runNameLabel = quote(colnames(q)[1]), activeRunningPath = "logs/q",
    ss_id = NULL, pane_mode = "killAndNewPane", email = NULL, cache_path = NULL,
    dots_path = NULL, ...)
  expect_true(grepl("spades.packagePath", mk(dev_library = libs, snapshot_library = FALSE), fixed = TRUE))
  expect_false(grepl("spades.packagePath", mk(), fixed = TRUE))
  expect_true(grepl("dev_library", mk(dev_library = libs), fixed = TRUE)) # so a respawned pane has it too
})

test_that("a launcher with devPackages builds a new dev library and its sbatch scripts carry it", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  stable <- mkStable()
  withr::local_libpaths(c(stable, .libPaths()))
  td <- withr::local_tempdir()
  g <- file.path(td, "global.R"); writeLines("invisible(NULL)", g)
  df <- data.frame(.ELFind = "job1", .rep = 1L, check.names = FALSE)
  go <- function(...) suppressMessages(experimentSBATCH(
    df = df, global_path = g, n_workers = 1L, queue_path = file.path(td, "q.rds"),
    log_dir = file.path(td, "logs"), dry_run = TRUE, ...))

  es <- go()  # no devPackages: as before
  expect_false(grepl("spades.packagePath", paste(readLines(unlist(es$job_scripts)[[1]]), collapse = "\n")))
  expect_false(dir.exists(paste0(stable, "_dev")))

  es <- go(devPackages = mkTinyPkg("tinyA", "1.0.0"), devInstallFun = installTiny)
  dev <- currentDevLibrary(stable)
  expect_length(dev, 2L)
  expect_true(dir.exists(file.path(dev[1], "tinyA")))
  expect_false(dir.exists(file.path(stable, "tinyA")))
  body <- paste(readLines(unlist(es$job_scripts)[[1]]), collapse = "\n")
  expect_true(grepl(SpaDES.project:::.devLibraryStartup(dev), body, fixed = TRUE))
})
