## Tests for the re** family in R/download.R
## Covers: reUntar (incl. pathRemap), reLoad. reGet / reGetUntarLoad
## require Google Drive auth and are exercised only via integration use.

# Helper: build a fake outTar-style archive — single .rds at an absolute
# path inside the archive, so reUntar's `entries[[1L]]` logic applies.
.makeFakeTar <- function(payload, srcDir) {
  rds <- file.path(srcDir, "fake_sim.rds")
  saveRDS(payload, rds)
  tarball <- file.path(srcDir, "fake_sim.tar.gz")
  withr::with_dir(srcDir, {
    utils::tar(tarball, files = rds, compression = "gzip")
  })
  tarball
}

# reUntar uses GNU tar's --absolute-names / --transform; BSD tar (macOS)
# rejects those flags, so the tarball-extraction tests are GNU-tar only.
.skip_if_no_gnu_tar <- function() {
  tar_bin <- Sys.getenv("TAR", unset = "tar")
  v <- tryCatch(suppressWarnings(system2(tar_bin, "--version", stdout = TRUE,
                                         stderr = FALSE)),
                error = function(e) character())
  if (!any(grepl("GNU tar", v, fixed = TRUE)))
    testthat::skip("GNU tar not available (reUntar uses GNU-only flags)")
}

# ---------------------------------------------------------------------------
# reUntar
# ---------------------------------------------------------------------------

test_that("reUntar restores absolute paths and returns the sim file path", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(answer = 42L, name = "fake_sim")
  tarball <- .makeFakeTar(payload, src)
  rdsAbs  <- file.path(src, "fake_sim.rds")

  unlink(rdsAbs)  # force reUntar to recreate it
  expect_false(file.exists(rdsAbs))

  expect_message(out <- reUntar(tarball), "untarred fake_sim.tar.gz")

  expect_equal(out, rdsAbs)
  expect_true(file.exists(rdsAbs))
  expect_equal(readRDS(rdsAbs), payload)
})

test_that("reUntar with pathRemap rewrites the prefix", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(x = 1L)
  tarball <- .makeFakeTar(payload, src)

  newPrefix <- withr::local_tempdir()
  remap <- c(old = src, new = newPrefix)

  out <- reUntar(tarball, pathRemap = remap)

  expected <- file.path(newPrefix, "fake_sim.rds")
  expect_equal(out, expected)
  expect_true(file.exists(expected))
  expect_equal(readRDS(expected), payload)
})

test_that("reUntar with pathRemap expands ~ in new prefix", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(x = 2L)
  tarball <- .makeFakeTar(payload, src)

  # Stage a fake home so ~ expands somewhere we control and clean up
  fakeHome <- withr::local_tempdir()
  withr::local_envvar(HOME = fakeHome)
  remap <- c(old = src, new = "~/sub/dir")

  out <- reUntar(tarball, pathRemap = remap)

  expected <- file.path(fakeHome, "sub/dir", "fake_sim.rds")
  expect_equal(out, expected)
  expect_true(file.exists(expected))
  expect_equal(readRDS(expected), payload)
})

test_that("reUntar rejects malformed pathRemap", {
  src <- withr::local_tempdir()
  tarball <- .makeFakeTar(list(), src)

  expect_error(reUntar(tarball, pathRemap = c("a", "b")),
               "named character vector")
  expect_error(reUntar(tarball, pathRemap = c(foo = "a", bar = "b")),
               "named character vector")
})

test_that("reUntar is vectorised over tarballs", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  t1 <- .makeFakeTar(list(i = 1L), src)
  src2 <- withr::local_tempdir()
  t2 <- .makeFakeTar(list(i = 2L), src2)

  out <- reUntar(c(t1, t2))
  expect_length(out, 2L)
  expect_true(all(file.exists(out)))
})

# ---------------------------------------------------------------------------
# reLoad
# ---------------------------------------------------------------------------

test_that("reLoad with method='readRDS' returns a named list", {
  src <- withr::local_tempdir()
  f1 <- file.path(src, "a.rds"); saveRDS(list(v = 1L), f1)
  f2 <- file.path(src, "b.rds"); saveRDS(list(v = 2L), f2)

  expect_message(
    out <- reLoad(c(f1, f2), method = "readRDS"),
    "loaded a.rds via readRDS"
  )

  expect_named(out, c("a.rds", "b.rds"))
  expect_equal(out$a.rds$v, 1L)
  expect_equal(out$b.rds$v, 2L)
})

test_that("reLoad errors clearly on missing files", {
  expect_error(reLoad("/no/such/file.rds", method = "readRDS"))
})

test_that("reLoad rejects unknown method", {
  src <- withr::local_tempdir()
  f <- file.path(src, "x.rds"); saveRDS(1L, f)
  expect_error(reLoad(f, method = "bogus"))
})

# ---------------------------------------------------------------------------
# Lazy sidecar round trip: outTar -> reUntar -> reLoad
#
# The contract these pin down is split across two packages: SpaDES.core writes
# the sidecar and binds the promises, SpaDES.project carries the directory
# through the archive. PR #137 went red because the upload-side test asserted a
# format SpaDES.core had already stopped writing, with nothing covering the
# round trip itself.
# ---------------------------------------------------------------------------

.skip_if_no_lazy_sidecar <- function() {
  testthat::skip_if_not_installed("SpaDES.core")
  if (!exists(".lazyDirName", envir = asNamespace("SpaDES.core"), inherits = FALSE))
    testthat::skip("installed SpaDES.core predates the _lazy sidecar format")
}

# A real lazily saved simList. Returns the shell path, its sidecar directory,
# and the object values, so a test can compare after a round trip.
.makeLazySim <- function(dir, runName = "lazyrun") {
  withr::local_options(spades.moduleCodeChecks = FALSE, reproducible.verbose = 0)
  sim <- suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1),
                                               paths = list(outputPath = dir)))
  sim$alpha <- runif(50)
  sim$beta  <- letters
  f <- file.path(dir, paste0(runName, ".rds"))
  suppressMessages(SpaDES.core::saveSimList(sim, filename = f, lazy = TRUE))
  list(simFile = f,
       lazyDir = file.path(dir, paste0(runName, "_lazy")),
       objs    = list(alpha = sim$alpha, beta = sim$beta))
}

test_that("outTar -> reUntar restores the _lazy sidecar directory", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)
  expect_true(dir.exists(s$lazyDir))
  sidecars <- sort(dir(s$lazyDir))

  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))

  ## wipe both, exactly as a fresh machine would be
  unlink(s$simFile); unlink(s$lazyDir, recursive = TRUE)
  expect_false(file.exists(s$simFile))

  withr::with_dir(tempdir(), suppressMessages(reUntar(tb)))
  expect_true(file.exists(s$simFile))
  expect_true(dir.exists(s$lazyDir))
  expect_identical(sort(dir(s$lazyDir)), sidecars)
})

test_that("reUntar keeps the _lazy/ path component loadSimList keys on", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)
  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))

  entries <- utils::untar(tb, list = TRUE, extras = "-P")
  ## loadSimList() separates sidecars from ordinary files with
  ## grepl("(^|/)<base>_lazy/", ...) -- so the component must survive tar.
  expect_true(any(grepl("(^|/)lazyrun_lazy/", entries)))
  ## and the shell must stay first: reUntar takes entries[[1L]] as the simList
  expect_identical(basename(entries[[1L]]), "lazyrun.rds")
})

test_that("a lazily saved simList survives the round trip as promises", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  skip_if_not_installed("rlang")
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)
  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))
  unlink(s$simFile); unlink(s$lazyDir, recursive = TRUE)

  sp <- withr::with_dir(tempdir(), suppressMessages(reUntar(tb)))
  sim <- suppressMessages(reLoad(sp))[[1L]]

  ## still promises before anything touches them -- the whole point of lazy
  expect_true(all(rlang::env_binding_are_lazy(sim@.xData, c("alpha", "beta"))))
  ## and correct once forced
  expect_identical(sim$alpha, s$objs$alpha)
  expect_identical(sim$beta,  s$objs$beta)
})

test_that("reUntar with pathRemap moves the _lazy directory with the sim", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  td <- withr::local_tempdir()
  src <- file.path(td, "src"); dir.create(src)
  s <- .makeLazySim(src)
  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))

  dest <- file.path(td, "dest")
  sp <- withr::with_dir(tempdir(),
                        suppressMessages(reUntar(tb, pathRemap = c(old = src, new = dest))))

  expect_identical(normalizePath(dirname(sp)), normalizePath(dest))
  ## the sidecar has to follow, or the reloaded sim is a shell of dead promises
  expect_true(dir.exists(file.path(dest, "lazyrun_lazy")))
  sim <- suppressMessages(reLoad(sp))[[1L]]
  expect_identical(sim$alpha, s$objs$alpha)
})

# ---------------------------------------------------------------------------
# The Google Drive half, mocked -- no network, no auth
# ---------------------------------------------------------------------------

test_that("reGet returns one row per file and defers to preProcess", {
  td <- withr::local_tempdir()
  f1 <- file.path(td, "a.tar.gz"); writeLines("a", f1)
  f2 <- file.path(td, "b.tar.gz"); writeLines("b", f2)
  seen <- character(0)

  testthat::local_mocked_bindings(
    preProcess = function(url, ...) {
      seen <<- c(seen, url)
      list(targetFilePath = if (url == "id1") f1 else f2)
    },
    .package = "reproducible"
  )

  out <- suppressMessages(reGet(c("id1", "id2"), destDir = td, verbose = FALSE))
  expect_s3_class(out, "data.table")
  expect_identical(nrow(out), 2L)
  expect_identical(out$local_path, c(f1, f2))
  expect_identical(seen, c("id1", "id2"))   # ids forwarded, in order
})

test_that("reGetUntarLoad drives the whole chain without touching Drive", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  skip_if_not_installed("rlang")
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)
  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))
  unlink(s$simFile); unlink(s$lazyDir, recursive = TRUE)

  ## the only Drive touchpoint in the chain is reGet()'s preProcess call
  testthat::local_mocked_bindings(
    preProcess = function(url, ...) list(targetFilePath = tb),
    .package = "reproducible"
  )

  sims <- withr::with_dir(
    tempdir(),
    suppressMessages(reGetUntarLoad("someDriveId", destDir = td, verbose = FALSE))
  )

  expect_length(sims, 1L)
  expect_named(sims, "lazyrun")            # archive name, sans .tar.gz
  expect_true(all(rlang::env_binding_are_lazy(sims[[1L]]@.xData, c("alpha", "beta"))))
  expect_identical(sims[[1L]]$alpha, s$objs$alpha)
})

test_that("outUpload hands the tarball to drive_upload and honours cleanup", {
  skip_if_not_installed("googledrive")
  td <- withr::local_tempdir()
  tb <- file.path(td, "run.tar.gz"); writeLines("x", tb)
  args <- NULL

  testthat::local_mocked_bindings(
    drive_upload = function(media, path, name, overwrite) {
      args <<- list(media = media, path = path, name = name, overwrite = overwrite)
      "fake-dribble"
    },
    .package = "googledrive"
  )

  res <- suppressMessages(outUpload(tb, gFolder = "folderId", cleanup = FALSE))
  expect_identical(res, "fake-dribble")
  expect_identical(args$name, "run.tar.gz")
  expect_identical(args$path, "folderId")
  expect_true(args$overwrite)
  expect_true(file.exists(tb))             # cleanup = FALSE leaves it alone

  suppressMessages(outUpload(tb, gFolder = "folderId", cleanup = TRUE))
  expect_false(file.exists(tb))
})

test_that("outUpload requires a gFolder", {
  expect_error(outUpload("x.tar.gz", gFolder = NULL), "gFolder must be supplied")
})

# ---------------------------------------------------------------------------
# reLoad(parse = ) -- skipping the module re-parse
# ---------------------------------------------------------------------------

test_that("reLoad(parse = FALSE) forwards reparse to loadSimList", {
  .skip_if_no_lazy_sidecar()
  skip_if(!"reparse" %in% names(formals(SpaDES.core::loadSimList)),
          "installed SpaDES.core has no `reparse` argument")
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)

  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    loadSimList = function(filename, ..., reparse = TRUE) {
      seen$reparse <- reparse
      readRDS(filename)
    },
    .package = "SpaDES.core"
  )

  suppressMessages(reLoad(s$simFile, parse = FALSE))
  expect_false(seen$reparse)

  suppressMessages(reLoad(s$simFile))          # default
  expect_true(seen$reparse)
})

test_that("reLoad(parse = FALSE) still yields lazily bound objects", {
  .skip_if_no_lazy_sidecar()
  skip_if_not_installed("rlang")
  skip_if(!"reparse" %in% names(formals(SpaDES.core::loadSimList)),
          "installed SpaDES.core has no `reparse` argument")
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)

  sim <- suppressMessages(reLoad(s$simFile, parse = FALSE))[[1L]]
  ## the point of the argument: cheaper load, same laziness
  expect_true(all(rlang::env_binding_are_lazy(sim@.xData, c("alpha", "beta"))))
  expect_identical(sim$alpha, s$objs$alpha)
})

test_that("reLoad warns, not errors, if SpaDES.core predates `reparse`", {
  .skip_if_no_lazy_sidecar()
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)

  ## an older loadSimList: no `reparse` in its formals
  testthat::local_mocked_bindings(
    loadSimList = function(filename, projectPath = getwd(), ...) readRDS(filename),
    .package = "SpaDES.core"
  )

  expect_warning(suppressMessages(reLoad(s$simFile, parse = FALSE)),
                 "reparse")
})

test_that("reGetUntarLoad passes parse through to reLoad", {
  .skip_if_no_gnu_tar()
  .skip_if_no_lazy_sidecar()
  td <- withr::local_tempdir()
  s <- .makeLazySim(td)
  tarDir <- file.path(td, "tars"); dir.create(tarDir)
  tb <- suppressMessages(outTar(s$simFile, runName = "lazyrun",
                                tarDir = tarDir, verbose = FALSE))
  unlink(s$simFile); unlink(s$lazyDir, recursive = TRUE)

  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    preProcess = function(url, ...) list(targetFilePath = tb),
    .package = "reproducible"
  )
  testthat::local_mocked_bindings(
    reLoad = function(simFilenames, ..., parse = TRUE) {
      seen$parse <- parse
      stats::setNames(list(NULL), basename(simFilenames))
    }
  )

  withr::with_dir(tempdir(),
    suppressMessages(reGetUntarLoad("id", destDir = td, parse = FALSE, verbose = FALSE)))
  expect_false(seen$parse)
})
