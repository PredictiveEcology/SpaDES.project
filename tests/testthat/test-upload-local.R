## outSave() / outTar() -- the filesystem half of R/upload.R. outUpload() and
## outSaveTarUpload() need Google Drive and are not covered here.

test_that("outTar bundles the sim file and named outputs", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "run1.rds")
  saveRDS(1, simFile)
  out1 <- file.path(td, "out1.tif"); writeLines("a", out1)
  out2 <- file.path(td, "out2.tif"); writeLines("b", out2)

  tarball <- suppressMessages(
    outTar(simFile, outputFiles = c(out1, out2), runName = "run1", verbose = FALSE)
  )

  expect_true(file.exists(tarball))
  expect_identical(basename(tarball), "run1.tar.gz")

  contents <- basename(untar(tarball, list = TRUE))
  expect_true(all(c("run1.rds", "out1.tif", "out2.tif") %in% contents))
})

test_that("outTar silently drops missing and empty output paths", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "run2.rds")
  saveRDS(1, simFile)
  present <- file.path(td, "here.tif"); writeLines("x", present)

  tarball <- suppressMessages(
    outTar(simFile,
           outputFiles = c(present, file.path(td, "gone.tif"), ""),
           runName = "run2", verbose = FALSE)
  )

  contents <- basename(untar(tarball, list = TRUE))
  expect_true("here.tif" %in% contents)
  expect_false("gone.tif" %in% contents)
})

test_that("outTar picks up the lazy-save sidecar directory", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "run3.rds")
  saveRDS(1, simFile)
  # saveSimList(lazy = TRUE) writes one file per object into <base>_lazy/,
  # plus a _manifest.rds, next to the sim
  lazyDir <- file.path(td, "run3_lazy")
  dir.create(lazyDir)
  saveRDS(list(), file.path(lazyDir, "_manifest.rds"))
  saveRDS(1, file.path(lazyDir, "someObj.rds"))

  tarball <- suppressMessages(
    outTar(simFile, runName = "run3", verbose = FALSE)
  )

  entries <- untar(tarball, list = TRUE)
  expect_true(all(c("_manifest.rds", "someObj.rds") %in% basename(entries)))
  # the sidecar files must keep their run3_lazy/ path component: that is how
  # loadSimList() tells them apart from ordinary output files
  expect_true(all(grepl("(^|/)run3_lazy/", grep("_lazy/", entries, value = TRUE))))
  expect_length(grep("(^|/)run3_lazy/", entries), 2L)
  # simFilename stays first: reUntar() takes the archive's first entry as the simList
  expect_identical(basename(entries[[1L]]), "run3.rds")
})

test_that("outTar honours tarDir", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "run4.rds")
  saveRDS(1, simFile)
  elsewhere <- file.path(td, "tarballs"); dir.create(elsewhere)

  tarball <- suppressMessages(
    outTar(simFile, runName = "run4", tarDir = elsewhere, verbose = FALSE)
  )

  expect_identical(normalizePath(dirname(tarball)), normalizePath(elsewhere))
})

test_that("outSave writes an rds next to the simList outputPath", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.moduleCodeChecks = FALSE)
  localSpadesOptions()

  td <- withr::local_tempdir()
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 0, end = 1),
    paths = list(outputPath = td)
  ))
  sim$val <- 42

  f <- suppressMessages(outSave(sim, runName = "myrun", lazy = FALSE))

  expect_true(file.exists(f))
  expect_match(basename(f), "^myrun.*\\.rds$")
})

test_that("outSave honours an explicit simFilename and creates its directory", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.moduleCodeChecks = FALSE)
  localSpadesOptions()

  td <- withr::local_tempdir()
  sim <- suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1)))
  target <- file.path(td, "nested", "deeper", "explicit.rds")

  f <- suppressMessages(outSave(sim, runName = "ignored", simFilename = target,
                                lazy = FALSE))

  expect_identical(f, target)
  expect_true(file.exists(target))
})

test_that("outSave survives an unset reproducible.verbose", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.moduleCodeChecks = FALSE)
  localSpadesOptions()

  # SpaDES.core::saveSimList() resolves its own default as a bare
  # getOption("reproducible.verbose") and hands the result to
  # Require::messageVerbose(), whose `if (verbose >= verboseLevel)` fails with
  # "argument is of length zero" when the option is absent -- which it is, at
  # this point in a full-suite run. outSave()'s default supplies the fallback.
  withr::local_options(reproducible.verbose = NULL)
  expect_length(getOption("reproducible.verbose"), 0L)

  td <- withr::local_tempdir()
  sim <- suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1),
                                               paths = list(outputPath = td)))

  f <- suppressMessages(outSave(sim, runName = "quiet", lazy = FALSE))
  expect_true(file.exists(f))
})

test_that("outSave forwards verbose to saveSimList", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.moduleCodeChecks = FALSE)
  localSpadesOptions()

  # saveSimList filters its own messages, so the argument cannot be observed
  # from the output; capture it at the call boundary instead.
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    saveSimList = function(...) {
      captured$verbose <- list(...)$verbose
      invisible(NULL)
    },
    .package = "SpaDES.core"
  )

  td <- withr::local_tempdir()
  sim <- suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1),
                                               paths = list(outputPath = td)))
  target <- file.path(td, "x.rds")

  suppressMessages(outSave(sim, runName = "x", simFilename = target,
                           lazy = FALSE, verbose = 3))
  expect_identical(captured$verbose, 3)

  # and the default fills in when the option is absent
  withr::local_options(reproducible.verbose = NULL)
  suppressMessages(outSave(sim, runName = "x", simFilename = target, lazy = FALSE))
  expect_identical(captured$verbose, 1)
})

## tarIndex(): offsets of every member of an uncompressed tar, found by seeking.

.tarBytes <- function(tarball, offset, size) {
  con <- file(tarball, "rb"); on.exit(close(con))
  seek(con, offset); readBin(con, "raw", size)
}

test_that("tarIndex offsets and sizes match the members, long paths included", {
  td <- withr::local_tempdir()
  deep <- file.path(td, paste(rep("a-long-directory-name", 6), collapse = "/"))  # > 100 chars
  dir.create(deep, recursive = TRUE)
  expect_gt(nchar(deep), 100)
  files <- c(file.path(td, "first.rds"), file.path(deep, "deep.rds"),
             file.path(td, "empty.txt"), file.path(td, "big.bin"))
  saveRDS(1:10, files[[1]]); saveRDS(letters, files[[2]])
  file.create(files[[3]]); writeBin(as.raw(rep(1:255, 40)), files[[4]])
  tarball <- file.path(td, "myrun.tar.gz")             # uncompressed, as outTar() makes it
  suppressWarnings(suppressMessages(utils::tar(tarball, files = files, extra_flags = "")))

  idxFile <- tarIndex(tarball)
  expect_identical(idxFile, file.path(td, "myrun_index.rds"))
  idx <- readRDS(idxFile)
  expect_named(idx, c("name", "offset", "size"))
  ## names as stored: GNU tar drops the leading "/", R's internal tar keeps it
  idx$name <- sub("^/", "", idx$name)
  expect_identical(idx$name[[1]], sub("^/", "", files[[1]]))
  expect_true(sub("^/", "", files[[2]]) %in% idx$name)         # the >100-char path, whole
  expect_equal(nrow(idx), length(files))                       # members only: no directories
  for (i in seq_len(nrow(idx))) {
    f <- files[match(idx$name[[i]], sub("^/", "", files))]
    expect_identical(.tarBytes(tarball, idx$offset[[i]], idx$size[[i]]),
                     readBin(f, "raw", file.size(f)))
  }
})

test_that("tarIndex reads pax headers", {
  skip_if(Sys.which("tar") == "")
  td <- withr::local_tempdir()
  deep <- file.path(td, paste(rep("pax-directory-name", 8), collapse = "/"))
  dir.create(deep, recursive = TRUE)
  f <- file.path(deep, "p.rds"); saveRDS(runif(50), f)
  tarball <- file.path(td, "pax.tar.gz")
  withr::with_dir(td, system2("tar", c("--format=pax", "-cf", shQuote(tarball), "-C", shQuote(td),
                                       shQuote(sub(paste0(td, "/"), "", f, fixed = TRUE)))))
  idx <- readRDS(tarIndex(tarball))
  expect_equal(nrow(idx), 1L)
  expect_true(grepl("p.rds$", idx$name) && nchar(idx$name) > 100)
  expect_identical(.tarBytes(tarball, idx$offset, idx$size), readBin(f, "raw", file.size(f)))
})

test_that("outTar writes the index beside the tarball", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "runI.rds"); saveRDS(1, simFile)
  tarball <- suppressMessages(outTar(simFile, runName = "runI", verbose = FALSE))
  idx <- readRDS(file.path(td, "runI_index.rds"))
  expect_identical(sub("^/", "", idx$name[[1]]), sub("^/", "", simFile))
  expect_identical(.tarBytes(tarball, idx$offset[[1]], idx$size[[1]]),
                   readBin(simFile, "raw", file.size(simFile)))
})

test_that("outUpload uploads the index beside the tarball and returns the tarball's dribble", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "runU.rds"); saveRDS(1, simFile)
  tarball <- suppressMessages(outTar(simFile, runName = "runU", verbose = FALSE))
  sent <- character()
  testthat::local_mocked_bindings(
    drive_upload = function(media, ...) {
      sent <<- c(sent, basename(media)); data.frame(id = paste0("id-", basename(media)))
    }, .package = "googledrive")
  res <- suppressMessages(outUpload(tarball, "folder"))
  expect_identical(sent, c("runU.tar.gz", "runU_index.rds"))
  expect_identical(res$id, "id-runU.tar.gz")

  ## no index beside the tarball: as before, one upload
  unlink(file.path(td, "runU_index.rds")); sent <- character()
  suppressMessages(outUpload(tarball, "folder"))
  expect_identical(sent, "runU.tar.gz")
})

## outSaveTarUpload(ledger =): a row per upload, written only after the upload.

.ledgerFixture <- function(env = parent.frame()) {
  skip_if_not_installed("sf")
  skip_if(!exists("CacheGeoWrite", asNamespace("reproducible")), "reproducible lacks CacheGeoWrite")
  td <- withr::local_tempdir(.local_envir = env)
  simFile <- file.path(td, "runL.rds"); saveRDS(1, simFile)
  area <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
                     crs = 4326)
  list(td = td, simFile = simFile, area = area,
       ledger = reproducible::CacheGeoLedger("ledger.rds", destinationPath = file.path(td, "led"),
                                             key = "runName"))
}
.mockSaveTar <- function(fx, env = parent.frame()) {
  testthat::local_mocked_bindings(
    outSave = function(...) fx$simFile,
    outTar = function(...) file.path(fx$td, "runL.tar.gz"),
    .env = env)
  testthat::local_mocked_bindings(outputs = function(...) data.frame(saveTime = c(3020, 2020, 3020)),
                                  outputPath = function(...) "/some/outputs",
                                  .package = "SpaDES.core", .env = env)
}

test_that("outSaveTarUpload writes one ledger row after a successful upload", {
  fx <- .ledgerFixture(); .mockSaveTar(fx)
  testthat::local_mocked_bindings(outUpload = function(...) data.frame(id = "DRIVEID"))
  suppressMessages(outSaveTarUpload("runL", sim = NULL, gFolder = "f", ledger = fx$ledger,
                                    area = fx$area, scenario = list(SSP = "ssp245", rep = 3L)))
  rows <- reproducible::CacheGeoRead(fx$ledger, area = NULL)
  expect_equal(nrow(rows), 1L)
  expect_identical(rows$runName, "runL")
  expect_identical(rows$driveId, "DRIVEID")
  expect_identical(rows$outputPath, "/some/outputs")
  expect_identical(rows$saveTimes[[1]], c(2020, 3020))
  expect_identical(rows$SSP, "ssp245")
  expect_identical(rows$rep, 3L)
  expect_s3_class(rows$uploaded, "POSIXct")
})

test_that("outSaveTarUpload writes no ledger row when the upload fails", {
  fx <- .ledgerFixture(); .mockSaveTar(fx)
  testthat::local_mocked_bindings(outUpload = function(...) stop("drive down"))
  expect_error(suppressMessages(outSaveTarUpload("runL", sim = NULL, gFolder = "f",
                                                 ledger = fx$ledger, area = fx$area)),
               "drive down")
  expect_false(file.exists(fx$ledger$path))
})

test_that("a ledger needs an area, and its key must be runName", {
  fx <- .ledgerFixture()
  expect_error(outSaveTarUpload("runL", sim = NULL, ledger = fx$ledger), "area")
  bad <- reproducible::CacheGeoLedger("b.rds", destinationPath = fx$td, key = "other")
  expect_error(outSaveTarUpload("runL", sim = NULL, ledger = bad, area = fx$area), "runName")
})

test_that("outLedgerRow builds the row a backfill writes, without a sim", {
  skip_if_not_installed("sf")
  area <- sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326)
  r <- outLedgerRow("old", "ID1", saveTimes = c(30, 10, 10), area = area,
                    outputPath = "/p/out", scenario = list(GCM = "x"), uploaded = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_s3_class(r, "sf")
  expect_identical(r$runName, "old"); expect_identical(r$driveId, "ID1")
  expect_identical(r$saveTimes[[1]], c(10, 30)); expect_identical(r$GCM, "x")
  expect_identical(r$outputPath, "/p/out")
})
