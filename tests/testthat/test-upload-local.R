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

test_that("outTar picks up the lazy-save sibling database", {
  td <- withr::local_tempdir()
  simFile <- file.path(td, "run3.rds")
  saveRDS(1, simFile)
  # tools::makeLazyLoadDB writes <base>_xData.rdx / .rdb next to the sim
  writeLines("x", file.path(td, "run3_xData.rdx"))
  writeLines("y", file.path(td, "run3_xData.rdb"))

  tarball <- suppressMessages(
    outTar(simFile, runName = "run3", verbose = FALSE)
  )

  contents <- basename(untar(tarball, list = TRUE))
  expect_true(all(c("run3_xData.rdx", "run3_xData.rdb") %in% contents))
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
