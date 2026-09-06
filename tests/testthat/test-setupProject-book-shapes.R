## Argument shapes taken from the PredictiveEcology training book
## (predictiveecology.org/training/book, 23 setupProject() calls across 18
## chapters, inventoried 2026-09-06). The suite had none of these before:
## chained `{ }` block dots, namespaced calls referencing earlier dots,
## `$`-indexed caller objects, dots referencing formals (`times`, `paths`),
## `params` from a file, `packages`/`require` as vectors. Each test is the
## offline analogue of the book's shape.

runIn <- function(expr, env = new.env(parent = globalenv())) eval(expr, envir = env)

test_that("book: chained { } dots, namespaced calls, $-indexed caller objects, studyArea = { }", {
  skip_on_cran(); skip_if_not_installed("terra")
  setupTest(pkgs = "terra"); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv())
  e$simOut <- list(sppEquiv = data.frame(Boreal = c("Pice_Gla", "Pinu_Ban"), stringsAsFactors = FALSE),
                   sppEquivCol = "Boreal")
  out <- runIn(quote(setupProject(
    sppEquivCol = simOut$sppEquivCol,                               # `$` on a caller object (LandRDemo)
    paths = list(packagePath = .libPaths()[1L]),
    studyArea = {                                                   # formal given as a block (all demos)
      centre <- terra::vect(cbind(-104.757, 55.68663), crs = "epsg:4326")
      terra::buffer(centre, 1000)
    },
    studyAreaLarge = terra::buffer(studyArea, width = 3000),        # namespaced call on the formal above (ForestsAndFire)
    rasterToMatchLarge = {                                          # block on the dot above
      r <- terra::rast(studyAreaLarge, res = 1000); r[] <- 1L; r
    },
    rasterToMatch = terra::crop(rasterToMatchLarge, studyArea),    # two earlier dots at once
    sppEquiv = simOut$sppEquiv[simOut$sppEquiv[[sppEquivCol]] == "Pinu_Ban", , drop = FALSE],  # `[` + earlier dot
    updateRprofile = FALSE)), e)
  expect_identical(out$sppEquivCol, "Boreal")
  expect_s4_class(out$studyArea, "SpatVector")
  expect_s4_class(out$studyAreaLarge, "SpatVector")
  expect_s4_class(out$rasterToMatchLarge, "SpatRaster")
  expect_s4_class(out$rasterToMatch, "SpatRaster")
  expect_identical(out$sppEquiv$Boreal, "Pinu_Ban")
  expect_false(any(vapply(out, is.language, logical(1))))
})

test_that("book: a dot may reference `times` written above it (outputs = ... times$end)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    times = list(start = 2001, end = 2003),
    outputs = data.frame(objectName = "cohortData", saveTime = seq(times$start, times$end)),   # spadesCBMDemo, LandRDemo
    updateRprofile = FALSE)))
  expect_equal(out$outputs$saveTime, 2001:2003)
})

test_that("a dot referencing `times` written BELOW it is not resolved (sequential contract)", {
  ## The contract is "any argument written above another is available to it".
  ## The pre-rewrite code forced the `times` promise through a proxy and made
  ## this work by accident; it is now a tolerated error, reported in the
  ## end-of-call diagnostics, and the dot comes back as its expression.
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  msgs <- character()
  out <- withCallingHandlers(
    runIn(quote(setupProject(
      paths = list(packagePath = .libPaths()[1L]),
      outputs = data.frame(saveTime = seq(times$start, times$end)),
      times = list(start = 2001, end = 2003),
      updateRprofile = FALSE))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(is.language(out$outputs))
  expect_true(any(grepl("tolerated error \\[outputs\\]", msgs)))
  expect_equal(out$times, list(start = 2001, end = 2003))
})

test_that("book: `options` may reference `paths` (reproducible.destinationPath = paths$inputPath)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  optOrig <- getOption("reproducible.destinationPath"); on.exit(options(reproducible.destinationPath = optOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    options = list(reproducible.destinationPath = paths$inputPath),   # LandRDemo
    updateRprofile = FALSE)))
  expect_identical(getOption("reproducible.destinationPath"), out$paths$inputPath)
})

test_that("book: `params` from a file that references a dot declared above", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  paramsFile <- tempfile(fileext = ".R")
  writeLines('list(.globals = list(sppEquivCol = sppEquivCol, PSPdataTypes = "dummy"))', paramsFile)
  e <- new.env(parent = globalenv()); e$paramsFile <- paramsFile
  out <- runIn(quote(setupProject(
    sppEquivCol = "Boreal",
    paths = list(packagePath = .libPaths()[1L]),
    params = paramsFile,                                                    # castorExample uses a GitHub file path
    updateRprofile = FALSE)), e)
  expect_identical(out$params$.globals$sppEquivCol, "Boreal")
  expect_identical(out$params$.globals$PSPdataTypes, "dummy")
})

test_that("book: `packages = c(...)` and `require = c(...)` as vectors", {
  skip_on_cran(); skip_if_not_installed("terra"); skip_if_not_installed("withr")
  ## No setupTest(pkgs = "terra") here: that attaches terra via withr::local_package(),
  ## and the point of this test is that `require =` does the attaching.
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  attachedBefore <- "package:terra" %in% search()
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    packages = c("terra", "withr"),                                  # 16 of the book's 23 calls
    require = c("terra"),
    .afterRequire = exists("vect"),                                  # late dot: attached packages are visible
    updateRprofile = FALSE)))
  if (!attachedBefore)
    on.exit(if ("package:terra" %in% search()) detach("package:terra", character.only = TRUE), add = TRUE)
  expect_true("package:terra" %in% search())
  expect_true(out$.afterRequire)
})
