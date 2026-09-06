## Formals must be resolved from the call site outward (#158).
##
## The formals' evaluator, evalSUB(), used to look a bare symbol up with
## inherits = FALSE in its first environment, then evaluate in setupProject()'s
## own frame -- whose enclosure chain reaches the package namespaces before the
## caller -- so `params = pf` resolved to stats::pf, and an `options` list that
## needed both a caller-local variable and a helper local (`paths`) had no single
## environment that could see both and came back unevaluated.

runIn <- function(expr, env = new.env(parent = globalenv())) eval(expr, envir = env)

test_that("a formal given as a symbol resolves to the caller's variable, not a package object of the same name", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv())
  e$pf <- tempfile(fileext = ".R")                                  # stats::pf exists
  writeLines('list(.globals = list(PSPdataTypes = "dummy"))', e$pf)
  e$df <- list(start = 2001, end = 2003)                            # stats::df exists
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    params = pf,
    times = df,
    updateRprofile = FALSE)), e)
  expect_identical(out$params$.globals$PSPdataTypes, "dummy")
  expect_equal(out$times, list(start = 2001, end = 2003))
})

test_that("an `options` list may combine a caller-local variable with `paths` (LandRDemo_coreVeg)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  reposOrig <- getOption("repos"); destOrig <- getOption("reproducible.destinationPath")
  on.exit(options(repos = reposOrig, reproducible.destinationPath = destOrig), add = TRUE)
  e <- new.env(parent = globalenv())                                 # NOT the global env: a sourced script or a batch worker
  e$repos <- c("https://predictiveecology.r-universe.dev", CRAN = "https://cloud.r-project.org")
  msgs <- character()
  out <- withCallingHandlers(
    runIn(quote(setupProject(
      paths = list(packagePath = .libPaths()[1L]),
      options = list(repos = repos, reproducible.destinationPath = paths$inputPath),
      updateRprofile = FALSE)), e),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_identical(getOption("repos"), e$repos)
  expect_identical(getOption("reproducible.destinationPath"), out$paths$inputPath)
  expect_false(any(grepl("tolerated error", msgs)))
})

test_that("`modules` given as a symbol that shadows a base function resolves to the caller's value", {
  skip_on_cran()
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv())
  e$t <- "PredictiveEcology/Biomass_speciesData@master"               # base::t exists
  out <- suppressWarnings(runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    modules = t, packages = NULL,
    updateRprofile = FALSE)), e))
  expect_identical(unname(out$modules), "Biomass_speciesData")
})
