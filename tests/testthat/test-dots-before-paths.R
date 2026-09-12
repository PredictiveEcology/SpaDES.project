## A `...` argument declared before `paths` must be usable inside `paths`.
##
## setupProject() evaluates dots in two batches: those before the first formal
## argument run before the formals, the rest run after. `defaultDots` counted as
## a formal for that split, so a dot written after it -- the natural place, since
## defaultDots is where its fallback lives -- was not resolved until AFTER `paths`
## had already been evaluated. pathBuild() then received the dot's unevaluated
## expression and deparsed it into a directory name:
##   outputs/.ELFind/370            (for `.foo = .ELFind`)
##   outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/...
## while out$.foo was, by the end, correctly "4.3". The headline contract --
## "any argument written above another is available to it" -- was broken for
## exactly the layout global.R files use.
##
## defaultDots is a fallback table, not a configuration block; it now joins
## params/studyArea/times in `argsCanGoAnywhere`, so it no longer splits the
## dot sequence.

test_that("a dot declared after defaultDots is resolved before paths is evaluated", {
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    .SSP    = .SSP,
    defaultDots = list(.ELFind = "4.3", .SSP = 370),
    .foo = .ELFind,                                   # after defaultDots
    paths = list(packagePath = .libPaths()[1L],
                 outputPath  = SpaDES.project::pathBuild(.foo, .SSP)),
    updateRprofile = FALSE
  ))
  expect_identical(out$.foo, "4.3")
  expect_match(out$paths$outputPath, "outputs/4\\.3/370$")
  expect_false(grepl("\\.ELFind|\\.foo", out$paths$outputPath))
})

test_that("a dot declared before defaultDots still resolves before paths", {
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    .SSP    = .SSP,
    .foo = .ELFind,                                   # before defaultDots
    defaultDots = list(.ELFind = "4.3", .SSP = 370),
    paths = list(packagePath = .libPaths()[1L],
                 outputPath  = SpaDES.project::pathBuild(.foo, .SSP)),
    updateRprofile = FALSE
  ))
  expect_identical(out$.foo, "4.3")
  expect_match(out$paths$outputPath, "outputs/4\\.3/370$")
})

test_that("a dot declared after paths is still evaluated after paths (unchanged)", {
  ## The split still exists; it is now anchored on real formals. This pins that a
  ## dot placed after `paths` keeps the documented late evaluation.
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    defaultDots = list(.ELFind = "4.3"),
    paths = list(packagePath = .libPaths()[1L]),
    .late = .ELFind,                                  # after paths
    updateRprofile = FALSE
  ))
  expect_identical(out$.late, "4.3")
})

## The early/late split must be computed by NAME, not by position.
##
## `origArgOrder` names every argument of the call; `dotsSUB` holds only the
## dots. `firstSet <- 1:(firstNamedArg - 2)` indexed the second with a position
## counted over the first, so whenever an argsCanGoAnywhere formal (defaultDots,
## times, params, studyArea) sat before the first counted formal, the slice ran
## past the true early dots and pulled in dots written AFTER `paths`. Those were
## then evaluated before the arguments above them existed, and an
## `if (exists(".x")) .x else .ELFind` dot came back as its own unevaluated call.

test_that("defaultDots before paths does not pull a later dot into the early batch", {
  ## global.R layout: dots, defaultDots, paths, then a self-defaulting dot.
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    defaultDots = list(.ELFind = "4.3"),
    paths = list(packagePath = .libPaths()[1L]),
    .studyAreaName = if (exists(".studyAreaName")) .studyAreaName else .ELFind,  # after paths
    updateRprofile = FALSE
  ))
  expect_identical(out$.ELFind, "4.3")
  expect_identical(out$.studyAreaName, "4.3")
})

test_that("times before paths does not pull a later dot into the early batch", {
  ## Same defect, pre-existing form: `times` has always been in argsCanGoAnywhere.
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    times = list(start = 1, end = 2),
    defaultDots = list(.ELFind = "4.3"),
    paths = list(packagePath = .libPaths()[1L]),
    .studyAreaName = if (exists(".studyAreaName")) .studyAreaName else .ELFind,  # after paths
    updateRprofile = FALSE
  ))
  expect_identical(out$.studyAreaName, "4.3")
  expect_equal(out$times, list(start = 1, end = 2))
})
