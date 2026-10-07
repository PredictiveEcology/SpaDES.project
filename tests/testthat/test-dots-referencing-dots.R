## A `...` argument that references another `...` argument, run the way an
## experimentTmux worker runs global.R: the queue row's values are assigned into
## a fresh environment (parent globalenv()) and global.R is source()d into it.
##
## The caller-supplied form of `.sa = if (exists(".sa")) .sa else .ELFind`
## worked until b365a4d (2026-06-03), which stopped capture_dots() from forcing
## call-shaped dots in the caller's frame. From then until #157 (2026-09-06) the
## dot reached `paths` as its expression, and pathBuild() made it a directory:
##   outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/...
## These tests pin each shape through setupProject(), with the value given by the
## worker and with only the defaultDots fallback.

## source() `lines` the way tmuxRunNextWorker() does, with `vals` as the queue row.
runAsWorker <- function(lines, vals = list()) {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeLines(lines, f)
  scn_env <- new.env(parent = globalenv())
  for (nm in names(vals)) assign(nm, vals[[nm]], envir = scn_env)
  source(f, local = scn_env)
  get("out", envir = scn_env)
}

globalLines <- function(dotLine, pathArg) c(
  "out <- SpaDES.project::setupProject(",
  "  .ELFind = .ELFind,",
  "  .x = .x,",
  "  defaultDots = list(.ELFind = '4.3', .x = 1985L),",
  paste0("  ", dotLine, ","),                           # after defaultDots, as in global.R
  "  overwrite = FALSE,",
  "  paths = list(packagePath = .libPaths()[1L],",
  paste0("               outputPath = SpaDES.project::pathBuild(", pathArg, ")),"),
  "  updateRprofile = FALSE)")

test_that("`.studyAreaName = .ELFind` reaches paths as a value (worker and default)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  lines <- globalLines(".studyAreaName = .ELFind", ".studyAreaName")

  out <- runAsWorker(lines, list(.ELFind = "6.3.1"))
  expect_identical(out$.studyAreaName, "6.3.1")
  expect_match(out$paths$outputPath, "outputs/6\\.3\\.1$")

  out <- runAsWorker(lines)
  expect_identical(out$.studyAreaName, "4.3")
  expect_match(out$paths$outputPath, "outputs/4\\.3$")
})

test_that("`if (exists(.sa)) .sa else .ELFind` reaches paths as a value (worker and default)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  lines <- globalLines(
    ".studyAreaName = if (exists('.studyAreaName')) .studyAreaName else .ELFind",
    ".studyAreaName")

  out <- runAsWorker(lines, list(.ELFind = "6.3.1"))
  expect_identical(out$.studyAreaName, "6.3.1")
  expect_match(out$paths$outputPath, "outputs/6\\.3\\.1$")

  out <- runAsWorker(lines)
  expect_identical(out$.studyAreaName, "4.3")
  expect_match(out$paths$outputPath, "outputs/4\\.3$")

  ## the worker supplies .studyAreaName itself: its value wins
  out <- runAsWorker(lines, list(.ELFind = "6.3.1", .studyAreaName = "mySA"))
  expect_identical(out$.studyAreaName, "mySA")
  expect_match(out$paths$outputPath, "outputs/mySA$")
})

test_that("a self-reference `.x = .x` takes the worker's value, else the default", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  lines <- globalLines(".yrs = .x:1990L", ".x")

  out <- runAsWorker(lines, list(.x = 1988L))
  expect_identical(out$.x, 1988L)
  expect_identical(out$.yrs, 1988:1990)
  expect_match(out$paths$outputPath, "outputs/1988$")

  out <- runAsWorker(lines)
  expect_identical(out$.x, 1985L)
  expect_identical(out$.yrs, 1985:1990)
  expect_match(out$paths$outputPath, "outputs/1985$")
})

test_that("a self-reference `.x = .x` with the value only from the worker (no default)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  lines <- c(
    "out <- SpaDES.project::setupProject(",
    "  .x = .x,",
    "  .y = .x + 1L,",
    "  paths = list(packagePath = .libPaths()[1L],",
    "               outputPath = SpaDES.project::pathBuild(.y)),",
    "  updateRprofile = FALSE)")
  out <- runAsWorker(lines, list(.x = 1988L))
  expect_identical(out$.x, 1988L)
  expect_identical(out$.y, 1989L)
  expect_match(out$paths$outputPath, "outputs/1989$")
})
