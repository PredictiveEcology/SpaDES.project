## Dot resolution: one scope, values only, evaluated once in written order.
## These are the cases isolated on 2026-09-05 (each in a fresh calling env --
## setupProject() used to publish resolved dots into the caller frame's scope,
## which made `exists(".x")` TRUE for the *next* call in the same session and
## hid every one of these).

sanDot <- quote(if (exists(".studyAreaName")) .studyAreaName else .ELFind)

runIn <- function(expr) {
  e <- new.env(parent = globalenv())
  eval(expr, envir = e)
}

test_that("global.R layout: dots, defaultDots, paths, then a self-defaulting dot", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(bquote(setupProject(
    .ELFind = .ELFind,
    defaultDots = list(.ELFind = "4.3"),
    paths = list(packagePath = .libPaths()[1L]),
    .studyAreaName = .(sanDot),
    updateRprofile = FALSE)))
  expect_identical(out$.ELFind, "4.3")
  expect_identical(out$.studyAreaName, "4.3")
})

test_that("no defaultDots at all: a self-defaulting dot still resolves", {
  ## Previously, without defaultDots the scoped evaluation was skipped entirely
  ## and the dot's own unevaluated expression was published under its name, so
  ## exists(".studyAreaName") became TRUE and the expression came back.
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(bquote(setupProject(
    .ELFind = "4.3",
    paths = list(packagePath = .libPaths()[1L]),
    .studyAreaName = .(sanDot),
    updateRprofile = FALSE)))
  expect_identical(out$.studyAreaName, "4.3")
})

test_that("a dot's own name is not visible to its own expression", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    .x = exists(".x"),
    updateRprofile = FALSE)))
  expect_false(out$.x)
})

test_that("caller-supplied value beats defaultDots; defaultDots beats nothing", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv())
  e$.ELFind <- "9.9"                                  # the batch-worker case
  out <- eval(bquote(setupProject(
    .ELFind = .ELFind,
    .studyAreaName = .(sanDot),
    defaultDots = list(.ELFind = "4.3"),
    paths = list(packagePath = .libPaths()[1L]),
    updateRprofile = FALSE)), envir = e)
  expect_identical(out$.ELFind, "9.9")
  expect_identical(out$.studyAreaName, "9.9")
})

test_that("a name that only resolves to a package function is not a caller value", {
  ## `.mode` is not a function, but `body` and `q` are: with a default present,
  ## the default must win over the namespace binding.
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    body = body, q = q,
    defaultDots = list(body = "torso", q = 7),
    paths = list(packagePath = .libPaths()[1L]),
    updateRprofile = FALSE)))
  expect_identical(out$body, "torso")
  expect_identical(out$q, 7)
})

test_that("defaultDots supplied as a variable works the same as a literal list()", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv())
  e$myDefaults <- list(.ELFind = "4.3", .SSP = 370)
  out <- eval(bquote(setupProject(
    .ELFind = .ELFind,
    .SSP = .SSP,
    defaultDots = myDefaults,
    paths = list(packagePath = .libPaths()[1L],
                 outputPath  = SpaDES.project::pathBuild(.ELFind, .SSP)),
    .studyAreaName = .(sanDot),
    updateRprofile = FALSE)), envir = e)
  expect_identical(out$.ELFind, "4.3")
  expect_identical(out$.SSP, 370)
  expect_identical(out$.studyAreaName, "4.3")
  expect_match(out$paths$outputPath, "outputs/4\\.3/370$")
})

test_that("a defaultDots entry may reference an earlier one", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    .ELFind = .ELFind, .label = .label,
    defaultDots = list(.ELFind = "4.3", .label = paste0("ELF", .ELFind)),
    paths = list(packagePath = .libPaths()[1L]),
    updateRprofile = FALSE)))
  expect_identical(out$.label, "ELF4.3")
})

test_that("a default is available to an argument whose name differs (cores = .cores)", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    cores = .cores,
    defaultDots = list(.cores = c("birds", "coco")),
    paths = list(packagePath = .libPaths()[1L]),
    updateRprofile = FALSE)))
  expect_identical(out$cores, c("birds", "coco"))
  expect_false(".cores" %in% names(out))
})

test_that("a dot that evaluates to NULL is kept, as NULL, under its name", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- runIn(quote(setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    .nothing = NULL, .alsoNothing = if (FALSE) 1,
    updateRprofile = FALSE)))
  expect_true(all(c(".nothing", ".alsoNothing") %in% names(out)))
  expect_null(out$.nothing)
  expect_null(out$.alsoNothing)
})

test_that("an unresolvable dot is returned unevaluated and reported, not an error", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  msgs <- character()
  out <- withCallingHandlers(
    runIn(quote(setupProject(
      paths = list(packagePath = .libPaths()[1L]),
      .mode = .mode,
      updateRprofile = FALSE))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_identical(out$.mode, quote(.mode))
  expect_true(any(grepl("tolerated error", msgs)))
})

test_that("later dots see earlier dots, formals, and `functions`; each dot runs once", {
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  e <- new.env(parent = globalenv()); e$count <- 0L
  out <- eval(quote(setupProject(
    .a = { count <<- count + 1L; 2 },
    .b = .a * 10,
    paths = list(packagePath = .libPaths()[1L]),
    functions = list(twice = function(x) 2 * x),
    .c = twice(.b),
    .d = basename(paths$projectPath),
    updateRprofile = FALSE)), envir = e)
  expect_identical(out$.b, 20)
  expect_identical(out$.c, 40)
  expect_identical(out$.d, basename(attr(out$paths, "extraPaths")$projectPath))
  expect_identical(e$count, 1L)
})

test_that("a default is visible to formals evaluated by the setup* helpers (modules, options)", {
  ## `modules = unlist(.modules)` reaches setupModules() with setupProject()'s
  ## own frame as the outermost fallback, not the resolution scope. Caught by
  ## FireSenseTesting's global.R, not by the suite -- hence this test.
  skip_on_cran()
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  msgs <- character()
  out <- withCallingHandlers(suppressWarnings(
    runIn(quote(setupProject(
      defaultDots = list(.mods = "PredictiveEcology/Biomass_speciesData@master", .optVal = 3L),
      paths = list(packagePath = .libPaths()[1L]),
      modules = unlist(.mods),
      packages = NULL,
      options = list(SpaDES.project.testOptFromDefault = .optVal),
      updateRprofile = FALSE)))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("tolerated error", msgs)))
  expect_identical(unname(out$modules), "Biomass_speciesData")
  expect_identical(getOption("SpaDES.project.testOptFromDefault"), 3L)
  options(SpaDES.project.testOptFromDefault = NULL)
})
