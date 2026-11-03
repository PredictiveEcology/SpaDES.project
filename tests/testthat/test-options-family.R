## optionsFast(), optionsBatch(), optionsCache(): the options families, and their use in setupProject(options = )

test_that("optionsFast() is exported, and fastOptions() is the same list", {
  expect_true("optionsFast" %in% getNamespaceExports("SpaDES.project"))
  expect_identical(SpaDES.project:::fastOptions(), optionsFast())
  expect_true(optionsFast()$reproducible.cacheSaveFormat == "qs2")
})

test_that("optionsBatch(): nothing to change when a person is debugging, the unattended settings otherwise", {
  expect_identical(optionsBatch(batch = FALSE), list())
  expect_identical(
    optionsBatch(batch = TRUE),
    list(reproducible.showSimilar = FALSE, reproducible.useMemoise = FALSE, reproducible.objSize = FALSE,
         spades.recoveryMode = 0, spades.allowInitDuringSimInit = TRUE))
})

test_that("optionsBatch() default: batch unless interactive and not in tmux (tmux panes report interactive() == TRUE)", {
  default <- function(interactive, tmux) {
    withr::local_envvar(TMUX = tmux)
    eval(formals(optionsBatch)$batch, list(interactive = function() interactive))
  }
  expect_false(default(interactive = TRUE, tmux = NA))             # a person at the console
  expect_false(default(interactive = TRUE, tmux = ""))
  expect_true(default(interactive = TRUE, tmux = "/tmp/tmux-1000/default,1234,0"))  # a tmux pane
  expect_true(default(interactive = FALSE, tmux = NA))             # Rscript, Slurm
  expect_true(default(interactive = FALSE, tmux = "/tmp/tmux-1000/default,1234,0"))
  ## under testthat the session is not interactive, so the real default is batch
  expect_identical(optionsBatch(), optionsBatch(batch = TRUE))
})

test_that("optionsCache() is the shared cache settings", {
  expect_identical(
    optionsCache(),
    list(reproducible.cacheSaveFormat = "qs2", reproducible.shapefileRead = "terra::vect",
         reproducible.overwrite = TRUE, reproducible.useCOG = FALSE, reproducible.memoisePersist = TRUE,
         reproducible.showSimilarDepth = 8))
})

## restore every option a test may set, whatever setupProject() did
localRestoreOptions <- function(nms, env = parent.frame()) {
  old <- stats::setNames(lapply(nms, getOption), nms)
  withr::defer(options(old), envir = env)
}

test_that("setupProject(options = c(optionsCache(), optionsBatch(), list(...))) sets every option", {
  skip_on_cran()
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  localRestoreOptions(c(names(optionsCache()), names(optionsBatch(TRUE)), "myOwn"))
  setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    options = c(SpaDES.project::optionsCache(), SpaDES.project::optionsBatch(batch = TRUE),
                list(spades.recoveryMode = 1, myOwn = "x")),
    updateRprofile = FALSE)
  expect_identical(getOption("reproducible.cacheSaveFormat"), "qs2")
  expect_identical(getOption("reproducible.shapefileRead"), "terra::vect")
  expect_identical(getOption("reproducible.showSimilarDepth"), 8)
  expect_false(getOption("reproducible.showSimilar"))
  expect_false(getOption("reproducible.useMemoise"))
  expect_true(getOption("spades.allowInitDuringSimInit"))
  expect_identical(getOption("spades.recoveryMode"), 1)   # the later `list()` wins
  expect_identical(getOption("myOwn"), "x")
})

test_that("setupProject(options = c(optionsCache(), optionsBatch(batch = FALSE))) leaves the batch options alone", {
  skip_on_cran()
  setupTest(); libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  localRestoreOptions(c(names(optionsCache()), names(optionsBatch(TRUE))))
  options(reproducible.showSimilar = NULL)
  setupProject(
    paths = list(packagePath = .libPaths()[1L]),
    options = c(SpaDES.project::optionsCache(), SpaDES.project::optionsBatch(batch = FALSE)),
    updateRprofile = FALSE)
  expect_identical(getOption("reproducible.cacheSaveFormat"), "qs2")
  expect_null(getOption("reproducible.showSimilar"))
})
