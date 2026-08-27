## factorialDesign(), the design engine behind experiment(), ported from
## SpaDES.experiment. Pure combinatorics -- no spades() call, no network.
## `sim` is only consulted for the `modules` default, so a module-free simList
## built by simInit(times = ...) is enough.

mkBaseSim <- function() {
  withr::local_options(spades.moduleCodeChecks = FALSE, .local_envir = parent.frame())
  suppressMessages(SpaDES.core::simInit(times = list(start = 0, end = 1)))
}

test_that("factorialDesign crosses every parameter alternative", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(
    mkBaseSim(),
    params  = list(modA = list(p1 = c(1, 2), p2 = c("x", "y"))),
    modules = list("modA")
  )

  # 2 x 2 fully crossed, stored as *indices* into the alternatives
  expect_identical(nrow(fd), 4L)
  expect_true(all(c("modA.p1", "modA.p2", "modules", "expLevel") %in% names(fd)))
  expect_identical(sort(unique(fd$modA.p1)), c(1L, 2L))
  expect_identical(sort(unique(fd$modA.p2)), c(1L, 2L))
  expect_identical(fd$expLevel, 1:4)
})

test_that("factorialDesign repeats the design for replicates", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(
    mkBaseSim(),
    params     = list(modA = list(p1 = c(1, 2))),
    modules    = list("modA"),
    replicates = 2
  )

  expect_identical(nrow(fd), 4L)
  expect_identical(fd$replicate, rep(1:2, each = 2))
  # expLevel restarts within each replicate rather than running 1:4
  expect_identical(fd$expLevel, rep(1:2, times = 2))
})

test_that("factorialDesign accepts an explicit vector of replicate ids", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(
    mkBaseSim(),
    params     = list(modA = list(p1 = c(1, 2))),
    modules    = list("modA"),
    replicates = c(5, 9)
  )

  expect_identical(fd$replicate, rep(c(5, 9), each = 2))
})

test_that("factorialDesign gives one row per module set when there are no params", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(mkBaseSim(), params = list(),
                        modules = list("modA", "modB"))

  expect_identical(nrow(fd), 2L)
  expect_identical(fd$modules, 1:2)
  expect_identical(fd$expLevel, 1:2)
})

test_that("factorialDesign indexes objects and drops the modules column", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(
    mkBaseSim(),
    params  = list(),
    modules = list("modA"),
    objects = list(list(o = 1), list(o = 2))
  )

  # when the only factor is `object`, no modules column is added
  expect_true("object" %in% names(fd))
  expect_false("modules" %in% names(fd))
  expect_identical(fd$object, 1:2)
})

test_that("factorialDesign indexes inputs", {
  skip_if_not_installed("SpaDES.core")

  fd <- factorialDesign(
    mkBaseSim(),
    params  = list(),
    modules = list("modA"),
    inputs  = list(data.frame(file = "a"), data.frame(file = "b"))
  )

  expect_true("input" %in% names(fd))
  expect_identical(fd$input, 1:2)
})

test_that("factorialDesign defaults modules to those in the simList", {
  skip_if_not_installed("SpaDES.core")

  sim <- mkBaseSim()
  # module-free simList -> one empty module set -> a single design row
  fd <- factorialDesign(sim, params = list())

  expect_identical(nrow(fd), 1L)
  expect_identical(fd$expLevel, 1L)
})
