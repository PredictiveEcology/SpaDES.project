## simLists class + as.data.table.simLists, ported from SpaDES.experiment.
## These are pure-R and offline; the only external requirement is SpaDES.core
## (Suggests), needed to build the simList objects a simLists holds.

test_that("updateNames fills only the empty names", {
  un <- SpaDES.project:::updateNames

  # no names at all -> take all of newNames
  expect_identical(names(un(list(1, 2), c("a", "b"))), c("a", "b"))

  # some names present -> only the blanks are filled
  expect_identical(names(un(list(a = 1, 2), c("x", "y"))), c("a", "y"))

  # fully named -> left alone, newNames ignored
  expect_identical(names(un(list(a = 1, b = 2), c("x", "y"))), c("a", "b"))
})

test_that("updateNames derives names from the values when newNames is missing", {
  un <- SpaDES.project:::updateNames

  # quoted expressions get deparsed
  expect_identical(names(un(list(quote(mean(x)), quote(sd(y))))),
                   c("mean(x)", "sd(y)"))

  # plain values get formatted
  expect_identical(names(un(list(1, 2))), c("1", "2"))
})

test_that(".objNamesBySimList groups and sorts object names by simList", {
  e <- new.env(parent = emptyenv())
  # deliberately out of order, to exercise the sort
  for (nm in c("sim1_2", "sim2_1", "sim1_1")) assign(nm, nm, envir = e)

  res <- SpaDES.project:::.objNamesBySimList(e)

  expect_named(res, c("sim1", "sim2"))
  expect_identical(res$sim1, c("sim1_1", "sim1_2"))
  expect_identical(res$sim2, "sim2_1")
})

test_that("new('simLists') builds an empty environment with paths", {
  skip_if_not_installed("SpaDES.core")

  sl <- new("simLists")

  expect_s4_class(sl, "simLists")
  expect_true(is.environment(sl@.xData))
  expect_identical(attr(sl@.xData, "name"), "simLists")
  expect_length(ls(sl), 0L)

  # initialize() pulls SpaDES.core::.paths() when available
  expect_true(is.list(sl@paths))
  expect_true(all(c("inputPath", "outputPath", "modulePath") %in% names(sl@paths)))
})

# Build a simLists holding trivial (module-free) simLists. simInit() with only
# `times` is ~0.1s, which keeps this offline and fast.
mkSimLists <- function() {
  sl <- new("simLists")
  mk <- function(v) {
    s <- SpaDES.core::simInit(times = list(start = 0, end = 1))
    s$val <- v
    s
  }
  sl$sim1_1 <- mk(10)
  sl$sim1_2 <- mk(20)
  sl$sim2_1 <- mk(30)
  sl
}

test_that("as.data.table.simLists extracts a named object from every simList", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("purrr")
  withr::local_options(spades.moduleCodeChecks = FALSE)

  dt <- data.table::as.data.table(mkSimLists(), vals = "val")

  expect_s3_class(dt, "data.table")
  expect_true(all(c("simName", "vals", "value", "simList", "reps") %in% names(dt)))
  expect_identical(sort(dt$simName), c("sim1_1", "sim1_2", "sim2_1"))

  # simList / reps are split off the <name>_<rep> convention
  expect_identical(dt[simName == "sim1_2"]$simList, "sim1")
  expect_identical(dt[simName == "sim1_2"]$reps, "2")
  expect_identical(dt[simName == "sim1_2"]$value, 20)
})

test_that("as.data.table.simLists evaluates quoted expressions and multiple vals", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("purrr")
  withr::local_options(spades.moduleCodeChecks = FALSE)

  dt <- data.table::as.data.table(mkSimLists(),
                                  vals = list(doubled = quote(val * 2)))

  expect_identical(unique(dt$vals), "doubled")
  expect_identical(dt[simName == "sim1_1"]$value, 20)

  # a character val and a quoted val together
  dt2 <- data.table::as.data.table(mkSimLists(),
                                   vals = list("val", doubled = quote(val * 2)))
  expect_setequal(unique(dt2$vals), c("val", "doubled"))
})

test_that("as.data.table.simLists honours objectsFromSim = NA", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("purrr")
  withr::local_options(spades.moduleCodeChecks = FALSE)

  # NA means "take nothing from the simList", so `val` is not in scope and the
  # expression must fail rather than silently returning a stale value.
  expect_error(
    data.table::as.data.table(mkSimLists(), vals = "val", objectsFromSim = NA)
  )
})

test_that("as.data.table.simLists rejects a malformed objectsFromOutputs", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("purrr")
  withr::local_options(spades.moduleCodeChecks = FALSE)

  sl <- mkSimLists()

  # must be a list
  expect_error(
    data.table::as.data.table(sl, vals = "val", objectsFromOutputs = "val"),
    "must be a list"
  )

  # names must line up with vals when recycling
  expect_error(
    data.table::as.data.table(
      sl,
      vals = list(a = quote(val), b = quote(val * 2)),
      objectsFromOutputs = list(zzz = "val")
    ),
    "name order also does not match"
  )
})

test_that("show method summarises simLists and replicate counts", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.moduleCodeChecks = FALSE)

  out <- capture.output(show(mkSimLists()))

  expect_true(any(grepl("2 simLists", out)))
  expect_true(any(grepl("^sim1: sim1_1, \\.\\.\\., sim1_2", out)))
  expect_true(any(grepl("^sim2:", out)))
})
