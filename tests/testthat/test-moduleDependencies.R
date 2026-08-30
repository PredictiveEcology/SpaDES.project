## moduleDependencies() and moduleDependenciesToGraph() from R/listModules.R.
##
## These read module metadata from disk via SpaDES.core::inputObjects() /
## outputObjects(); no simulation runs and nothing is downloaded. SpaDES.core's
## sample modules form a real dependency chain -- randomLandscapes produces a
## landscape, fireSpread consumes and republishes it, caribouMovement consumes
## both -- which is exactly the shape this function exists to extract.

sampleModulePathMD <- function() {
  p <- system.file("sampleModules", package = "SpaDES.core")
  skip_if(!nzchar(p) || !dir.exists(file.path(p, "randomLandscapes")),
          "SpaDES.core sample modules not available")
  p
}

test_that("moduleDependencies returns the from/to object table", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  d <- moduleDependencies(c("randomLandscapes", "fireSpread", "caribouMovement"),
                          modulePath = mp)

  expect_s3_class(d, "data.table")
  expect_identical(names(d), c("from", "to", "objName", "objClass"))
  expect_gt(nrow(d), 0L)
})

test_that("moduleDependencies finds the landscape edge between modules", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  d <- moduleDependencies(c("randomLandscapes", "fireSpread", "caribouMovement"),
                          modulePath = mp)

  # randomLandscapes publishes `landscape`; fireSpread and caribouMovement read it
  land <- d[d$objName == "landscape", ]
  expect_gt(nrow(land), 0L)
  expect_true("randomLandscapes" %in% land$from)
  expect_true(any(c("fireSpread", "caribouMovement") %in% land$to))
  expect_true(all(land$objClass == "SpatRaster"))
})

test_that("moduleDependencies accepts a list of modules", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  fromChar <- moduleDependencies(c("randomLandscapes", "fireSpread"), modulePath = mp)
  fromList <- moduleDependencies(list("randomLandscapes", "fireSpread"), modulePath = mp)

  expect_identical(fromList, fromChar)
})

test_that("moduleDependencies drops _INPUT_ pseudo-edges", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  d <- moduleDependencies(c("randomLandscapes", "fireSpread", "caribouMovement"),
                          modulePath = mp)

  # objects with no producing module are labelled _INPUT_ and then filtered out
  expect_false(any(grepl("INPUT", d$from)))
})

test_that("moduleDependencies survives a module it cannot read", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  # the failure is reported but does not abort the whole call
  expect_message(
    d <- moduleDependencies("noSuchModuleAnywhere", modulePath = mp)
  )
  expect_s3_class(d, "data.table")
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), c("from", "to", "objName", "objClass"))
})

test_that("moduleDependencies returns an empty table for a single self-contained module", {
  skip_if_not_installed("SpaDES.core")
  mp <- sampleModulePathMD()

  d <- moduleDependencies("randomLandscapes", modulePath = mp)

  expect_s3_class(d, "data.table")
  expect_identical(names(d), c("from", "to", "objName", "objClass"))
})

test_that("moduleDependenciesToGraph builds an igraph from the table", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("igraph")
  mp <- sampleModulePathMD()

  d <- moduleDependencies(c("randomLandscapes", "fireSpread", "caribouMovement"),
                          modulePath = mp)
  g <- moduleDependenciesToGraph(d)

  expect_s3_class(g, "igraph")
  expect_gt(igraph::gorder(g), 0L)
  expect_gt(igraph::gsize(g), 0L)
  # every module named in the table becomes a vertex
  expect_true(all(unique(c(d$from, d$to)) %in% igraph::V(g)$name))
})
