## plotSAs() / plotSAsLeaflet(): the two "equivalent" entry points, one drawing
## to a graphics device via ggplot2/patchwork, the other building a leaflet
## widget.
##
## setupStudyArea() is mocked throughout. Unmocked it downloads GADM country
## boundaries, which would make these tests network-dependent and slow; the
## returned polygon is only ever used as a base layer, so a synthetic one
## exercises the same code.

skip_if_no_plotting <- function(extra = character()) {
  for (p in c("terra", "sf", "reproducible", "tidyterra", "RColorBrewer", extra))
    skip_if_not_installed(p)
}

## A projected CRS (Canada Lambert) keeps buffs() metric: plotSAs() enlarges the
## crop extent when the study area is smaller than minArea, and that arithmetic
## is in map units.
theCRS <- "epsg:3347"

mkSA <- function(xmin = 5e6, xmax = 5.2e6, ymin = 2e6, ymax = 2.2e6) {
  terra::vect(terra::ext(xmin, xmax, ymin, ymax), crs = theCRS)
}

mkRTM <- function(sa = mkSA(), nrows = 8, ncols = 8) {
  r <- terra::rast(terra::ext(sa), nrows = nrows, ncols = ncols, crs = theCRS)
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "rasterToMatch"
  r
}

## The base layer plotSAs() would otherwise download: a polygon comfortably
## larger than the study area, in the same CRS.
localSetupStudyArea <- function(...) mkSA(4e6, 6e6, 1e6, 3e6)

useLocalCache <- function(env = parent.frame()) {
  withr::local_options(reproducible.cachePath = withr::local_tempdir(.local_envir = env),
                       reproducible.useCache = FALSE,
                       reproducible.verbose = -1,
                       .local_envir = env)
}

# --- plotSAs ------------------------------------------------------------------

test_that("plotSAs returns a patchwork object for a studyArea + rasterToMatch", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkRTM())

  gg <- suppressWarnings(plotSAs(ll))

  expect_s3_class(gg, "patchwork")
  expect_s3_class(gg, "ggplot")
})

test_that("plotSAs draws to a png device", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkRTM())
  f <- file.path(withr::local_tempdir(), "sa.png")

  gg <- suppressWarnings(plotSAs(ll))
  png(f, width = 400, height = 400)
  on.exit(if (grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
  suppressWarnings(print(gg))
  grDevices::dev.off()

  # the plot really rendered rather than merely being constructed
  expect_true(file.exists(f))
  expect_gt(file.size(f), 1000)
})

test_that("plotSAs handles the study-area-only case", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  # no rasterToMatch: study areas are drawn on their own panel over the base map
  ll <- list(studyArea = mkSA(), studyAreaLarge = mkSA(4.9e6, 5.3e6, 1.9e6, 2.3e6))

  gg <- suppressWarnings(plotSAs(ll))

  expect_s3_class(gg, "patchwork")
})

test_that("plotSAs errors when there is nothing spatial to plot", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  expect_error(plotSAs(list(notSpatial = 1L), include = FALSE),
               "No spatial objects to plot")
})

# --- plotSAsLeaflet -----------------------------------------------------------

test_that("plotSAsLeaflet returns a leaflet widget", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()
  ll <- list(studyArea = mkSA(), rasterToMatch = mkRTM())

  a <- suppressWarnings(plotSAsLeaflet(ll))

  expect_s3_class(a, "leaflet")
  expect_s3_class(a, "htmlwidget")
})

test_that("plotSAsLeaflet handles the study-area-only case", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()
  ll <- list(studyArea = mkSA())

  a <- suppressWarnings(plotSAsLeaflet(ll))

  expect_s3_class(a, "leaflet")
})

test_that("plotSAsLeaflet errors when there is nothing spatial to plot", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  expect_error(plotSAsLeaflet(list(notSpatial = 1L), include = FALSE),
               "No spatial objects to plot")
})
