## Time-series discovery and coercion helpers behind plotTimeSeriesLeaflet() /
## plotChangeOverTime(): .scanOutputDirForTimeSeries(), .coerceToMultiObjects()
## and .leafletGeoTiffPath(). These run on real (tiny) GeoTIFFs; no plotting and
## no leaflet widget is built.

mkRast <- function(nlyr = 1, vals = 1:4) {
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = nlyr,
                   xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- rep(vals, length.out = 4 * nlyr)
  r
}

writeTif <- function(dir, name, nlyr = 1) {
  p <- file.path(dir, paste0(name, ".tif"))
  suppressWarnings(terra::writeRaster(mkRast(nlyr), p, overwrite = TRUE))
  p
}

# --- .leafletGeoTiffPath ------------------------------------------------------

test_that(".leafletGeoTiffPath returns a tempfile outside a knitr render", {
  p <- SpaDES.project:::.leafletGeoTiffPath("rtm one")

  expect_true(grepl("\\.tif$", p))
  expect_identical(dirname(p), dirname(tempfile()))
})

test_that(".leafletGeoTiffPath writes under fig_path during a knitr render", {
  skip_if_not_installed("knitr")
  withr::local_options(knitr.in.progress = TRUE)

  p <- SpaDES.project:::.leafletGeoTiffPath("rtm one")

  expect_true(grepl("\\.tif$", p))
  # name is sanitised, and the containing directory is created
  expect_true(grepl("rtm.one", basename(p)))
  expect_true(dir.exists(dirname(p)))
})

# --- .scanOutputDirForTimeSeries ----------------------------------------------

test_that(".scanOutputDirForTimeSeries returns an empty list for a dir with no tifs", {
  d <- withr::local_tempdir()
  writeLines("not a raster", file.path(d, "readme.txt"))

  expect_identical(SpaDES.project:::.scanOutputDirForTimeSeries(d), list())
})

test_that(".scanOutputDirForTimeSeries groups files by object and sorts by time", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "biomass_year2020")
  writeTif(d, "biomass_year2010")

  objs <- SpaDES.project:::.scanOutputDirForTimeSeries(d)

  # the trailing "year" is stripped from the key
  expect_named(objs, "biomass")
  expect_identical(objs$biomass$times$time, c(2010, 2020))
  expect_identical(objs$biomass$band, 1L)
})

test_that(".scanOutputDirForTimeSeries keeps distinct objects apart", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "biomass_year2010")
  writeTif(d, "burnMap_year2010")

  objs <- SpaDES.project:::.scanOutputDirForTimeSeries(d)

  expect_setequal(names(objs), c("biomass", "burnMap"))
})

test_that(".scanOutputDirForTimeSeries emits one object per band for multi-band files", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "stack_year2010", nlyr = 2)

  objs <- SpaDES.project:::.scanOutputDirForTimeSeries(d)

  expect_length(objs, 2L)
  expect_identical(unname(vapply(objs, `[[`, integer(1), "band")), 1:2)
})

test_that(".scanOutputDirForTimeSeries records NA time when no digits are present", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "elevation")

  objs <- SpaDES.project:::.scanOutputDirForTimeSeries(d)

  expect_named(objs, "elevation")
  expect_true(is.na(objs$elevation$times$time))
})

# --- .coerceToMultiObjects ----------------------------------------------------

test_that(".coerceToMultiObjects splits a multi-layer SpatRaster into layers", {
  skip_if_not_installed("terra")
  r <- mkRast(nlyr = 3)
  names(r) <- c("a", "b", "c")

  out <- SpaDES.project:::.coerceToMultiObjects(r)

  expect_named(out, "raster")
  expect_length(out$raster, 3L)
  expect_named(out$raster, c("a", "b", "c"))
  expect_s4_class(out$raster[[1]], "SpatRaster")
})

test_that(".coerceToMultiObjects passes a list of SpatRasters through", {
  skip_if_not_installed("terra")
  ll <- list(y2010 = mkRast(), y2020 = mkRast())

  out <- SpaDES.project:::.coerceToMultiObjects(ll)

  expect_named(out, "raster")
  expect_named(out$raster, c("y2010", "y2020"))
})

test_that(".coerceToMultiObjects reads a directory of GeoTIFFs", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "biomass_year2010")
  writeTif(d, "biomass_year2020")

  out <- SpaDES.project:::.coerceToMultiObjects(d)

  expect_named(out, "biomass")
  # layers are named from the parsed year
  expect_named(out$biomass, c("year2010", "year2020"))
})

test_that(".coerceToMultiObjects rejects an unusable x", {
  expect_error(SpaDES.project:::.coerceToMultiObjects(42),
               "must be a multi-layer SpatRaster")
})

test_that(".coerceToMultiObjects errors when a directory holds no time series", {
  d <- withr::local_tempdir()

  expect_error(SpaDES.project:::.coerceToMultiObjects(d),
               "No time-series objects discovered")
})

# --- .coerceToLayerList -------------------------------------------------------

test_that(".coerceToLayerList splits a multi-layer SpatRaster into named layers", {
  skip_if_not_installed("terra")
  r <- mkRast(nlyr = 2)
  names(r) <- c("first", "second")

  out <- SpaDES.project:::.coerceToLayerList(r)

  expect_length(out, 2L)
  expect_named(out, c("first", "second"))
})

test_that(".coerceToLayerList passes a list of SpatRasters through unchanged", {
  skip_if_not_installed("terra")
  ll <- list(a = mkRast(), b = mkRast())

  expect_identical(SpaDES.project:::.coerceToLayerList(ll), ll)
})

test_that(".coerceToLayerList rejects an unusable x", {
  expect_error(SpaDES.project:::.coerceToLayerList(42),
               "must be a multi-layer SpatRaster")
})

test_that(".coerceToLayerList requires a name when reading from a directory", {
  d <- withr::local_tempdir()

  expect_error(SpaDES.project:::.coerceToLayerList(d),
               "`name` is required")
})

test_that(".coerceToLayerList reports the names it did find", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "biomass_year2010")

  expect_error(
    SpaDES.project:::.coerceToLayerList(d, name = "notThere"),
    "not found among discovered time-series"
  )
})

test_that(".coerceToLayerList reads the named object from a directory", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  writeTif(d, "biomass_year2010")
  writeTif(d, "biomass_year2020")

  out <- SpaDES.project:::.coerceToLayerList(d, name = "biomass")

  expect_length(out, 2L)
  expect_named(out, c("year2010", "year2020"))
  expect_s4_class(out[[1]], "SpatRaster")
})
