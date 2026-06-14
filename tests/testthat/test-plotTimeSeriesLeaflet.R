## All tests skip on systems missing the heavy spatial/htmlwidgets stack so
## CRAN / minimal-Suggests CI matrix entries don't fail.
skip_if_not_installed("terra")
skip_if_not_installed("leaflet")
skip_if_not_installed("leafem")
skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

mkSpatRaster <- function(nlyr = 3, vals_offset = 0) {
  withr::local_seed(42)
  r <- terra::rast(nrows = 4, ncols = 4, nlyr = nlyr,
                   vals = runif(16 * nlyr) + vals_offset)
  names(r) <- paste0("year", seq(2020, by = 5, length.out = nlyr))
  r
}

test_that("plotTimeSeriesLeaflet: errors on non-raster input", {
  expect_error(plotTimeSeriesLeaflet("not a raster"),
               "multi-layer SpatRaster")
  expect_error(plotTimeSeriesLeaflet(list(1, 2, 3)),
               "multi-layer SpatRaster")
})

test_that("plotTimeSeriesLeaflet: errors when fewer than 2 layers", {
  r <- mkSpatRaster(nlyr = 1)
  expect_error(plotTimeSeriesLeaflet(r), "at least 2 layers")
})

test_that("plotTimeSeriesLeaflet: errors when years length doesn't match layers", {
  r <- mkSpatRaster(nlyr = 3)
  expect_error(plotTimeSeriesLeaflet(r, years = c(2020, 2025)),
               "does not match")
})

test_that("plotTimeSeriesLeaflet: returns leaflet widget; year labels reach the slider JS", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    m <- plotTimeSeriesLeaflet(r)

    expect_s3_class(m, "leaflet")
    ## the layer control radios are wired by leaflet itself; the slider JS
    ## is what we add via onRender -- the year strings must be in there
    js <- vapply(m$jsHooks$render, function(h) h$code, character(1))
    js_all <- paste(js, collapse = "\n")
    expect_match(js_all, "year2020")
    expect_match(js_all, "year2030")
    ## the slider position arg should round-trip
    expect_match(js_all, "bottomleft")
  })
})

test_that("plotTimeSeriesLeaflet: accepts a named list of SpatRaster", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    asList <- lapply(seq_len(terra::nlyr(r)), function(i) r[[i]])
    names(asList) <- names(r)
    m <- plotTimeSeriesLeaflet(asList)
    expect_s3_class(m, "leaflet")
  })
})

test_that("plotTimeSeriesLeaflet: custom sliderPosition is honoured", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    m <- plotTimeSeriesLeaflet(r, sliderPosition = "topright")
    js <- paste(vapply(m$jsHooks$render, function(h) h$code, character(1)),
                collapse = "\n")
    expect_match(js, "topright")
  })
})

test_that("plotChangeOverTime: errors on bad input or empty layer set", {
  expect_error(plotChangeOverTime("nope"), "multi-layer SpatRaster")
  ## bogus from/to: nothing differenced anywhere → diagnostic error
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    expect_error(plotChangeOverTime(r, from = "ghost"),
                 "No object had at least 2 named layers")
  })
})

test_that("plotChangeOverTime: returns leaflet widget; defaults to first/last; renders a legend", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)   # year2020, year2025, year2030
    m <- plotChangeOverTime(r)
    expect_s3_class(m, "leaflet")
    ## one baseGroup, labelled with first→last
    ctrl <- m$x$calls[vapply(m$x$calls, function(c) c$method == "addLayersControl", logical(1))]
    expect_true(length(ctrl) >= 1L)
    base_groups <- ctrl[[1L]]$args[[1L]]   # baseGroups arg
    expect_true(any(grepl("year2030", base_groups)))
    expect_true(any(grepl("year2020", base_groups)))
    ## continuous legend via addLegend (shine pattern: rev pal + reversed labFormat)
    legends <- m$x$calls[vapply(m$x$calls, function(c) c$method == "addLegend", logical(1))]
    expect_true(length(legends) >= 1L)
    ## legend toggle JS is injected via htmlwidgets::onRender
    expect_true(length(m$jsHooks$render) >= 1L)
    js <- paste(vapply(m$jsHooks$render, function(h) h$code, character(1)),
                collapse = "\n")
    expect_match(js, "baselayerchange")
    expect_match(js, "ts-legend-")
  })
})

test_that("plotChangeOverTime: explicit from/to overrides defaults", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    m <- plotChangeOverTime(r, from = "year2025", to = "year2030")
    ctrl <- m$x$calls[vapply(m$x$calls, function(c) c$method == "addLayersControl", logical(1))]
    base_groups <- ctrl[[1L]]$args[[1L]]
    expect_true(any(grepl("year2025", base_groups)))
    expect_true(any(grepl("year2030", base_groups)))
    expect_false(any(grepl("year2020", base_groups)))
  })
})

## --- helper coverage: disk-scan branch (simList/path) ---

stageOutputDir <- function(d) {
  ## stage three single-layer GeoTIFFs that look like a SpaDES outputPath:
  ## simPred_year2020.tif, simPred_year2025.tif, simPred_year2030.tif
  for (yr in c(2020, 2025, 2030)) {
    r <- terra::rast(nrows = 4, ncols = 4, vals = runif(16))
    terra::writeRaster(r, file.path(d, paste0("simPred_year", yr, ".tif")))
  }
  ## red herring to confirm the name filter works
  rr <- terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  terra::writeRaster(rr, file.path(d, "somethingElse_year2020.tif"))
  d
}

test_that(".coerceToLayerList: reads + sorts GeoTIFFs by year from a directory", {
  d <- withr::local_tempdir()
  stageOutputDir(d)
  lst <- SpaDES.project:::.coerceToLayerList(d, name = "simPred")
  expect_length(lst, 3L)
  expect_identical(names(lst), c("year2020", "year2025", "year2030"))
  expect_true(all(vapply(lst, inherits, logical(1), "SpatRaster")))
})

test_that(".coerceToLayerList: directory branch requires `name`", {
  d <- withr::local_tempdir()
  stageOutputDir(d)
  expect_error(SpaDES.project:::.coerceToLayerList(d, name = NULL),
               "`name` is required")
})

test_that(".coerceToLayerList: helpful error when name doesn't match any discovered time-series", {
  d <- withr::local_tempdir()
  stageOutputDir(d)
  ## new error lists the available object keys so the user can pick one
  expect_error(SpaDES.project:::.coerceToLayerList(d, name = "doesNotExist"),
               "not found among discovered time-series.*simPred.*somethingElse")
})

test_that(".coerceToLayerList: rejects nonsense input shapes", {
  expect_error(SpaDES.project:::.coerceToLayerList(42),
               "multi-layer SpatRaster")
  expect_error(SpaDES.project:::.coerceToLayerList("/no/such/dir", name = "x"),
               "multi-layer SpatRaster")
})

test_that("plotTimeSeriesLeaflet + plotChangeOverTime: accept a directory of GeoTIFFs", {
  withr::local_options(knitr.in.progress = TRUE)
  d <- withr::local_tempdir()
  stageOutputDir(d)
  withr::with_tempdir({
    m1 <- plotTimeSeriesLeaflet(d, name = "simPred")
    expect_s3_class(m1, "leaflet")
    js <- paste(vapply(m1$jsHooks$render, function(h) h$code, character(1)),
                collapse = "\n")
    expect_match(js, "year2020")
    expect_match(js, "year2030")

    m2 <- plotChangeOverTime(d)
    expect_s3_class(m2, "leaflet")
    ctrl <- m2$x$calls[vapply(m2$x$calls, function(c) c$method == "addLayersControl", logical(1))]
    base_groups <- ctrl[[1L]]$args[[1L]]   # baseGroups (radios)
    ## simPred (3 years) shows up; somethingElse (1 file) is skipped silently
    ## because plotChangeOverTime needs >= 2 layers per object
    expect_true(any(grepl("simPred",       base_groups)))
    expect_false(any(grepl("somethingElse", base_groups)))
  })
})
