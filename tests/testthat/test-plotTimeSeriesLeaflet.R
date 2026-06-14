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

test_that("plotChangeOverTime: errors on bad input or unnamed layers", {
  expect_error(plotChangeOverTime("nope"), "multi-layer SpatRaster")
  r <- mkSpatRaster(nlyr = 3)
  names(r) <- NULL
  expect_error(plotChangeOverTime(r), "must be named")
})

test_that("plotChangeOverTime: errors on bogus from/to selectors", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    expect_error(plotChangeOverTime(r, from = "ghost"), "not in layer names")
    expect_error(plotChangeOverTime(r, to   = "ghost"), "not in layer names")
  })
})

test_that("plotChangeOverTime: returns leaflet widget; defaults to first/last", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)   # year2020, year2025, year2030
    m <- plotChangeOverTime(r)
    expect_s3_class(m, "leaflet")
    ## the layer group name we add should be "year2030 − year2020" (first→last)
    ctrl <- m$x$calls[vapply(m$x$calls, function(c) c$method == "addLayersControl", logical(1))]
    expect_true(length(ctrl) >= 1L)
    overlays <- ctrl[[1L]]$args[[2L]]   # overlayGroups arg
    expect_true(any(grepl("year2030", overlays)))
    expect_true(any(grepl("year2020", overlays)))
  })
})

test_that("plotChangeOverTime: explicit from/to overrides defaults", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    r <- mkSpatRaster(nlyr = 3)
    m <- plotChangeOverTime(r, from = "year2025", to = "year2030")
    ctrl <- m$x$calls[vapply(m$x$calls, function(c) c$method == "addLayersControl", logical(1))]
    overlays <- ctrl[[1L]]$args[[2L]]
    expect_true(any(grepl("year2025", overlays)))
    expect_true(any(grepl("year2030", overlays)))
    expect_false(any(grepl("year2020", overlays)))
  })
})
