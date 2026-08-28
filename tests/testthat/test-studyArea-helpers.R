## Pure geometry / palette helpers from R/studyAreaPlotting.R.
## Synthetic terra objects only -- no downloads, no plotting.
##
## Two helpers in this file are deliberately NOT covered here because they are
## defective; see the note at the bottom rather than tests pinning their current
## behaviour.

mkVect <- function(xmin = 0, xmax = 10, ymin = 0, ymax = 5, crs = "epsg:4326") {
  terra::vect(terra::ext(xmin, xmax, ymin, ymax), crs = crs)
}

mkRast <- function(xmin = 0, xmax = 10, ymin = 0, ymax = 5, vals = 1) {
  r <- terra::rast(terra::ext(xmin, xmax, ymin, ymax), nrows = 5, ncols = 10,
                   crs = "epsg:4326")
  terra::values(r) <- vals
  r
}

test_that("minmaxFn reads each extent edge off a SpatVector", {
  skip_if_not_installed("terra")
  v <- mkVect(xmin = 1, xmax = 9, ymin = 2, ymax = 4)

  expect_equal(SpaDES.project:::minmaxFn(v, "xmin"), 1)
  expect_equal(SpaDES.project:::minmaxFn(v, "xmax"), 9)
  expect_equal(SpaDES.project:::minmaxFn(v, "ymin"), 2)
  expect_equal(SpaDES.project:::minmaxFn(v, "ymax"), 4)
})

test_that("the x/y min/max wrappers agree with minmaxFn", {
  skip_if_not_installed("terra")
  v <- mkVect(xmin = 1, xmax = 9, ymin = 2, ymax = 4)

  expect_equal(SpaDES.project:::xminFn(v), 1)
  expect_equal(SpaDES.project:::xmaxFn(v), 9)
  expect_equal(SpaDES.project:::yminFn(v), 2)
  expect_equal(SpaDES.project:::ymaxFn(v), 4)
})

test_that("minmaxFn works on a SpatRaster and returns NULL for other classes", {
  skip_if_not_installed("terra")

  expect_equal(SpaDES.project:::xmaxFn(mkRast(xmax = 7)), 7)
  expect_null(SpaDES.project:::minmaxFn(data.frame(x = 1), "xmin"))
  expect_null(SpaDES.project:::minmaxFn("not spatial", "xmin"))
})

test_that("minmaxFn reads an sf bbox", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  s <- sf::st_as_sf(mkVect(xmin = 3, xmax = 8, ymin = 1, ymax = 6))

  expect_equal(SpaDES.project:::xminFn(s), 3)
  expect_equal(SpaDES.project:::ymaxFn(s), 6)
})

test_that("extInLatLong returns an extent for terra and a bbox for sf", {
  skip_if_not_installed("terra")
  v <- mkVect()

  e <- SpaDES.project:::extInLatLong(v)
  expect_s4_class(e, "SpatExtent")

  expect_null(SpaDES.project:::extInLatLong(data.frame(a = 1)))
  expect_null(SpaDES.project:::extInLatLong(42))
})

test_that("extInLatLong returns an sf bbox for sf input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  s <- sf::st_as_sf(mkVect())

  expect_s3_class(SpaDES.project:::extInLatLong(s), "bbox")
})

test_that("areas sums polygon area for a SpatVector", {
  skip_if_not_installed("terra")

  small <- SpaDES.project:::areas(mkVect(xmax = 1, ymax = 1))
  big   <- SpaDES.project:::areas(mkVect(xmax = 2, ymax = 2))

  expect_type(small, "double")
  expect_gt(big, small)
})

test_that("areas counts non-NA cells for a SpatRaster and NULL otherwise", {
  skip_if_not_installed("terra")
  r <- mkRast(vals = c(rep(1, 25), rep(NA, 25)))

  a <- SpaDES.project:::areas(r)

  # 25 non-NA cells times cell area
  expect_equal(a, 25 * prod(terra::res(r)))
  expect_null(SpaDES.project:::areas(data.frame(a = 1)))
})

test_that("buffs buffers a SpatVector and grows its extent", {
  skip_if_not_installed("terra")
  v <- mkVect(xmin = 0, xmax = 1, ymin = 0, ymax = 1)

  b <- SpaDES.project:::buffs(v, width = 100000)

  expect_s4_class(b, "SpatVector")
  expect_lt(SpaDES.project:::xminFn(b), SpaDES.project:::xminFn(v))
  expect_gt(SpaDES.project:::xmaxFn(b), SpaDES.project:::xmaxFn(v))
})

test_that("buffs accepts dist as an alias for width on SpatVector", {
  skip_if_not_installed("terra")
  v <- mkVect(xmin = 0, xmax = 1, ymin = 0, ymax = 1)

  byWidth <- SpaDES.project:::buffs(v, width = 50000)
  byDist  <- SpaDES.project:::buffs(v, dist = 50000)

  expect_equal(SpaDES.project:::xmaxFn(byDist), SpaDES.project:::xmaxFn(byWidth))
})

test_that("appendDotsToLL keeps only studyArea / rasterToMatch entries", {
  dots <- list(studyArea = 1, rasterToMatch = 2, studyAreaLarge = 3,
               unrelated = 4, times = 5)

  res <- SpaDES.project:::appendDotsToLL(dots = dots)

  expect_setequal(names(res), c("studyArea", "rasterToMatch", "studyAreaLarge"))
  expect_false("unrelated" %in% names(res))
})

test_that("appendDotsToLL appends to an existing list", {
  res <- SpaDES.project:::appendDotsToLL(ll = list(existing = 0),
                                         dots = list(studyArea = 1, junk = 2))

  expect_setequal(names(res), c("existing", "studyArea"))
})

test_that("hasNames flags which elements carry a name", {
  expect_identical(SpaDES.project:::hasNames(c(a = "Reds", "Blues")), c(TRUE, FALSE))
})

test_that("hasNames reports no names for a wholly unnamed vector", {
  # names() is NULL there, so nzchar() gives logical(0) rather than a FALSE per
  # element. Both callers only ever ask any(hasName), which is what is asserted;
  # pinning the length would fix an incidental shape rather than the contract.
  expect_false(any(SpaDES.project:::hasNames(c("Reds", "Blues"))))
})

test_that("rasterToMatchPaletteUpdate recycles unnamed palettes to the raster count", {
  res <- SpaDES.project:::rasterToMatchPaletteUpdate(c("Reds"), c("r1", "r2", "r3"))

  expect_length(res, 3L)
  expect_true(all(res == "Reds"))
})

test_that("rasterToMatchPaletteUpdate drops named entries before recycling", {
  # named entries belong to specific rasters and are handled separately
  res <- SpaDES.project:::rasterToMatchPaletteUpdate(c(r1 = "Blues", "Reds"),
                                                     c("r1", "r2"))

  expect_length(res, 2L)
  expect_true(all(res == "Reds"))
})

## NOT covered here, reported instead -- writing tests would pin behaviour that
## looks wrong rather than intended:
##
## toLatLong(ll, rtmsNames, sasNames) never returns the whole `ll`. It has no
## explicit return, so its value is whatever the last conditional
## sub-assignment yields:
##     both name vectors empty  -> NULL
##     only rtmsNames supplied  -> NULL
##     both supplied            -> only the sasNames subset
## Both call sites do `ll <- toLatLong(ll, ...)` -- plotSAsLeaflet() line ~304
## unconditionally, plotSAs() line ~78 when latlong = TRUE.
##
## rasterToMatchPaletteNamed(x) returns the FUNCTION ITSELF when no element of
## `x` is named: the local `rasterToMatchPaletteNamed` is only assigned inside
## `if (any(hasName))`, so otherwise the name resolves to the enclosing function
## by lexical scoping.
