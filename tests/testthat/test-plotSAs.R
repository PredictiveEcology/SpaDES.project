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
  v <- terra::vect(terra::ext(xmin, xmax, ymin, ymax), crs = theCRS)
  ## Realistic: real study areas carry an attribute table. The attribute-less
  ## case is covered separately below -- it used to break plotSAsLeaflet().
  v$name <- "studyArea"
  v
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

## A categorical (factor) rasterToMatch -- e.g. ELF classes. ggplot2 gives it a
## discrete fill aesthetic, so plotSAs() must pick a discrete scale; a continuous
## one fails with "Discrete value supplied to a continuous scale".
mkCatRTM <- function(sa = mkSA(), nlev = 3) {
  r <- terra::rast(terra::ext(sa), nrows = 6, ncols = 6, crs = theCRS)
  terra::values(r) <- rep(seq_len(nlev), length.out = 36)
  levels(r) <- data.frame(id = seq_len(nlev),
                          cls = paste0("class", seq_len(nlev)))
  names(r) <- "rasterToMatch"
  r
}

## the fill scale ggplot2 actually resolved for the (single) panel
fillScale <- function(gg) {
  p <- if (inherits(gg, "patchwork")) gg[[1]] else gg
  Filter(function(s) "fill" %in% s$aesthetics, p$scales$scales)[[1]]
}

fillValues <- function(gg) {
  p <- if (inherits(gg, "patchwork")) gg[[1]] else gg
  unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
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

# --- palette resolution -------------------------------------------------------
## Both entry points accept the same palette names but previously consumed them
## differently, which is where they had drifted apart. .paletteRampFun() is the
## single resolver they now share.

test_that(".paletteRampFun expands an RColorBrewer name to colours", {
  skip_if_not_installed("RColorBrewer")

  cols <- SpaDES.project:::.paletteRampFun("Set1")(12)

  expect_length(cols, 12L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that(".paletteRampFun expands a whitebox name to colours", {
  skip_if_not_installed("tidyterra")

  cols <- SpaDES.project:::.paletteRampFun("muted")(12)

  expect_length(cols, 12L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}", cols)))
})

test_that(".paletteRampFun passes explicit colours through", {
  # the pass-through branch is reached only after the Brewer test, which reads
  # RColorBrewer::brewer.pal.info
  skip_if_not_installed("RColorBrewer")

  expect_identical(SpaDES.project:::.paletteRampFun("#FF0000")(3),
                   rep("#FF0000", 3))
})

test_that("plotSAs gives each rasterToMatch its own palette", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  sa <- mkSA()
  r1 <- mkRTM(sa); r2 <- mkRTM(sa)
  ll <- list(studyArea = sa, rasterToMatch1 = r1, rasterToMatch2 = r2)

  gg <- suppressWarnings(plotSAs(ll, rasterToMatchPalette = c("Set1", "Greens")))

  fills1 <- unique(ggplot2::ggplot_build(gg[[1]])$data[[1]]$fill)
  fills2 <- unique(ggplot2::ggplot_build(gg[[2]])$data[[1]]$fill)
  # taking rasterToMatchPalette[[1]] for every panel would make these identical
  expect_false(identical(fills1, fills2))
})

test_that("plotSAsLeaflet sends literal colours to addGeotiff, not a palette name", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()
  ll <- list(studyArea = mkSA(), rasterToMatch = mkRTM())

  a <- suppressWarnings(plotSAsLeaflet(ll))

  geo <- Filter(function(x) identical(x$method, "addGeotiff"), a$x$calls)
  expect_length(geo, 1L)
  args <- geo[[1]]$args
  opts <- args[[which(vapply(args, function(z) is.list(z) && "palette" %in% names(z),
                             logical(1)))]]
  # leafem::colorOptions() accepts a bare name but stores the string itself,
  # which reaches the widget as a one-element palette of the text "Set1"
  expect_gt(length(opts$palette), 1L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}", opts$palette)))
})

# --- categorical vs continuous rasterToMatch ---------------------------------
## plotSAs() crosses two choices: Brewer vs whitebox palette, and categorical vs
## continuous raster. Only Brewer + continuous was exercised before.

test_that("plotSAs uses a discrete scale for a categorical raster with a Brewer palette", {
  skip_if_no_plotting(c("ggplot2", "patchwork"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkCatRTM(nlev = 3))

  gg <- suppressWarnings(plotSAs(ll, rasterToMatchPalette = "Set1"))

  # scale_fill_manual(), not scale_fill_gradientn()
  expect_true(fillScale(gg)$is_discrete())
  expect_length(fillValues(gg), 3L)
})

test_that("plotSAs uses a discrete scale for a categorical raster with a whitebox palette", {
  skip_if_no_plotting(c("ggplot2", "patchwork", "tidyterra"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkCatRTM(nlev = 3))

  gg <- suppressWarnings(plotSAs(ll, rasterToMatchPalette = "muted"))

  # scale_fill_whitebox_d(), not scale_fill_whitebox_c()
  expect_true(fillScale(gg)$is_discrete())
  expect_length(fillValues(gg), 3L)
})

test_that("plotSAs picks different colours for whitebox than for Brewer", {
  skip_if_no_plotting(c("ggplot2", "patchwork", "tidyterra"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkCatRTM(nlev = 3))

  brew <- fillValues(suppressWarnings(plotSAs(ll, rasterToMatchPalette = "Set1")))
  wbox <- fillValues(suppressWarnings(plotSAs(ll, rasterToMatchPalette = "muted")))

  # proves the two categorical branches are genuinely distinct, not just both discrete
  expect_false(identical(brew, wbox))
})

test_that("plotSAs uses a continuous scale for a continuous raster with a whitebox palette", {
  skip_if_no_plotting(c("ggplot2", "patchwork", "tidyterra"))
  useLocalCache()
  testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea,
                                  .package = "SpaDES.project")
  ll <- list(studyArea = mkSA(), rasterToMatch = mkRTM())

  gg <- suppressWarnings(plotSAs(ll, rasterToMatchPalette = "muted"))

  # scale_fill_whitebox_c()
  expect_false(fillScale(gg)$is_discrete())
})

test_that("plotSAs handles a categorical raster in plotSAsLeaflet too", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()
  ll <- list(studyArea = mkSA(), rasterToMatch = mkCatRTM(nlev = 3))

  a <- suppressWarnings(plotSAsLeaflet(ll))

  expect_s3_class(a, "leaflet")
})
test_that("plotSAsLeaflet works with a study area that has no attribute table", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()
  # a bare extent -- 0 columns of attributes. The polygon label used to be a
  # ~formula, which made leaflet resolve it against metaData(<SpatVector>) and
  # fail here under newer terra ("differing number of rows: 0, 1").
  bare <- terra::vect(terra::ext(5e6, 5.2e6, 2e6, 2.2e6), crs = theCRS)
  expect_equal(ncol(bare), 0)   # SpatVector ncol() is a double

  a <- suppressWarnings(plotSAsLeaflet(list(studyArea = bare,
                                            rasterToMatch = mkRTM())))

  expect_s3_class(a, "leaflet")
})

test_that("plotSAsLeaflet labels each study-area polygon with its layer name", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()

  a <- suppressWarnings(plotSAsLeaflet(list(studyArea = mkSA(),
                                            rasterToMatch = mkRTM())))

  pg <- Filter(function(x) identical(x$method, "addPolygons"), a$x$calls)
  expect_length(pg, 1L)
  # the label survives as the layer name, formula or not
  expect_true(any(vapply(pg[[1]]$args,
                         function(z) isTRUE(any(unlist(z) == "studyArea")),
                         logical(1))))
})

## `labelCols`: per-feature hover labels. mkSA()'s attribute column is `name`,
## which is deliberately NOT one of the defaults, so the tests above keep
## exercising the layer-name fallback.
mkIdSA <- function(col = "ID", ids = c("6.1.1", "6.2.2")) {
  v <- rbind(terra::vect(terra::ext(5.0e6, 5.1e6, 2e6, 2.2e6), crs = theCRS),
             terra::vect(terra::ext(5.1e6, 5.2e6, 2e6, 2.2e6), crs = theCRS))
  v[[col]] <- ids
  v
}

test_that("plotSAsLeaflet labels each polygon with its own ID, not the layer name", {
  skip_if_no_plotting(c("leaflet", "leafem"))
  useLocalCache()

  a <- suppressWarnings(plotSAsLeaflet(list(studyArea = mkIdSA(),
                                            rasterToMatch = mkRTM())))

  pg <- Filter(function(x) identical(x$method, "addPolygons"), a$x$calls)
  labs <- unlist(pg[[1]]$args)
  # one label per feature, the ID values -- not a single "studyArea"
  expect_true(all(c("6.1.1", "6.2.2") %in% labs))
})

test_that(".saHoverLabels picks the first available column, else the layer name", {
  skip_if_not_installed("terra")

  # `Name` when `ID` is absent; `ID` wins when both are present
  expect_equal(.saHoverLabels(mkIdSA("Name", c("a", "b")), "sa", c("ID", "Name", "Names")),
               c("a", "b"))
  v <- mkIdSA("ID", c("x", "y"))
  v$Name <- c("ignored1", "ignored2")
  expect_equal(.saHoverLabels(v, "sa", c("ID", "Name", "Names")), c("x", "y"))

  # no matching column, no attribute table, and no labelCols: fall back
  expect_equal(.saHoverLabels(mkIdSA("other", c("a", "b")), "sa", c("ID", "Name")), "sa")
  expect_equal(.saHoverLabels(mkSA(), "sa", c("ID", "Name")), "sa")
  expect_equal(.saHoverLabels(mkIdSA(), "sa", NULL), "sa")

  # NA / empty entries would render as blank tooltips
  expect_equal(.saHoverLabels(mkIdSA("ID", c(NA, "")), "sa", "ID"), c("sa", "sa"))

  # non-character columns are coerced, so leaflet always gets a character vector
  expect_equal(.saHoverLabels(mkIdSA("ID", c(1L, 2L)), "sa", "ID"), c("1", "2"))
})
