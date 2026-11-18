# Extracted from test-plotSAs.R:376

# prequel ----------------------------------------------------------------------
skip_if_no_plotting <- function(extra = character()) {
  for (p in c("terra", "sf", "reproducible", "tidyterra", "RColorBrewer", extra))
    skip_if_not_installed(p)
}
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
localSetupStudyArea <- function(...) mkSA(4e6, 6e6, 1e6, 3e6)
useLocalCache <- function(env = parent.frame()) {
  withr::local_options(reproducible.cachePath = withr::local_tempdir(.local_envir = env),
                       reproducible.useCache = FALSE,
                       reproducible.verbose = -1,
                       .local_envir = env)
}
mkCatRTM <- function(sa = mkSA(), nlev = 3) {
  r <- terra::rast(terra::ext(sa), nrows = 6, ncols = 6, crs = theCRS)
  terra::values(r) <- rep(seq_len(nlev), length.out = 36)
  levels(r) <- data.frame(id = seq_len(nlev),
                          cls = paste0("class", seq_len(nlev)))
  names(r) <- "rasterToMatch"
  r
}
fillScale <- function(gg) {
  p <- if (inherits(gg, "patchwork")) gg[[1]] else gg
  Filter(function(s) "fill" %in% s$aesthetics, p$scales$scales)[[1]]
}
fillValues <- function(gg) {
  p <- if (inherits(gg, "patchwork")) gg[[1]] else gg
  unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
}
mkIdSA <- function(col = "ID", ids = c("6.1.1", "6.2.2")) {
  v <- rbind(terra::vect(terra::ext(5.0e6, 5.1e6, 2e6, 2.2e6), crs = theCRS),
             terra::vect(terra::ext(5.1e6, 5.2e6, 2e6, 2.2e6), crs = theCRS))
  v[[col]] <- ids
  v
}

# test -------------------------------------------------------------------------
skip_if_no_plotting(c("ggplot2", "patchwork", "tidyterra"))
useLocalCache()
testthat::local_mocked_bindings(setupStudyArea = localSetupStudyArea, .package = "SpaDES.project")
sa <- mkSA()
big <- mkCatRTM(mkSA(1e6, 9e6, -2e6, 6e6), nlev = 4)
names(big) <- "ELFs"
gg <- suppressWarnings(plotSAs(list(studyArea = sa, ELFs = big)))
p <- if (inherits(gg, "patchwork")) gg[[1]] else gg
pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
e <- list(xmin = pp$x_range[1], xmax = pp$x_range[2], ymin = pp$y_range[1], ymax = pp$y_range[2])
expect_lt(e$xmax - e$xmin, 2e6)
expect_lt(e$ymax - e$ymin, 2e6)
