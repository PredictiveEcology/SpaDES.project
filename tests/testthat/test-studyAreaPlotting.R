test_that(".leafletGeoTiffPath: tempfile when knitr is NOT in progress", {
  withr::local_options(knitr.in.progress = NULL)
  path <- SpaDES.project:::.leafletGeoTiffPath("myraster")

  expect_match(path, "\\.tif$")
  ## `tempfile()` puts files directly in `tempdir()` -- compare raw strings,
  ## NOT `normalizePath()`, because on macOS/Windows the file doesn't exist
  ## yet, so `normalizePath(path, mustWork = FALSE)` won't resolve symlinks
  ## consistently with `normalizePath(tempdir())` (which does, because tempdir
  ## exists). That mismatch was the prior CI failure.
  expect_identical(dirname(path), tempdir())
})

test_that(".leafletGeoTiffPath: knitr::fig_path-based when knitr IS in progress", {
  withr::local_options(knitr.in.progress = TRUE)

  ## isolate cwd so the helper's dir.create() / fig_path don't litter the repo
  withr::with_tempdir({
    path <- SpaDES.project:::.leafletGeoTiffPath("myraster")

    expect_match(path, "\\.tif$")
    ## raster name embedded so multiple rasters in one chunk don't clobber
    expect_match(path, "myraster")
    ## fig_path returns a path relative to the qmd source dir; the browser
    ## must be able to resolve it against the rendered HTML page's URL
    expect_false(fs::is_absolute_path(path))
    ## the parent directory must exist by the time we return -- writeRaster()
    ## would otherwise fail on the very next line in plotSAsLeaflet()
    expect_true(dir.exists(dirname(path)))
  })
})

test_that(".leafletGeoTiffPath: unique paths per raster name within one chunk", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::with_tempdir({
    p1 <- SpaDES.project:::.leafletGeoTiffPath("rasterA")
    p2 <- SpaDES.project:::.leafletGeoTiffPath("rasterB")
    expect_false(identical(p1, p2))
  })
})
