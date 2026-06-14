test_that(".leafletGeoTiffPath: tempfile when knitr is NOT in progress", {
  withr::local_options(knitr.in.progress = NULL)
  path <- SpaDES.project:::.leafletGeoTiffPath("myraster")

  expect_match(path, "\\.tif$")
  ## must be inside the session tempdir -- the existing interactive behaviour
  expect_true(startsWith(
    normalizePath(path, mustWork = FALSE),
    normalizePath(tempdir(), mustWork = FALSE)
  ))
})

test_that(".leafletGeoTiffPath: knitr::fig_path-based when knitr IS in progress", {
  withr::local_options(knitr.in.progress = TRUE)

  ## isolate cwd so the helper's dir.create() / fig_path don't litter the repo
  withr::with_tempdir({
    path <- SpaDES.project:::.leafletGeoTiffPath("myraster")

    expect_match(path, "\\.tif$")
    ## raster name embedded so multiple rasters in one chunk don't clobber
    expect_match(path, "myraster")
    ## NOT a tempdir path -- the whole point of the change
    expect_false(startsWith(
      normalizePath(path, mustWork = FALSE),
      normalizePath(tempdir(), mustWork = FALSE)
    ))
    ## fig_path returns a relative path -- the browser must be able to resolve
    ## it against the rendered HTML page's URL
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
