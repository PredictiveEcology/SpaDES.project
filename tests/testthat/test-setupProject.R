## NOTE: first run of these tests will take a while due to package installation;
## however, subsequent runs will use the Require package cache.
## Be sure to periodically cleanup the Require package cache manually :
##   Cache directory is here: `Require::RequireCacheDir()`
##   Clear the entire cache using: `Require::clearRequirePackageCache()`
##

test_that("test setupProject - simplest", {
  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## simplest case; just creates folders
  mess <- capture_messages({
    out <- setupProject(
      name = "test_SpaDES_project"
    )
  })

  ## TODO: `testthat` dependency `waldo` missing from project's libpath
  utils::install.packages("waldo", lib = head(.libPaths(), 1))

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - relative paths and modules", {
  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## set relative paths & modules
  mess <- capture_messages({
    out <- setupProject(
      name = "test_SpaDES_project",
      paths = list(projectPath = "test_SpaDES_project",
                   modulePath = "m",
                   scratchPath = tempdir()),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  })

  ## TODO: `testthat` dependency `waldo` missing from project's libpath
  utils::install.packages("waldo", lib = head(.libPaths(), 1))

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 0)

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - options and params", {
  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## With options and params set
  mess <- capture_messages({
    out <- setupProject(
      name = "test_SpaDES_project",
      options = list(reproducible.useTerra = TRUE),
      params = list(Biomass_borealDataPrep = list(.plots = "screen")),
      paths = list(modulePath = "m",
                   projectPath = "test",
                   scratchPath = tempdir()),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  })

  ## TODO: `testthat` dependency `waldo` missing from project's libpath
  install.packages("waldo", lib = head(.libPaths(), 1))

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 1)

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - remote options file", {
  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## using an options file that is remote
  mess <- capture_messages({
    out <- setupProject(
      name = "test_SpaDES_project",
      options = c("PredictiveEcology/SpaDES.project@transition/inst/options.R"),
      params = list(Biomass_borealDataPrep = list(.plots = "screen")),
      paths = list(modulePath = "m",
                   projectPath = "test",
                   scratchPath = tempdir()),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  })
  withr::defer(try(options(attr(out, "projectOptions"))))

  ## TODO: `testthat` dependency `waldo` missing from project's libpath
  install.packages("waldo", lib = head(.libPaths(), 1))

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 1)
  expect_true(!is.null(getOption("LandR.assertions"))) # pick one from the file
  expect_true(is.list(attr(out, "projectOptions"))) # pick one from the file
  options(attr(out, "projectOptions"))

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - arbitrary arguments", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## setting arbitrary arguments
  mess <- capture_messages({
    out <- setupProject(
      modules = "PredictiveEcology/Biomass_borealDataPrep@development",
      sideEffects = "PredictiveEcology/SpaDES.project@transition/inst/sideEffects.R",
      defaultDots = list(mode = "development",
                         studyAreaName = "MB"),
      mode = mode, studyAreaName = studyAreaName,
      # params = list("Biomass_borealDataPrep" = list(.useCache = mode))
    )
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - args from global envir", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## Pass args from GlobalEnv
  studyAreaName <- "AB"
  mess <- capture_messages({
    out <- setupProject(
      paths = list(projectPath = "LandWeb"),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development",
      defaultDots = list(mode = "development",
                         studyAreaName = "MB"),
      mode = "development",
      studyAreaName = studyAreaName)
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - mixture of named list elements", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## mixture of named list element, github file and local file for e.g., options
  mess <- capture_messages({
    out <- setupProject(
      name = "test_SpaDES_project",
      options = list(reproducible.useTerra = TRUE,
                     "PredictiveEcology/SpaDES.project@transition/inst/options.R",
                     system.file("authentication.R", package = "SpaDES.project")), # local file
      params = list(Biomass_borealDataPrep = list(.plots = "screen")),
      paths = list(modulePath = "m", projectPath = "test",
                   scratchPath = tempdir()),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
})

test_that("test setupProject - load packages using require argument", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## load packages using `require` argument -- now loads SpaDES.core & reproducible
  mess <- capture_messages({
    out <- setupProject(
      paths = list(projectPath = "MEE_Paper"), # will deduce name of project from projectPath
      standAlone = TRUE,
      require = c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
      modules = c("PredictiveEcology/Biomass_speciesData@master",
                  "PredictiveEcology/Biomass_borealDataPrep@development",
                  "PredictiveEcology/Biomass_core@master",
                  "PredictiveEcology/Biomass_validationKNN@master",
                  "PredictiveEcology/Biomass_speciesParameters@development")
    )
  })

  ### cleanup
  withr::defer(setwd(cwd))
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject - studyArea in lonlat", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")
  ## NOTE: tests/testthat/setup.R is run before each test and uses withr funs

  ## example with studyArea, left in long-lat, for Alberta and British Columbia, Canada
  mess <- capture_messages({
    out <- setupProject(studyArea = list("Al|Brit"))
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("test setupProject -studyArea using CRS", {
  skip("Not completed tests yet")

  skip_on_cran()
  skip_if_not_installed("withr")
  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  ## example 2 with studyArea, converted to BC Albers 3005, Alberta, BC, SK,
  ##    with level 2 administrative boundaries
  mess <- capture_messages({
    out <- setupProject(studyArea = list("Al|Brit|Sas", level = 2, epsg = "3005"))
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

## Make project-level change to .libPaths() that is persistent
test_that("projectPath is in a tempdir", {
  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  expect_warning({
    out <- setupProject(package = "terra", updateRprofile = TRUE)
  }, regexp = "but the projectPath is the tempdir")

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})

test_that("projectPath is in a tempdir", {
  skip("config not working yet")

  skip_on_cran()
  skip_if_not_installed("withr")

  origLibPaths <- .libPaths()
  withr::defer(.libPaths(origLibPaths))

  ## TODO: Error in getCRANrepos(repos) : Please set a CRAN mirror
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org")
    )
  )

  tmpdir <- Require::tempdir2(sub = .rndstr(1))
  withr::local_dir(tmpdir)

  err <- capture_error({
    mess <- capture_messages({
      out <- setupProject(
        paths = list(projectPath = "test_LandWeb"),
        modules = "PredictiveEcology/Biomass_borealDataPrep@development",
        config = "LandWeb",
        defaultDots = list(mode = "development", studyAreaName = "MB"),
        mode = mode, studyAreaName = studyAreaName#,
        # params = list("Biomass_borealDataPrep" = list(.useCache = mode))
      )
    })
  })

  ### cleanup
  withr::defer(.teardownProject(out$paths, origLibPaths))
  withr::deferred_run()
})
