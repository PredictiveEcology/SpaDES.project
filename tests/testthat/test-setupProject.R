test_that("test file-backed raster caching", {
  skip_on_cran()
  tmpdir1 <- Require::checkPath(file.path(tempdir(), .rndstr(1)), create = TRUE)
  setwd(tmpdir1)

  mess <- capture_messages(out <- setupProject()) # simplest case; just creates folders

  # set relative paths & modules
  mess <- capture_messages(
    out <- setupProject(name = "SpaDES.project",
                 paths = list(projectPath = "SpaDES.project",
                              modulePath = "m",
                              scratchPath = tempdir()),
                 modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  )

  expect_true(!isNamespaceLoaded("reproducible") )
  # With options and params set
  mess <- capture_messages(
    out <- SpaDES.project::setupProject(name = "SpaDES.project",
                                        options = list(reproducible.useTerra = TRUE),
                                        params = list(Biomass_borealDataPrep = list(.plots = "screen")),
                                        paths = list(modulePath = "m", projectPath = "SpaDES.project",
                                                     scratchPath = tempdir()),
                                        modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  )
  expect_true(!isNamespaceLoaded("reproducible") )

  # using an options file that is remote
  mess <- capture_messages(
    out <- setupProject(name = "SpaDES.project",
                        options = c("PredictiveEcology/SpaDES.project@transition/inst/options.R"),
                        params = list(Biomass_borealDataPrep = list(.plots = "screen")),
                        paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project",
                                     scratchPath = tempdir()),
                        modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  )
  expect_true(!isNamespaceLoaded("reproducible") )

  # setting arbitrary arguments
  mess <- capture_messages(
    out <- setupProject(modules = "PredictiveEcology/Biomass_borealDataPrep@development",
                        sideEffects = "PredictiveEcology/SpaDES.project@transition/inst/sideEffects.R",
                        defaultDots = list(mode = "development",
                                           studyAreaName = "MB"),
                        mode = mode, studyAreaName = studyAreaName,
                        # params = list("Biomass_borealDataPrep" = list(.useCache = mode))
    )
  )

  err <- capture_error(
  mess <- capture_messages(
    out <- setupProject(paths = list(projectPath = "LandWeb"),
                        modules = "PredictiveEcology/Biomass_borealDataPrep@development",
                        config = "LandWeb",
                        defaultDots = list(mode = "development",
                                           studyAreaName = "MB"),
                        mode = mode, studyAreaName = studyAreaName,
                        # params = list("Biomass_borealDataPrep" = list(.useCache = mode))
    )
  )
  )

  # Pass args from GlobalEnv
  studyAreaName <- "AB"
  mess <- capture_messages(
    out <- setupProject(paths = list(projectPath = "LandWeb"),
                        modules = "PredictiveEcology/Biomass_borealDataPrep@development",
                        defaultDots = list(mode = "development",
                                           studyAreaName = "MB"),
                        mode = "development", studyAreaName = studyAreaName,
    )
  )

  # mixture of named list element, github file and local file for e.g., options
  mess <- capture_messages(
    out <-
      setupProject(name = "SpaDES.project",
                   options = list(reproducible.useTerra = TRUE,
                                  "PredictiveEcology/SpaDES.project@transition/inst/options.R",
                                  system.file("authentication.R", package = "SpaDES.project")), # local file
                   params = list(Biomass_borealDataPrep = list(.plots = "screen")),
                   paths = list(modulePath = "m", projectPath = "SpaDES.project",
                                scratchPath = tempdir()),
                   modules = "PredictiveEcology/Biomass_borealDataPrep@development"
      )
  )
  expect_true(!isNamespaceLoaded("reproducible") )


  # If using SpaDES.core, the return object can be passed to `simInit` via `do.call`
  #   do.call(simInit, out)

  # load packages using `require` argument -- now loads SpaDES.core & reproducible
  mess <- capture_messages(
    out <- SpaDES.project::setupProject(
      paths = list(projectPath = "MEE_Paper"), # will deduce name of project from projectPath
      standAlone = TRUE,
      require =
        c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
          "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
      modules = c("PredictiveEcology/Biomass_speciesData@master",
                  "PredictiveEcology/Biomass_borealDataPrep@development",
                  "PredictiveEcology/Biomass_core@master",
                  "PredictiveEcology/Biomass_validationKNN@master",
                  "PredictiveEcology/Biomass_speciesParameters@development")

    )
  )

  # example with studyArea, left in long-lat, for Alberta and British Columbia, Canada
  mess <- capture_messages(
    out <- setupProject(studyArea = list("Al|Brit"))
  )
  expect_true(isNamespaceLoaded("reproducible") )

  # example 2 with studyArea, converted to BC Albers 3005, Alberta, BC, SK,
  #    with level 2 administrative boundaries
  mess <- capture_messages(
    out <- setupProject(studyArea = list("Al|Brit|Sas", level = 2, epsg = "3005"))
  )

})


## Make project-level change to .libPaths() that is persistent
test_that("test file-backed raster caching", {
  skip_on_cran()
  tmpdir1 <- Require::checkPath(file.path(tempdir(), .rndstr(1)), create = TRUE)
  setwd(tmpdir1)
  setupProject(package = "terra",
               updateRprofile = TRUE)

})
