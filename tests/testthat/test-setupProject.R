## NOTE: first run of these tests will take a while due to package installation;
## however, subsequent runs will use the Require package cache.
## Be sure to periodically cleanup the Require package cache manually :
##   Cache directory is here: `Require::RequireCacheDir()`
##   Clear the entire cache using: `Require::clearRequirePackageCache()`
##

test_that("test setupProject - simplest", {
  skip_on_cran()
  setupTest() # setwd, sets .libPaths() to a temp

  ## simplest case; just creates folders
  warns <- capture_warnings( #
    mess <- capture_messages({
      out <- setupProject(
        name = paste0("test_SpaDES_project_", .rndstr(1)),
      )
    })
  )
  expect_true(all(grepl("won\\'t be read upon restart", warns)))
  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
})

test_that("test setupProject - relative paths and modules", {
  skip_on_cran()
  nam <- "test_SpaDES_project"
  setupTest(first = TRUE, name = nam) # setwd, sets .libPaths() to a temp
  ## set relative paths & modules
  warn <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(
        name = nam,
        paths = list(projectPath = name,
                     modulePath = "m",
                     scratchPath = tempdir()),
        modules = "PredictiveEcology/Biomass_borealDataPrep@development"
      )
    })
  )

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 1) # .globals for .studyAreaName
})

test_that("test setupProject - options and params", {
  skip_on_cran()
  setupTest()
  ## With options and params set
  warn <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(
        name = paste0("test_SpaDES_project_", .rndstr(1)),
        # name = "test_SpaDES_project",
        options = list(reproducible.useTerra = TRUE),
        params = list(Biomass_borealDataPrep = list(.plots = "screen")),
        paths = list(modulePath = "m",
                     scratchPath = tempdir()),
        modules = "PredictiveEcology/Biomass_borealDataPrep@development"
      )
    })
  )
  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 2)
})

test_that("test setupProject - remote options file", {
  skip_on_cran()
  setupTest()
  ## using an options file that is remote
  warn <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(
        name = paste0("test_SpaDES_project_", .rndstr(1)),
        options = c("PredictiveEcology/SpaDES.project@transition/inst/options.R"),
        params = list(Biomass_borealDataPrep = list(.plots = "screen")),
        paths = list(modulePath = "m",
                     scratchPath = tempdir()),
        modules = "PredictiveEcology/Biomass_borealDataPrep@development"
      )
    })
  )
  withr::defer(try(options(attr(out, "projectOptions"))))

  expect_true(all(names(out) %in% c("modules", "paths", "params", "times")))
  expect_true(fs::path_has_parent(out$paths$modulePath, getwd()))
  expect_true(dir(out$paths$modulePath) %in% Require::extractPkgName(out$modules))
  expect_true(length(out$params) == 2)
  expect_true(!is.null(getOption("LandR.assertions"))) # pick one from the file
  expect_true(is.list(attr(out, "projectOptions"))) # pick one from the file
  options(attr(out, "projectOptions"))
})

test_that("test setupProject - arbitrary arguments", {
  # skip("Not completed tests yet")
  skip_on_cran()
  setupTest("geodata", "reproducible") ## setting arbitrary arguments
  jur <- "MB"
  mod <- "development"
  # expect_false(googledrive::drive_has_token())

  module <- "PredictiveEcology/Biomass_borealDataPrep@development"
  mess <- capture_messages({
    out <- setupProject(
      modules = module,
      sideEffects = "PredictiveEcology/SpaDES.project@transition/inst/sideEffects.R",
      defaultDots = list(mode = mod,
                         studyAreaName = jur),
      packages = NULL,
      updateRprofile = FALSE,
      mode = mode, studyAreaName = studyAreaName,
    )
  })
  expect_identical(out$studyAreaName, jur)
  expect_identical(out$mode, mod)
  expect_true(length(dir(out$paths$modulePath, pattern = extractPkgName(module))) == 1)
  expect_true(length(
    dir(out$paths$modulePath, recursive = TRUE,
        pattern = paste0(extractPkgName(module), ".R$"))) == 1)

  # if (user("emcintir"))
  #   expect_true(sum(grepl("Authenticating as", mess)) == 1)
})

test_that("test setupProject - args from global envir", {
  skip("Not completed tests yet")
  skip_on_cran()
  setupTest("geodata", "reproducible")
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
})

test_that("test setupProject - mixture of named list elements", {
  skip("Not completed tests yet")
  skip_on_cran()
  setupTest()
  ## mixture of named list element, github file and local file for e.g., options
  mess <- capture_messages({
    out <- setupProject(
      name = paste0("test_SpaDES_project_", .rndstr(1)),
      # name = "test_SpaDES_project",
      options = list(reproducible.useTerra = TRUE,
                     "PredictiveEcology/SpaDES.project@transition/inst/options.R",
                     system.file("authentication.R", package = "SpaDES.project")), # local file
      params = list(Biomass_borealDataPrep = list(.plots = "screen")),
      paths = list(modulePath = "m", # projectPath = "test",
                   scratchPath = tempdir()),
      modules = "PredictiveEcology/Biomass_borealDataPrep@development"
    )
  })
})

test_that("test setupProject - load packages using require argument", {
  skip("Not completed tests yet")
  skip_on_cran()
  setupTest()
  ## load packages using `require` argument -- now loads SpaDES.core & reproducible
  mess <- capture_messages({
    out <- setupProject(
      paths = list(projectPath = paste0("MEE_Paper", .rndstr(1))), # will deduce name of project from projectPath
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

})

test_that("test setupProject - studyArea in lonlat", {

  skip_on_cran()
  setupTest(c("geodata", "filelock", "reproducible")) # filelock is unnecessary "first time", but errors if run again
  jurs <- "Al|Brit"
  ## example with studyArea, left in long-lat, for Alberta and British Columbia, Canada
  mess <- capture_messages(
    out <- setupProject(studyArea = list(jurs), updateRprofile = FALSE, verbose = -2)
  )

  expect_true(length(mess) == 0)
  expect_true(!is.null(out$studyArea))
  expect_true(is(out$studyArea, "SpatVector"))
  expect_true(NROW(unique(out$studyArea[["NAME_1"]])) == length(strsplit(jurs, "\\|")[[1]]))
  expect_true(terra::is.lonlat(out$studyArea))
})

test_that("test setupProject -studyArea using CRS", {
  skip_on_cran()
  ## example 2 with studyArea, converted to BC Albers 3005, Alberta, BC, SK,
  ##    with level 2 administrative boundaries
  setupTest("geodata", "reproducible")
  jurs <- "Al|Brit|Sas"
  epsg <- "3005"

  warns <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(studyArea = list("Al|Brit|Sas", level = 2, epsg = epsg))
    }))
  expect_true(!is.null(out$studyArea))
  expect_true(is(out$studyArea, "SpatVector"))
  expect_true(NROW(unique(out$studyArea[["NAME_1"]])) == length(strsplit(jurs, "\\|")[[1]]))
  expect_equal(terra::crs(out$studyArea, describe = TRUE)$code, epsg)
  expect_false(terra::is.lonlat(out$studyArea))
  expect_true(grepl("but the projectPath is the tempdir", warns))

})

## Make project-level change to .libPaths() that is persistent
test_that("projectPath is in a tempdir", {
  skip_on_cran()
  setupTest()

  warns <- capture_warnings(
    out <- setupProject(package = "terra", updateRprofile = TRUE, verbose = -1))

  expect_true(grepl("but the projectPath is the tempdir", warns))
})

test_that("projectPath is in a tempdir", {
  skip("config not working yet")

  skip_on_cran()
  setupTest("geodata", "reproducible")

  err <- capture_error({
    mess <- capture_messages({
      out <- setupProject(
        paths = list(projectPath = paste0("test_LandWeb", .rndstr(1))),
        modules = "PredictiveEcology/Biomass_borealDataPrep@development",
        config = "LandWeb",
        defaultDots = list(mode = "development", studyAreaName = "MB"),
        mode = mode, studyAreaName = studyAreaName#,
      )
    })
  })

})

test_that("test setupProject - nested GH modules", {
  skip_on_cran()
  skip("nested castor GH modules")
  setupTest() # setwd, sets .libPaths() to a temp
  ## set relative paths & modules
  warn <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(
        name = paste0("test_SpaDES_project_", .rndstr(1)),
        paths = list(modulePath = "m",
                     scratchPath = tempdir()),
        modules = "bcgov/castor@main/R/SpaDES-modules/dataCastor"
      )
    })
  )
  expect_true(dir(out$paths$modulePath) %in% "dataCastor")   ## failing -- castor repo and module exist in m/

  warn <- capture_warnings(
    mess <- capture_messages({
      out <- setupProject(
        name = paste0("test_SpaDES_project_", .rndstr(1)),
        paths = list(modulePath = "m",
                     scratchPath = tempdir()),
        modules = c("bcgov/castor@main/R/SpaDES-modules/dataCastor",
                    "PredictiveEcology/Biomass_borealDataPrep@development")
      )
    })
  )
  expect_true(all(dir(out$paths$modulePath) %in% c("dataCastor", "Biomass_borealDataPrep")))   ## failing -- someother issue.
})

