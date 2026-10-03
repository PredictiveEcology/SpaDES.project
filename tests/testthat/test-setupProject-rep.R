test_that("setupProject - a .rep dot becomes params$.globals$.rep", {
  skip_on_cran()
  setupTest()
  pp <- list(packagePath = .libPaths()[1L])
  nm <- paste0("test_SpaDES_project_", .rndstr(1))
  quietly <- function(x) suppressWarnings(suppressMessages(x))

  ## (1) .rep supplied, not set in .globals: .globals$.rep is that .rep, as integer
  out <- quietly(setupProject(name = nm, paths = pp, packages = NULL, .rep = 3,
                              params = list(.globals = list(.studyAreaName = "x"))))
  expect_identical(out$params$.globals$.rep, 3L)

  ## .rep resolved from defaultDots is treated the same
  out <- quietly(setupProject(name = nm, paths = pp, packages = NULL, defaultDots = list(.rep = 2),
                              params = list(.globals = list(.studyAreaName = "x"))))
  expect_identical(out$params$.globals$.rep, 2L)

  ## (2) an explicit .globals$.rep wins
  out <- quietly(setupProject(name = nm, paths = pp, packages = NULL, .rep = 3,
                              params = list(.globals = list(.rep = 9L))))
  expect_identical(out$params$.globals$.rep, 9L)

  ## (3) no .rep: .globals has no .rep
  out <- quietly(setupProject(name = nm, paths = pp, packages = NULL,
                              params = list(.globals = list(.studyAreaName = "x"))))
  expect_false(".rep" %in% names(out$params$.globals))

  ## (4) a toy module with a `.rep` parameter: its own value is kept beside .globals$.rep
  mp <- tempfile("mods")
  dir.create(file.path(mp, "modA"), recursive = TRUE)
  writeLines('defineModule(sim, list(name = "modA", version = list(modA = "0.0.1"),
    parameters = bindrows(defineParameter(".rep", "integer", 1L, NA, NA, "replicate")),
    inputObjects = bindrows(), outputObjects = bindrows(), reqdPkgs = list()))',
    file.path(mp, "modA", "modA.R"))
  out <- quietly(setupProject(name = nm, paths = c(pp, list(modulePath = mp)), packages = NULL,
                              modules = "modA", .rep = 3, params = list(modA = list(.rep = 7L))))
  expect_identical(out$params$modA$.rep, 7L)
  expect_identical(out$params$.globals$.rep, 3L)
})
