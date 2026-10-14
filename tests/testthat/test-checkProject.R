## checkProject() from R/checkProject.R, on tiny temp modules. No network, no simInit.

cpPaths <- function(mp, root = withr::local_tempdir(.local_envir = parent.frame())) {
  list(modulePath = mp, inputPath = file.path(root, "inputs"), cachePath = file.path(root, "cache"),
       outputPath = file.path(root, "outputs"), scratchPath = file.path(root, "scratch"))
}
cpSim <- function(mp, modules, ..., times = list(start = 2000, end = 2010)) {
  list(modules = modules, paths = cpPaths(mp), times = times, ...)
}
cpFlag <- function(res, chk) res[res$check == chk, ]

test_that("checkProject returns the documented table, empty when nothing is wrong", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "modA")
  res <- checkProject(cpSim(mp, "modA"), verbose = 0)
  expect_s3_class(res, "data.table")
  expect_identical(names(res), c("check", "severity", "module", "message", "fix"))
  expect_identical(nrow(res), 0L)
})

test_that("checkProject flags times that are not calendar years when a module reads year-indexed data", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "yr", params = "defineParameter(\"dataYear\", \"numeric\", 2020, NA, NA, \"d\")")
  diWriteModule(mp, "plain")

  expect_identical(nrow(cpFlag(checkProject(cpSim(mp, "yr"), verbose = 0), "times")), 0L)
  r <- cpFlag(checkProject(cpSim(mp, "yr", times = list(start = 0, end = 10)), verbose = 0), "times")
  expect_identical(r$module, "yr")
  expect_match(r$message, "calendar years")
  r <- cpFlag(checkProject(cpSim(mp, "yr", times = NULL), verbose = 0), "times")
  expect_identical(nrow(r), 1L)
  expect_identical(nrow(cpFlag(checkProject(cpSim(mp, "plain", times = NULL), verbose = 0), "times")), 0L)
})

test_that("checkProject flags a module whose .inputObjects uses an object another module creates in init", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "maker", outputs = diOutput("studyArea"), inputs = diInput("other", NA))
  diWriteModule(mp, "other", outputs = diOutput("other"))
  diWriteModule(mp, "user", inputs = diInput("studyArea", NA),
                code = c(".inputObjects <- function(sim) {",
                         "  if (!suppliedElsewhere(\"studyArea\", sim)) sim$studyArea <- 1",
                         "  sim",
                         "}"))
  diWriteModule(mp, "quiet", inputs = diInput("studyArea", NA),
                code = ".inputObjects <- function(sim) sim")

  withr::local_options(spades.allowInitDuringSimInit = FALSE)
  r <- cpFlag(checkProject(cpSim(mp, c("maker", "other", "user", "quiet")), verbose = 0), "studyAreaOrder")
  expect_identical(r$module, "user")
  expect_identical(r$severity, "warning")
  expect_match(r$fix, "spades.allowInitDuringSimInit", fixed = TRUE)
  expect_match(r$message, "maker", fixed = TRUE)
  expect_match(r$message, "cannot run early", fixed = TRUE) # maker itself expects `other`

  withr::local_options(spades.allowInitDuringSimInit = TRUE)
  r <- cpFlag(checkProject(cpSim(mp, c("maker", "other", "user")), verbose = 0), "studyAreaOrder")
  expect_identical(r$severity, "note")
  expect_identical(r$module, "maker")
})

test_that("checkProject lists Google Drive datasets when there are no credentials", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "drv", inputs = c(diInput("a", "https://drive.google.com/file/d/abc"),
                                      diInput("b", "https://example.org/b.tif")))
  sim <- cpSim(mp, "drv")

  local_mocked_bindings(.cpDriveCredentials = function() FALSE)
  r <- cpFlag(checkProject(sim, verbose = 0), "googleDrive")
  expect_identical(r$module, "drv")
  expect_match(r$message, "drive.google.com/file/d/abc", fixed = TRUE)
  expect_false(grepl("example.org", r$message))

  local_mocked_bindings(.cpDriveCredentials = function() TRUE)
  expect_identical(nrow(cpFlag(checkProject(sim, verbose = 0), "googleDrive")), 0L)
})

test_that(".cpDriveCredentials reads the gargle option and environment variables", {
  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GOOGLEDRIVE_AUTH = NA, GARGLE_SERVICE_ACCOUNT = NA)
  has <- function() isTRUE(tryCatch(.cpDriveCredentials(), error = function(e) FALSE))
  ## with googledrive absent or without a token this is FALSE; the explicit settings make it TRUE
  withr::local_options(gargle_oauth_email = "a@b.c")
  expect_true(has())
  withr::local_options(gargle_oauth_email = NULL)
  withr::local_envvar(GARGLE_SERVICE_ACCOUNT = "/some/key.json")
  expect_true(has())
})

test_that("checkProject warns about low disk space and is quiet above the threshold", {
  skip_on_os("windows")
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "modA")
  sim <- cpSim(mp, "modA")
  withr::local_options(SpaDES.project.checkProject.minFreeGB = 1e9)
  r <- cpFlag(checkProject(sim, verbose = 0), "disk")
  expect_gt(nrow(r), 0L)
  expect_match(r$message[1L], "GB free", fixed = TRUE)
  withr::local_options(SpaDES.project.checkProject.minFreeGB = 0)
  expect_identical(nrow(cpFlag(checkProject(sim, verbose = 0), "disk")), 0L)
})

test_that("checkProject flags a parameter two modules define with different effective values", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  p <- function(nm, v) sprintf("defineParameter(\"%s\", \"logical\", %s, NA, NA, \"d\")", nm, v)
  diWriteModule(mp, "fitA", params = c(p("refitExisting", "FALSE"), p("same", "TRUE")))
  diWriteModule(mp, "fitB", params = c(p("refitExisting", "TRUE"), p("same", "TRUE")),
                code = "chk <- function(sim) SpaDES.core::paramCheckOtherMods(sim, \"refitExisting\")")

  r <- cpFlag(checkProject(cpSim(mp, c("fitA", "fitB")), verbose = 0), "linkedParams")
  expect_identical(nrow(r), 1L)
  expect_match(r$message, "refitExisting")
  expect_match(r$message, "fitA=FALSE")
  expect_match(r$message, "fitB=TRUE")
  expect_match(r$message, "paramCheckOtherMods", fixed = TRUE)
  expect_false(grepl("same", r$message))

})

test_that("checkProject agrees when .globals sets the shared parameter", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  p <- function(v) sprintf("defineParameter(\"refitExisting\", \"logical\", %s, NA, NA, \"d\")", v)
  diWriteModule(mp, "fitA", params = p("FALSE"))
  diWriteModule(mp, "fitB", params = p("TRUE"))
  g <- cpSim(mp, c("fitA", "fitB"), params = list(.globals = list(refitExisting = TRUE)))
  expect_identical(nrow(cpFlag(checkProject(g, verbose = 0), "linkedParams")), 0L)
  g <- cpSim(mp, c("fitA", "fitB"),
             params = list(fitA = list(refitExisting = FALSE), .globals = list(refitExisting = TRUE)))
  expect_identical(nrow(cpFlag(checkProject(g, verbose = 0), "linkedParams")), 1L)
})

test_that("checkProject notes a default study area that will be used", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "needs", inputs = diInput("studyArea", "https://example.org/sa.shp"))
  diWriteModule(mp, "makes", outputs = diOutput("studyArea"))

  r <- cpFlag(checkProject(cpSim(mp, "needs"), verbose = 0), "fallbackStudyArea")
  expect_identical(r$module, "needs")
  expect_identical(r$severity, "note")
  expect_match(r$message, "https://example.org/sa.shp", fixed = TRUE)
  expect_identical(nrow(cpFlag(checkProject(cpSim(mp, c("needs", "makes")), verbose = 0),
                               "fallbackStudyArea")), 0L)
  expect_identical(nrow(cpFlag(checkProject(cpSim(mp, "needs", objects = list(studyArea = 1)),
                                            verbose = 0), "fallbackStudyArea")), 0L)
})

test_that("option SpaDES.project.checkProject = FALSE silences the check run by setupProject", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "needs", inputs = diInput("studyArea", "https://example.org/sa.shp"))
  out <- cpSim(mp, "needs")
  withr::local_options(SpaDES.project.checkProject = FALSE)
  expect_silent(.checkProjectOnSetup(out, verbose = 1))
  withr::local_options(SpaDES.project.checkProject = TRUE)
  expect_message(.checkProjectOnSetup(out, verbose = 1), "checkProject")
  ## never an error, never changes the object
  expect_no_error(.checkProjectOnSetup(list(modules = "nope"), verbose = 1))
})
