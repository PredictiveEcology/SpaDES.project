test_that("newProject (basic) creates necessary files", {
  myProjDir <- SpaDES.project::newProject("basicProject", tempdir(), open = FALSE) ## "basic" is default type

  expect_true(dir.exists(file.path(myProjDir, "cache")))
  expect_true(dir.exists(file.path(myProjDir, "inputs")))
  expect_true(dir.exists(file.path(myProjDir, "modules")))
  expect_true(dir.exists(file.path(myProjDir, "outputs")))
  expect_true(file.exists(file.path(myProjDir, "README.md")))

  unlink(myProjDir, recursive = TRUE) ## cleanup
})

test_that("newProject (advanced) creates necessary files", {
  myProjDir <- SpaDES.project::newProject("advancedProject", tempdir(), type = "advanced", open = FALSE)

  expect_true(dir.exists(file.path(myProjDir, "cache")))
  expect_true(dir.exists(file.path(myProjDir, "inputs")))
  expect_true(dir.exists(file.path(myProjDir, "modules")))
  expect_true(dir.exists(file.path(myProjDir, "outputs")))
  expect_true(dir.exists(file.path(myProjDir, "packages")))

  expect_true(file.exists(file.path(myProjDir, ".Rprofile")))
  expect_true(file.exists(file.path(myProjDir, "config.yml")))
  expect_true(file.exists(file.path(myProjDir, "00-global.R")))
  expect_true(file.exists(file.path(myProjDir, "01-init.R")))
  expect_true(file.exists(file.path(myProjDir, "02-paths.R")))
  expect_true(file.exists(file.path(myProjDir, "03-packages.R")))
  expect_true(file.exists(file.path(myProjDir, "04-options.R")))
  expect_true(file.exists(file.path(myProjDir, "05-google-ids.R")))
  expect_true(file.exists(file.path(myProjDir, "06-studyArea.R")))
  expect_true(file.exists(file.path(myProjDir, "07-dataPrep.R")))
  expect_true(file.exists(file.path(myProjDir, "08-pre-sim.R")))
  expect_true(file.exists(file.path(myProjDir, "09-main-sim.R")))

  unlink(myProjDir, recursive = TRUE) ## cleanup
})
