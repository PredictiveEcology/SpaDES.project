## `paths$modulePath` may hold several directories, each module living in any
## one of them. getModule() used to pair module i with path i (recycling), and
## setupModules() ran `if (!dir.exists(<one per path>))` -- "the condition has
## length > 1". Offline: fixture modules on disk.

mkModule <- function(modulePath, name, reqdPkgs) {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf('  version = list(%s = "1.0.0"),', name),
      sprintf("  reqdPkgs = list(%s)", reqdPkgs),
      "))"),
    file.path(modulePath, name, paste0(name, ".R"))
  )
}

mkTwoModulePaths <- function(envir = parent.frame()) {
  mps <- c(withr::local_tempdir(.local_envir = envir),
           withr::local_tempdir(.local_envir = envir))
  mkModule(mps[1], "modA", '"data.table"')
  mkModule(mps[2], "modB", '"fs"')
  mps
}

test_that("whichModulePath finds each module in whichever modulePath holds it", {
  mps <- mkTwoModulePaths()
  expect_identical(whichModulePath(c("modB", "modA"), mps), mps[2:1])
  ## not found anywhere: the first path, where a download would go
  expect_identical(whichModulePath("modZ", mps), mps[1])
})

test_that("getModule finds local modules across several modulePaths, in any order", {
  mps <- mkTwoModulePaths()
  for (mods in list(c("modA", "modB"), c("modB", "modA"), "modB")) {
    out <- getModule(mods, modulePath = mps, verbose = -1)
    expect_setequal(out$success, mods)
    expect_length(out$failed, 0L)
  }
})

test_that("setupModules reads reqdPkgs from modules spread over several modulePaths", {
  withr::local_options(Require.updateRprofile = NULL)
  mps <- mkTwoModulePaths()
  proj <- withr::local_tempdir()
  for (mods in list(c("modA", "modB"), c("modB", "modA"))) {
    pkgs <- setupModules(paths = list(modulePath = mps, projectPath = proj),
                         modules = mods, inProject = TRUE, useGit = FALSE,
                         updateRprofile = FALSE, verbose = -1)
    expect_identical(names(pkgs), mods)
    expect_identical(pkgs[["modA"]], "data.table")
    expect_identical(pkgs[["modB"]], "fs")
  }
})
