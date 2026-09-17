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

test_that("nested GitHub modules are flattened into the modulePath they were downloaded to", {
  ## With several modulePaths, flattening used `sub(..., paths$modulePath, ...)`
  ## (only the first used) and checked `file.path(modulePath, modules)` pairwise,
  ## so it undid itself; the added nested modulePath entry was also paired
  ## positionally, pointing at a directory that did not exist.
  skip_on_cran()
  skip_if_offline()
  setupTest()
  ## normalised, as setupProject() returns them: macOS /var -> /private/var,
  ## Windows 8.3 short names and backslashes
  root <- normPath(withr::local_tempdir())
  mps <- file.path(root, c("mp1", "mp2"))
  mkModule(mps[2], "localMod", '"fs"')
  nested <- c("PredictiveEcology/scfm@development/modules/scfmLandcoverInit",
              "PredictiveEcology/scfm@development/modules/scfmRegime")
  warns <- capture_warnings(suppressMessages(
    out <- setupProject(modules = c("localMod", nested),
                        paths = list(modulePath = mps, projectPath = file.path(root, "proj"),
                                     packagePath = .libPaths()[1L]),
                        packages = NULL, useGit = FALSE, updateRprofile = FALSE)))
  expect_false(any(grepl("replacement", warns)))
  expect_true(all(dir.exists(file.path(mps[1], c("scfmLandcoverInit", "scfmRegime")))))
  expect_true(dir.exists(file.path(mps[2], "localMod")))
  expect_false(dir.exists(file.path(mps[1], "scfm")))   # superfolder removed after copying
  expect_identical(out$paths$modulePath[1:2], mps)
  expect_true(all(dir.exists(out$paths$modulePath) | grepl("scfm/modules$", out$paths$modulePath)))
  expect_false(any(grepl(file.path(mps[2], "scfm"), out$paths$modulePath, fixed = TRUE)))
})

test_that("useGit clones each GitHub module into the modulePath that holds it, else the first", {
  ## The git branch of setupModules() used `paths$modulePath` as one path:
  ## `dir.exists(localPath) && ...` failed with two. A clone (project repo with
  ## no commits yet) also went to modulePath/basename(modulePath)/repo.
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  mod <- "PredictiveEcology/Biomass_speciesFactorial@main"
  repo <- "Biomass_speciesFactorial"
  sig <- gert::git_signature("test", "test@example.com")
  mkProj <- function(commit) {
    proj <- normPath(withr::local_tempdir(.local_envir = parent.frame()))
    mps <- file.path(proj, c("mp1", "mp2"))
    for (mp in mps) dir.create(mp)
    gert::git_init(proj)
    if (commit) {
      writeLines("x", file.path(proj, "README.md"))
      gert::git_add("README.md", repo = proj)
      gert::git_commit("init", author = sig, repo = proj)
    }
    list(proj = proj, mps = mps)
  }
  runSetupModules <- function(p) {
    withr::local_dir(p$proj)
    suppressMessages(
      setupModules(name = basename(p$proj), paths = list(modulePath = p$mps, projectPath = p$proj),
                   modules = mod, inProject = TRUE, useGit = TRUE,
                   updateRprofile = FALSE, verbose = -1))
  }

  ## clone (no commits yet) and submodule (committed): both go to the first path
  for (commit in c(FALSE, TRUE)) {
    p <- mkProj(commit)
    pkgs <- runSetupModules(p)
    expect_true(file.exists(file.path(p$mps[1], repo, ".git")))  # a dir, or a file for a submodule
    expect_false(dir.exists(file.path(p$mps[1], "mp1")))
    expect_length(list.files(p$mps[2]), 0L)
    expect_true("data.table" %in% pkgs[[mod]])
  }

  ## already cloned in the second path: used there, not cloned again
  p <- mkProj(TRUE)
  gert::git_clone(paste0("https://github.com/PredictiveEcology/", repo),
                  path = file.path(p$mps[2], repo), verbose = FALSE)
  pkgs <- runSetupModules(p)
  expect_length(list.files(p$mps[1]), 0L)
  expect_true("data.table" %in% pkgs[[mod]])
})
