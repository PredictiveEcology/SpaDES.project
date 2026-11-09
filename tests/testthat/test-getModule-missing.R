## A module in a repository subfolder ("acct/repo@br/sub/mod") was reported as
## "downloaded" whenever the repository downloaded, because getModule() looked for
## <modulePath>/<repo>, not for the module (#182). Offline: a fake "GitHub" of local
## folders stands in for downloadGHRepoOuter().

## remote/<repo>/modules/<mod>/<mod>.R; the fake download copies the whole repository
localFakeRepo <- function(remote, repo, mods, envir = parent.frame()) {
  for (mod in mods) {
    d <- file.path(remote, repo, "modules", mod)
    dir.create(d, recursive = TRUE)
    writeLines(sprintf('defineModule(sim, list(name = "%s"))', mod), file.path(d, paste0(mod, ".R")))
  }
  local_mocked_bindings(
    downloadGHRepoOuter = function(modToDL, verbose, overwrite, modulePath)
      file.copy(file.path(remote, repo), modulePath, recursive = TRUE),
    .env = envir
  )
}

test_that("getModule keeps the modules it found and reports the ones not in the repository", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  localFakeRepo(remote, "repo", "modA")

  expect_warning(
    out <- getModule(c("Acct/repo@main/modules/modA", "Acct/repo@main/modules/notAModule"),
                     modulePath = mp, verbose = -1),
    "notAModule")
  expect_identical(out$success, "Acct/repo@main/modules/modA")
  expect_identical(out$failed, "Acct/repo@main/modules/notAModule")
  expect_true(file.exists(file.path(mp, "repo", "modules", "modA", "modA.R")))
})

test_that("a repository folder holding none of the listed modules is not left behind", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  localFakeRepo(remote, "repo", "modA")

  expect_warning(
    out <- getModule("Acct/repo@main/modules/notAModule", modulePath = mp, verbose = -1),
    "notAModule")
  expect_identical(out$failed, "Acct/repo@main/modules/notAModule")
  expect_identical(out$notInRepository, "Acct/repo@main/modules/notAModule")
  expect_false(dir.exists(file.path(mp, "repo")))
})

test_that("getModule reports a repository subfolder module that does exist", {
  remote <- withr::local_tempdir()
  mp <- withr::local_tempdir()
  localFakeRepo(remote, "repo", "modA")

  out <- getModule("Acct/repo@main/modules/modA", modulePath = mp, verbose = -1)
  expect_identical(out$success, "Acct/repo@main/modules/modA")
  expect_length(out$failed, 0L)
  expect_true(file.exists(file.path(mp, "repo", "modules", "modA", "modA.R")))
})

test_that("a module whose repository did not download stays in `failed`, without an error", {
  ## setupProject() falls back to git for these, e.g. for a private repository
  mp <- withr::local_tempdir()
  local_mocked_bindings(downloadGHRepoOuter = function(...) NULL)
  out <- getModule("Acct/repo@main/modules/modA", modulePath = mp, verbose = -1)
  expect_length(out$success, 0L)
  expect_identical(out$failed, "Acct/repo@main/modules/modA")
  expect_length(out$notInRepository, 0L)
})

test_that("setupProject stops, without a git clone, when no listed module is in the repository", {
  ## getModule() removes the repository folder; these must not go to the git fallback
  skip_on_cran()
  skip_if_offline()
  setupTest()
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  local_mocked_bindings(isProjectGitRepo = function(...) stop("git fallback reached"))
  expect_error(suppressWarnings(suppressMessages(
    setupProject(modules = file.path(scfmParent, "modules", "notAModule"),
                 paths = list(modulePath = mp, projectPath = file.path(root, "proj"),
                              packagePath = .libPaths()[1L]),
                 packages = NULL, useGit = FALSE, updateRprofile = FALSE))),
    "could not be found: .*notAModule")
  expect_false(dir.exists(file.path(mp, "scfm")))
})

test_that("setupProject puts the found nested modules in place, then stops naming the missing one", {
  skip_on_cran()
  skip_if_offline()
  setupTest()
  root <- normPath(withr::local_tempdir())
  mp <- file.path(root, "modules")
  expect_error(suppressWarnings(suppressMessages(
    setupProject(modules = file.path(scfmParent, "modules", c("scfmRegime", "notAModule")),
                 paths = list(modulePath = mp, projectPath = file.path(root, "proj"),
                              packagePath = .libPaths()[1L]),
                 packages = NULL, useGit = FALSE, updateRprofile = FALSE))),
    "notAModule")
  expect_true(file.exists(file.path(mp, "scfmRegime", "scfmRegime.R")))
  expect_false(dir.exists(file.path(mp, "notAModule")))
  expect_false(dir.exists(file.path(mp, "scfm")))
})
