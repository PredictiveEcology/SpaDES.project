## A module spec's "(HEAD)" and "(>= x.y.z)", as Require reads them for packages.
##   no git: "@main (HEAD)" stopped with "missing value where TRUE/FALSE needed" (HEAD was compared as a
##           version); "(HEAD)" with no branch downloaded nothing; "(>= x)" replaced a local copy that
##           might hold edits, without `overwrite`.
##   git:    an existing clone was pulled on every run with the failure hidden, and a version spec or
##           "(HEAD)" was never looked at.
## The downloads are mocked; the git tests use local bare repositories.

## a module "name" at `version` in `mp`; as written by downloadGHRepoOuter() would
.fakeDownload <- function(version = "2.0.0") {
  calls <- new.env()
  calls$n <- 0L
  fun <- function(modToDL, verbose, overwrite, modulePath) {
    calls$n <- calls$n + 1L
    calls$spec <- modToDL
    mkVersionedModule(modulePath, Require::extractPkgName(modToDL), version)
  }
  list(fun = fun, calls = calls)
}

test_that("no git: a local copy below `(>= x)` is kept, with a message, unless overwrite", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA", "1.8.0")
  dl <- .fakeDownload()
  local_mocked_bindings(downloadGHRepoOuter = dl$fun)
  mess <- capture_messages(out <- getModule("Acct/modA@main (>= 1.9.0)", modulePath = mp, verbose = 1))
  expect_identical(dl$calls$n, 0L)
  expect_identical(out$failed, "Acct/modA@main (>= 1.9.0)")
  expect_true(any(grepl("overwrite = TRUE", mess)))
  expect_true(any(grepl("modA", mess)))

  out <- getModule("Acct/modA@main (>= 1.9.0)", modulePath = mp, overwrite = TRUE, verbose = -1)
  expect_identical(dl$calls$n, 1L)
  expect_identical(out$success, "Acct/modA@main (>= 1.9.0)")
  expect_identical(as.character(metadataInModules("modA", "version", mp)), "2.0.0")
})

test_that("no git: `(>= x)` that the local copy meets downloads nothing", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA", "1.9.0")
  dl <- .fakeDownload()
  local_mocked_bindings(downloadGHRepoOuter = dl$fun)
  out <- getModule("Acct/modA@main (>= 1.9.0)", modulePath = mp, verbose = -1)
  expect_identical(dl$calls$n, 0L)
  expect_identical(out$success, "Acct/modA@main (>= 1.9.0)")
})

test_that("no git: `(HEAD)` on an absent module downloads it, with or without a branch", {
  for (spec in c("Acct/modA@main (HEAD)", "Acct/modA (HEAD)", "Acct/modA (>= 1.9.0)")) {
    mp <- withr::local_tempdir()
    dl <- .fakeDownload()
    local_mocked_bindings(downloadGHRepoOuter = dl$fun)
    out <- getModule(spec, modulePath = mp, verbose = -1)
    expect_identical(dl$calls$n, 1L, info = spec)
    expect_true(grepl("@", dl$calls$spec), info = spec)   # a branch (HEAD) is named for the download
    expect_length(out$failed, 0L)
    expect_true(dir.exists(file.path(mp, "modA")), info = spec)
  }
})

test_that("no git: `(HEAD)` on a present module is kept, with a message, unless overwrite", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA", "1.0.0")
  dl <- .fakeDownload()
  local_mocked_bindings(downloadGHRepoOuter = dl$fun)
  mess <- capture_messages(out <- getModule("Acct/modA@main (HEAD)", modulePath = mp, verbose = 1))
  expect_identical(dl$calls$n, 0L)
  expect_identical(out$success, "Acct/modA@main (HEAD)")
  expect_true(any(grepl("overwrite = TRUE", mess)))
  expect_identical(as.character(metadataInModules("modA", "version", mp)), "1.0.0")

  out <- getModule("Acct/modA@main (HEAD)", modulePath = mp, overwrite = TRUE, verbose = -1)
  expect_identical(dl$calls$n, 1L)
  expect_identical(as.character(metadataInModules("modA", "version", mp)), "2.0.0")
})

test_that("no git: a module without a spec is left alone", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "modA", "1.0.0")
  dl <- .fakeDownload()
  local_mocked_bindings(downloadGHRepoOuter = dl$fun)
  out <- getModule("Acct/modA@main", modulePath = mp, verbose = -1)
  expect_identical(dl$calls$n, 0L)
  expect_identical(out$success, "Acct/modA@main")
})

test_that("no git: `@main (HEAD)` downloads the branch from GitHub", {
  skip_on_cran()
  skip_if_offline()
  mp <- withr::local_tempdir()
  out <- getModule("PredictiveEcology/Biomass_borealDataPrep@main (HEAD)", modulePath = mp, verbose = -1)
  expect_length(out$failed, 0L)
  expect_true(file.exists(file.path(mp, "Biomass_borealDataPrep", "Biomass_borealDataPrep.R")))
  out <- getModule("PredictiveEcology/Biomass_borealDataPrep (HEAD)", modulePath = mp, overwrite = TRUE,
                   verbose = -1)
  expect_length(out$failed, 0L)
})

## ---- git: an existing clone ----

## the module "mod" on branch dev, as a submodule of a new project; returns a function that runs
## setupModules() on it again, and the paths used
.gitModuleProject <- function(td, envir = parent.frame()) {
  mkBareModule(td, "mod", "dev")
  local_mocked_bindings(.gh_url = function(acct, repo) file.path(td, paste0(repo, ".git")),
                        .env = envir)
  proj <- file.path(td, "proj")
  mp <- file.path(proj, "modules")
  dir.create(mp, recursive = TRUE)
  gert::git_init(proj)
  writeLines("x", file.path(proj, "README.md"))
  gert::git_add("README.md", repo = proj)
  gert::git_commit("init", author = gert::git_signature("test", "test@example.com"), repo = proj)
  withr::local_dir(proj, .local_envir = envir)
  list(run = function(spec, quiet = TRUE) {
         run <- function() setupModules(name = "proj", paths = list(modulePath = mp, projectPath = proj),
                                        modules = spec, inProject = TRUE, useGit = TRUE,
                                        updateRprofile = FALSE, verbose = -1)
         if (quiet) suppressMessages(run()) else run()
       },
       modFile = file.path(mp, "mod", "mod.R"), mp = mp)
}

## a new commit on the remote's dev branch; `version` replaces the module's 1.0.0
.advanceRemote <- function(td, version) {
  seed <- file.path(td, "mod-seed")
  f <- file.path(seed, "mod.R")
  writeLines(sub('= "[0-9.]+"\\)', paste0('= "', version, '")'), readLines(f)), f)
  gert::git_add("mod.R", repo = seed)
  gert::git_commit(paste("version", version),
                   author = gert::git_signature("test", "test@example.com"), repo = seed)
  gert::git_push("origin", refspec = "refs/heads/dev", repo = seed, verbose = FALSE)
}
.localVersion <- function(f) sub('.*= "([0-9.]+)".*', "\\1", grep("version", readLines(f), value = TRUE))

test_that("git: an existing clone on the requested branch is not pulled without a version spec", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  expect_no_message(p$run("Acct/mod@dev", quiet = FALSE), message = "not pulled")   # a fresh clone
  .advanceRemote(td, "1.1.0")
  expect_message(p$run("Acct/mod@dev", quiet = FALSE),
                 "not pulled: Acct/mod@dev.*Add \\(HEAD\\) to a module to get the previous behaviour")
  expect_identical(.localVersion(p$modFile), "1.0.0")
  expect_no_message(p$run("Acct/mod@dev (HEAD)", quiet = FALSE), message = "not pulled")
})

test_that("git: `(HEAD)` pulls an existing clone", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  p$run("Acct/mod@dev")
  .advanceRemote(td, "1.1.0")
  p$run("Acct/mod@dev (HEAD)")
  expect_identical(.localVersion(p$modFile), "1.1.0")
  .advanceRemote(td, "1.2.0")
  p$run("Acct/mod (HEAD)")   # no branch: the clone's own
  expect_identical(.localVersion(p$modFile), "1.2.0")
})

test_that("git: `(HEAD)` with a local edit that the pull would overwrite stops, naming the module", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  p$run("Acct/mod@dev")
  writeLines(sub("1.0.0", "9.9.9", readLines(p$modFile), fixed = TRUE), p$modFile)
  .advanceRemote(td, "1.1.0")
  expect_error(p$run("Acct/mod@dev (HEAD)"), "mod")
  expect_identical(.localVersion(p$modFile), "9.9.9")   # the edit is still there
})

test_that("git: `(>= x)` leaves a clone that meets it, and pulls one that does not", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  p$run("Acct/mod@dev")
  .advanceRemote(td, "1.1.0")
  p$run("Acct/mod@dev (>= 1.0.0)")
  expect_identical(.localVersion(p$modFile), "1.0.0")
  p$run("Acct/mod@dev (>= 1.1.0)")
  expect_identical(.localVersion(p$modFile), "1.1.0")
})

test_that("git: `(>= x)` still unmet after the pull stops, naming the module and both versions", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  p$run("Acct/mod@dev")
  .advanceRemote(td, "1.1.0")
  expect_error(p$run("Acct/mod@dev (>= 5.0.0)"), "mod.*1\\.1\\.0.*5\\.0\\.0")
})

test_that("git: another requested branch is checked out and pulled", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  p <- .gitModuleProject(td)
  p$run("Acct/mod@dev")
  seed <- file.path(td, "mod-seed")
  gert::git_branch_create("alt", repo = seed)
  .advanceRemoteBranch <- function(version) {
    writeLines(sub('= "[0-9.]+"\\)', paste0('= "', version, '")'), readLines(file.path(seed, "mod.R"))),
               file.path(seed, "mod.R"))
    gert::git_add("mod.R", repo = seed)
    gert::git_commit(version, author = gert::git_signature("test", "test@example.com"), repo = seed)
    gert::git_push("origin", refspec = "refs/heads/alt", repo = seed, verbose = FALSE)
  }
  .advanceRemoteBranch("3.0.0")
  p$run("Acct/mod@alt")
  expect_identical(gert::git_branch(repo = dirname(p$modFile)), "alt")
  expect_identical(.localVersion(p$modFile), "3.0.0")
})
