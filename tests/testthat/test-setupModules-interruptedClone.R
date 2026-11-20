## An interrupted or failed module clone (useGit = TRUE, the project a git repository) left
## setupModules() stuck: the next run took the half-made clone as a real one and failed to pull
## ("missing value where TRUE/FALSE needed"); a failed clone went on to check out the branch in
## an empty folder, i.e. in the project's own repository ("No local or remote branch ... found");
## and the cleanup left registrations behind ("already exists", "attempt to reinitialize"), so
## each module took several reruns and prompts. Offline: local bare repositories stand in for GitHub.

local_interruptedProject <- function(env = parent.frame()) {
  skip_if_not_installed("gert")
  skip_if_not(nzchar(Sys.which("git")))
  withr::local_options(Require.updateRprofile = NULL, .local_envir = env)
  td <- normPath(withr::local_tempdir(.local_envir = env))
  mkBareModule(td, "mod", "dev")
  proj <- file.path(td, "proj")
  mp <- file.path(proj, "modules")
  dir.create(mp, recursive = TRUE)
  gert::git_init(proj)
  writeLines("x", file.path(proj, "README.md"))
  gert::git_add("README.md", repo = proj)
  gert::git_commit("init", author = gert::git_signature("test", "test@example.com"), repo = proj)
  withr::local_dir(proj, .local_envir = env)
  list(td = td, proj = proj, mp = mp, branch = gert::git_branch(repo = proj))
}

## What a clone interrupted part way leaves: the submodule registered (.gitmodules, .git/config),
## its git directory created with no commits, and the module folder holding only its `.git` file.
mkInterruptedSubmodule <- function(proj, rel, url) {
  writeLines(c(sprintf('[submodule "%s"]', rel), sprintf("\tpath = %s", rel),
               sprintf("\turl = %s", url)), file.path(proj, ".gitmodules"))
  gert::git_config_set(paste0("submodule.", rel, ".url"), url, repo = proj)
  dir.create(file.path(proj, ".git", "modules", dirname(rel)), recursive = TRUE)
  system2("git", c("init", "-q", "--separate-git-dir",
                   shQuote(file.path(proj, ".git", "modules", rel)), shQuote(file.path(proj, rel))))
}

runSetup <- function(p) {
  suppressMessages(
    setupModules(name = "proj", paths = list(modulePath = p$mp, projectPath = p$proj),
                 modules = "Acct/mod@dev", inProject = TRUE, useGit = TRUE,
                 updateRprofile = FALSE, verbose = -1))
}

expectCloned <- function(p) {
  expect_true(file.exists(file.path(p$mp, "mod", "mod.R")))
  expect_identical(gert::git_branch(repo = file.path(p$mp, "mod")), "dev")
  expect_identical(gert::git_branch(repo = p$proj), p$branch) # the project's own branch untouched
  expect_identical(gert::git_submodule_list(repo = p$proj)$path, "modules/mod")
}

test_that("useGit: a module folder from an interrupted clone is cloned again, in one run, with no prompt", {
  p <- local_interruptedProject()
  url <- file.path(p$td, "mod.git")
  local_mocked_bindings(.gh_url = function(acct, repo) url,
                        readLineYoNDelFolder = function() stop("prompted"))
  mkInterruptedSubmodule(p$proj, "modules/mod", url)

  runSetup(p)
  expectCloned(p)
})

test_that("useGit: an interrupted clone whose folder was deleted by hand is cloned again, in one run", {
  p <- local_interruptedProject()
  url <- file.path(p$td, "mod.git")
  local_mocked_bindings(.gh_url = function(acct, repo) url,
                        readLineYoNDelFolder = function() stop("prompted"))
  mkInterruptedSubmodule(p$proj, "modules/mod", url)
  unlink(file.path(p$mp, "mod"), recursive = TRUE)

  runSetup(p)
  expectCloned(p)
})

test_that("useGit: a failed clone stops, naming the module, and the next run clones it", {
  p <- local_interruptedProject()
  good <- file.path(p$td, "mod.git")
  url <- file.path(p$td, "unreachable.git")
  local_mocked_bindings(.gh_url = function(acct, repo) url,
                        readLineYoNDelFolder = function() stop("prompted"))

  expect_error(runSetup(p), "Acct/mod")
  expect_identical(gert::git_branch(repo = p$proj), p$branch)

  url <- good
  runSetup(p)
  expectCloned(p)
})
