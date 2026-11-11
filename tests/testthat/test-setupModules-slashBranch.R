## A module spec could not name a branch with a "/" (`Acct/repo@feature/x`): the first
## segment after "@" was the branch, the rest a subfolder. Nor could it pin a commit: the
## clone tried to check the SHA out as a branch. Offline: local bare repositories stand
## in for GitHub.

## A bare module repo `par` on `main`, plus branch `br` with one more commit (feat.txt).
## Returns the new commit's SHA.
mkSlashRepo <- function(td, br) {
  mkBareModule(td, "par", "main")
  seed <- file.path(td, "par-seed")
  gert::git_branch_create(br, repo = seed)
  gert::git_branch_checkout(br, repo = seed)
  writeLines("x", file.path(seed, "feat.txt"))
  gert::git_add("feat.txt", repo = seed)
  sha <- gert::git_commit("feat", author = gert::git_signature("t", "t@e.c"), repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", br), repo = seed, verbose = FALSE)
  sha
}

## Needs Require whose splitGitRepo() decodes "%2F" (Require PR).
skipIfNoDecode <- function()
  skip_if(!identical(Require:::splitGitRepo("A/r@a%2Fb")$br[[1]], "a/b"),
          "Require without %2F decoding in splitGitRepo()")

## The remote branch lists are kept for the session; start each test without them.
localFreshRefs <- function(env = parent.frame()) {
  old <- .pkgEnv$remoteRefs
  .pkgEnv$remoteRefs <- NULL
  withr::defer(.pkgEnv$remoteRefs <- old, envir = env)
}

setupSlash <- function(td, spec) {
  proj <- file.path(td, "proj")
  mp <- file.path(proj, "modules")
  dir.create(mp, recursive = TRUE)
  gert::git_init(proj)
  withr::local_dir(proj)
  suppressMessages(
    setupModules(name = "proj", paths = list(modulePath = mp, projectPath = proj),
                 modules = spec, inProject = TRUE, useGit = TRUE,
                 updateRprofile = FALSE, verbose = -1))
  file.path(mp, "par")
}

test_that("useGit: a branch with a '/' is cloned and checked out", {
  skip_if_not_installed("gert")
  skip_if(!nzchar(Sys.which("git")))
  skipIfNoDecode()
  localFreshRefs()
  td <- normPath(withr::local_tempdir())
  mkSlashRepo(td, "feature/x")
  local_mocked_bindings(.gh_url = function(acct, repo) file.path(td, paste0(repo, ".git")))
  mod <- setupSlash(td, "Acct/par@feature/x")
  expect_identical(gert::git_branch(repo = mod), "feature/x")
  expect_true(file.exists(file.path(mod, "feat.txt")))
})

test_that("useGit: a commit SHA is checked out (detached)", {
  skip_if_not_installed("gert")
  skip_if(!nzchar(Sys.which("git")))
  localFreshRefs()
  td <- normPath(withr::local_tempdir())
  sha <- mkSlashRepo(td, "side")
  local_mocked_bindings(.gh_url = function(acct, repo) file.path(td, paste0(repo, ".git")))
  mod <- setupSlash(td, paste0("Acct/par@", substr(sha, 1, 7)))
  expect_identical(gert::git_info(repo = mod)$commit, sha)
  expect_true(file.exists(file.path(mod, "feat.txt")))
})

test_that(".escapeSlashBranches finds the branch among the segments after '@'", {
  skip_if(!nzchar(Sys.which("git")))
  withr::local_options(Require.offlineMode = FALSE)
  localFreshRefs()
  local_mocked_bindings(.remoteRefs = function(acct, repo)
    list(heads = c("development", "feature/x", "main"), tags = "v1/beta"))
  esc <- function(x) .escapeSlashBranches(x)
  expect_identical(esc("A/r@feature/x"), "A/r@feature%2Fx")
  expect_identical(esc("A/r@feature/x/modules/m"), "A/r@feature%2Fx/modules/m")
  expect_identical(esc("A/r@feature/x (>= 1.0)"), "A/r@feature%2Fx (>= 1.0)")
  expect_identical(esc("A/r@v1/beta/m"), "A/r@v1%2Fbeta/m")
  ## the branch is the first segment: as before
  expect_identical(esc("A/r@development/modules/m"), "A/r@development/modules/m")
  ## no "/" after "@", already escaped, or a commit then a subfolder: untouched
  expect_identical(esc(c("A/r@main", "A/r@feature%2Fx/m", "A/r@abc1234/m")),
                   c("A/r@main", "A/r@feature%2Fx/m", "A/r@abc1234/m"))
  expect_error(esc("A/r@feature/y"), "branches starting with 'feature': feature/x")
})

test_that(".escapeSlashBranches leaves specs alone offline or when the remote can't be listed", {
  localFreshRefs()
  local_mocked_bindings(.remoteRefs = function(acct, repo) stop("must not be called"))
  withr::local_options(Require.offlineMode = TRUE)
  expect_identical(.escapeSlashBranches("A/r@feature/x"), "A/r@feature/x")
  withr::local_options(Require.offlineMode = FALSE)
  local_mocked_bindings(.remoteRefs = function(acct, repo) NULL)
  expect_identical(.escapeSlashBranches("A/r@feature/x"), "A/r@feature/x")
})

test_that("a child inherits a parent's '/' branch escaped", {
  skipIfNoDecode()
  expect_identical(.childModuleSpec("kid", "Acct/par@feature%2Fx"), "Acct/kid@feature%2Fx")
})

test_that(".escapeSlashBranches lists a remote once per session, and again on a miss", {
  skip_if(!nzchar(Sys.which("git")))
  withr::local_options(Require.offlineMode = FALSE)
  localFreshRefs()
  calls <- 0L
  heads <- c("main", "feature/x")
  local_mocked_bindings(.remoteRefs = function(acct, repo) {
    calls <<- calls + 1L
    list(heads = heads, tags = character())
  })
  .escapeSlashBranches("A/r@feature/x")
  .escapeSlashBranches(c("A/r@feature/x/m", "A/r@main/m"))
  expect_identical(calls, 1L)
  heads <- c(heads, "feature/new") # pushed since the list was taken
  expect_identical(.escapeSlashBranches("A/r@feature/new"), "A/r@feature%2Fnew")
  expect_identical(calls, 2L)
})

test_that("file specs: the file name is never part of the branch", {
  skip_if(!nzchar(Sys.which("git")))
  skipIfNoDecode()
  withr::local_options(Require.offlineMode = FALSE)
  localFreshRefs()
  local_mocked_bindings(.remoteRefs = function(acct, repo)
    list(heads = c("development", "feature/x"), tags = character()))
  expect_identical(.escapeSlashBranches("A/r@feature/x/inst/o.R", isFile = TRUE),
                   "A/r@feature%2Fx/inst/o.R")
  expect_identical(.escapeSlashBranches("A/r@development/inst/o.R", isFile = TRUE),
                   "A/r@development/inst/o.R")
  ## `@branch/file.R`: nothing to look up
  local_mocked_bindings(.remoteRefs = function(acct, repo) stop("must not be called"))
  expect_identical(.escapeSlashBranches("A/r@feature/o.R", isFile = TRUE), "A/r@feature/o.R")

  ## getGithubFile(): the file path is relative to the branch, not to its first segment
  localFreshRefs()
  got <- NULL
  local_mocked_bindings(
    .remoteRefs = function(acct, repo) list(heads = "feature/x", tags = character()),
    downloadFile = function(gitRepo, file, ...) {
      got <<- list(gitRepo = gitRepo, file = file); NULL
    })
  getGithubFile("A/r@feature/x/inst/o.R", destDir = withr::local_tempdir(), verbose = -1)
  expect_identical(got, list(gitRepo = "A/r@feature%2Fx", file = "inst/o.R"))
})
