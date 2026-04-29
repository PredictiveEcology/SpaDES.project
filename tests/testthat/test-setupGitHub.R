# Tests for the two recent setupGitHub-related fixes:
#
# 1. .shouldOfferClone() should return FALSE when the project path is
#    already a git working copy, even without an .Rproj file -- so
#    setupGitHub() doesn't prompt "would you like to clone it now to
#    <pp>?" and then stop with "Can't proceed:" when the user (correctly)
#    answers "no".
#
# 2. setUpstreamWithTry() should switch to a branch that lives on a
#    non-default remote (e.g. a fork) by walking every configured remote
#    after a default `git_fetch()` doesn't bring it in.

testthat::test_that(".shouldOfferClone is FALSE for an existing git repo (no .Rproj)", {
  testthat::skip_if_not_installed("gert")
  testthat::skip_if_not_installed("rprojroot")

  td <- tempfile("git-only"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  gert::git_init(td)
  testthat::expect_true(file.exists(file.path(td, ".git")))
  testthat::expect_false(file.exists(file.path(td, paste0(basename(td), ".Rproj"))))

  testthat::expect_false(SpaDES.project:::.shouldOfferClone(td))
  testthat::expect_true(rprojroot::is_git_root$testfun[[1]](td))
})

testthat::test_that(".shouldOfferClone is FALSE for an Rproj path with no .git", {
  testthat::skip_if_not_installed("rprojroot")

  td <- tempfile("rproj-only"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  writeLines("Version: 1.0\n", file.path(td, paste0(basename(td), ".Rproj")))

  testthat::expect_false(SpaDES.project:::.shouldOfferClone(td))
  testthat::expect_true(rprojroot::is_rstudio_project$testfun[[1]](td))
})

testthat::test_that(".shouldOfferClone is TRUE for an empty directory", {
  td <- tempfile("empty"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  testthat::expect_true(SpaDES.project:::.shouldOfferClone(td))
})

testthat::test_that(".shouldOfferClone is FALSE when both .git and .Rproj exist", {
  testthat::skip_if_not_installed("gert")
  td <- tempfile("both"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  gert::git_init(td)
  writeLines("Version: 1.0\n", file.path(td, paste0(basename(td), ".Rproj")))
  testthat::expect_false(SpaDES.project:::.shouldOfferClone(td))
})

testthat::test_that("setUpstreamWithTry switches to a branch only available on a non-default remote", {
  testthat::skip_if_not_installed("gert")
  testthat::skip_if(.Platform$OS.type != "unix", "git plumbing test relies on POSIX paths")

  td <- tempfile("forkpath"); dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # 1. Build an "origin" bare repo that only has `main`, and a "fork" bare
  #    repo that has `main` plus a `feature-branch` we want to check out.
  origin_bare <- file.path(td, "origin.git")
  fork_bare   <- file.path(td, "fork.git")
  gert::git_init(origin_bare, bare = TRUE)
  gert::git_init(fork_bare,   bare = TRUE)

  # 2. Seed both bare repos by pushing from a throwaway working copy.
  seed <- file.path(td, "seed"); dir.create(seed)
  gert::git_init(seed)
  gert::git_config_set("user.email", "test@example.com", repo = seed)
  gert::git_config_set("user.name",  "test",             repo = seed)
  writeLines("hi", file.path(seed, "README.md"))
  gert::git_add("README.md", repo = seed)
  gert::git_commit("init", repo = seed)
  default_br <- gert::git_branch(repo = seed)   # whatever gert chose ("main" or "master")
  gert::git_remote_add(url = origin_bare, name = "origin", repo = seed)
  gert::git_remote_add(url = fork_bare,   name = "fork",   repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", default_br), repo = seed)
  gert::git_push("fork",   refspec = paste0("refs/heads/", default_br), repo = seed)

  # Add the fork-only branch and push it ONLY to the fork.
  gert::git_branch_create("feature-branch", repo = seed)
  gert::git_branch_checkout("feature-branch", repo = seed)
  writeLines("feature\n", file.path(seed, "feature.txt"))
  gert::git_add("feature.txt", repo = seed)
  gert::git_commit("feat", repo = seed)
  gert::git_push("fork", refspec = "refs/heads/feature-branch", repo = seed)

  # 3. Clone from origin (the default remote) -- this clone does NOT see
  #    `feature-branch` until we add the fork as a remote.
  work <- file.path(td, "work")
  gert::git_clone(origin_bare, path = work)
  gert::git_remote_add(url = fork_bare, name = "fork", repo = work)

  # 4. Switch into the working copy and call the real setUpstreamWithTry.
  origDir <- getwd(); on.exit(setwd(origDir), add = TRUE)
  setwd(work)
  curBr <- gert::git_branch()                # default branch
  split <- list(br = "feature-branch", repo = "work")
  out <- SpaDES.project:::setUpstreamWithTry(split, curBr = curBr, verbose = -1)

  # The walk-every-remote fallback should have fetched the fork and
  # checked out feature-branch.  HEAD now points at it.
  testthat::expect_identical(gert::git_branch(), "feature-branch")
  testthat::expect_identical(out$br, "feature-branch")
})

testthat::test_that("setUpstreamWithTry adds a fork remote on demand when split has $acct/$repo", {
  testthat::skip_if_not_installed("gert")
  testthat::skip_if(.Platform$OS.type != "unix")

  td <- tempfile("forkadd"); dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Set up TWO bare repos and seed each: origin has only `main`, fork has
  # `main` + `feature-branch`.  The working copy will be cloned from
  # origin and the fork will NOT yet be a remote -- setUpstreamWithTry
  # has to add it itself using $acct + $repo.
  origin_bare <- file.path(td, "origin.git")
  fork_bare   <- file.path(td, "fork.git")
  gert::git_init(origin_bare, bare = TRUE)
  gert::git_init(fork_bare,   bare = TRUE)

  seed <- file.path(td, "seed"); dir.create(seed)
  gert::git_init(seed)
  gert::git_config_set("user.email", "t@e", repo = seed)
  gert::git_config_set("user.name",  "t",   repo = seed)
  writeLines("hi", file.path(seed, "README.md"))
  gert::git_add("README.md", repo = seed)
  gert::git_commit("init", repo = seed)
  default_br <- gert::git_branch(repo = seed)
  gert::git_remote_add(url = origin_bare, name = "origin", repo = seed)
  gert::git_remote_add(url = fork_bare,   name = "fork",   repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", default_br), repo = seed)
  gert::git_push("fork",   refspec = paste0("refs/heads/", default_br), repo = seed)
  gert::git_branch_create("feature-branch", repo = seed)
  gert::git_branch_checkout("feature-branch", repo = seed)
  writeLines("f\n", file.path(seed, "feature.txt"))
  gert::git_add("feature.txt", repo = seed)
  gert::git_commit("feat", repo = seed)
  gert::git_push("fork", refspec = "refs/heads/feature-branch", repo = seed)

  # Working copy: cloned from origin, fork NOT added.  setUpstreamWithTry
  # only knows the fork's location via split$acct / split$repo (which is
  # what splitGitRepo() puts there).  We point that to the bare path via
  # a stubbed acct -- in real use, paste0("https://github.com/", acct,
  # "/", repo) would be the fork URL.  For the test, we stub the URL
  # construction by mocking acct/repo to point at our local bare repo.
  work <- file.path(td, "work")
  gert::git_clone(origin_bare, path = work)

  # Stub the SpaDES.project::.gh_url helper so the "fork URL" the
  # production code asks gert to add is our local bare-repo path.
  # In real use, .gh_url("STUB_ACCT", "STUB_REPO") would return
  # "https://github.com/STUB_ACCT/STUB_REPO".
  testthat::local_mocked_bindings(
    .gh_url = function(acct, repo) fork_bare,
    .package = "SpaDES.project"
  )

  origDir <- getwd(); on.exit(setwd(origDir), add = TRUE)
  setwd(work)
  curBr <- gert::git_branch()
  split <- list(br = "feature-branch", acct = "STUB_ACCT", repo = "STUB_REPO")
  out <- SpaDES.project:::setUpstreamWithTry(split, curBr = curBr, verbose = -1)

  testthat::expect_identical(gert::git_branch(), "feature-branch")
  # The new remote was added under the acct name.
  remotes <- gert::git_remote_list()
  testthat::expect_true("STUB_ACCT" %in% remotes$name)
})

testthat::test_that("setUpstreamWithTry returns gracefully (no stop) when branch is missing everywhere", {
  testthat::skip_if_not_installed("gert")
  testthat::skip_if(.Platform$OS.type != "unix")

  td <- tempfile("missingbr"); dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  bare <- file.path(td, "origin.git")
  gert::git_init(bare, bare = TRUE)
  seed <- file.path(td, "seed"); dir.create(seed)
  gert::git_init(seed)
  gert::git_config_set("user.email", "t@e", repo = seed)
  gert::git_config_set("user.name",  "t",   repo = seed)
  writeLines("x", file.path(seed, "x.txt"))
  gert::git_add("x.txt", repo = seed)
  gert::git_commit("init", repo = seed)
  default_br <- gert::git_branch(repo = seed)
  gert::git_remote_add(url = bare, name = "origin", repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", default_br), repo = seed)

  work <- file.path(td, "work")
  gert::git_clone(bare, path = work)

  origDir <- getwd(); on.exit(setwd(origDir), add = TRUE)
  setwd(work)
  start_br <- gert::git_branch()
  split <- list(br = "does-not-exist-anywhere", repo = "work")
  # Must not stop().  Should fall through with a message and return the
  # split unchanged (or with the current branch).
  out <- testthat::expect_no_error(
    SpaDES.project:::setUpstreamWithTry(split, curBr = start_br, verbose = -1)
  )
  # No checkout happened, and the function did not abort.
  testthat::expect_identical(gert::git_branch(), start_br)
})
