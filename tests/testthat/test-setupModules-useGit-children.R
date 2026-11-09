## With useGit, setupModules() added each listed module as a git submodule but never
## read a parent's `childModules`, so a parent (e.g. fireSense) arrived without its
## children and simInit() stopped on the missing module files. Only the useGit = FALSE
## path (getModule()) fetched them. Offline: local bare repositories stand in for GitHub.

## A bare repository <td>/<repo>.git with one module, <repo>/<repo>.R, on branch `br`.
mkBareModule <- function(td, repo, br, childModules = character(), reqdPkgs = '"fs"') {
  bare <- file.path(td, paste0(repo, ".git"))
  gert::git_init(bare, bare = TRUE)
  writeLines(paste0("ref: refs/heads/", br), file.path(bare, "HEAD"))
  seed <- file.path(td, paste0(repo, "-seed"))
  dir.create(seed)
  gert::git_init(seed)
  writeLines(paste0("ref: refs/heads/", br), file.path(seed, ".git", "HEAD"))
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', repo),
      sprintf('  version = list(%s = "1.0.0"),', repo),
      sprintf("  childModules = %s,", paste(deparse(childModules), collapse = "")),
      sprintf("  reqdPkgs = list(%s)", reqdPkgs),
      "))"),
    file.path(seed, paste0(repo, ".R")))
  sig <- gert::git_signature("test", "test@example.com")
  gert::git_add(paste0(repo, ".R"), repo = seed)
  gert::git_commit("init", author = sig, repo = seed)
  gert::git_remote_add(url = bare, name = "origin", repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", br), repo = seed, verbose = FALSE)
  bare
}

test_that("useGit: a parent's children (and theirs) are added as submodules, on the parent's branch", {
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  mkBareModule(td, "par", "dev", childModules = "kidB")
  mkBareModule(td, "kidB", "dev", childModules = "kidC", reqdPkgs = '"data.table"')
  mkBareModule(td, "kidC", "dev")
  local_mocked_bindings(.gh_url = function(acct, repo) file.path(td, paste0(repo, ".git")))

  proj <- file.path(td, "proj")
  mp <- file.path(proj, "modules")
  dir.create(mp, recursive = TRUE)
  gert::git_init(proj)
  writeLines("x", file.path(proj, "README.md"))
  gert::git_add("README.md", repo = proj)
  gert::git_commit("init", author = gert::git_signature("test", "test@example.com"), repo = proj)
  withr::local_dir(proj)

  pkgs <- suppressMessages(
    setupModules(name = "proj", paths = list(modulePath = mp, projectPath = proj),
                 modules = "Acct/par@dev", inProject = TRUE, useGit = TRUE,
                 updateRprofile = FALSE, verbose = -1))

  for (mod in c("par", "kidB", "kidC")) {
    expect_true(file.exists(file.path(mp, mod, paste0(mod, ".R"))), info = mod)
    expect_identical(gert::git_branch(repo = file.path(mp, mod)), "dev", info = mod)
  }
  expect_setequal(gert::git_submodule_list(repo = proj)$path,
                  file.path("modules", c("par", "kidB", "kidC")))
  expect_true("data.table" %in% pkgs[["Acct/par@dev"]])
})

test_that("useGit: each module is named as it is cloned", {
  ## gert prints only the refs it fetched, so nine children went by with no module names
  skip_if_not_installed("gert")
  withr::local_options(Require.updateRprofile = NULL)
  td <- normPath(withr::local_tempdir())
  mkBareModule(td, "par", "dev", childModules = c("kidB", "kidC"))
  mkBareModule(td, "kidB", "dev")
  mkBareModule(td, "kidC", "dev")
  local_mocked_bindings(.gh_url = function(acct, repo) file.path(td, paste0(repo, ".git")))
  proj <- file.path(td, "proj")
  mp <- file.path(proj, "modules")
  dir.create(mp, recursive = TRUE)
  gert::git_init(proj)
  withr::local_dir(proj)

  msgs <- character()
  withCallingHandlers(
    setupModules(name = "proj", paths = list(modulePath = mp, projectPath = proj),
                 modules = "Acct/par@dev", inProject = TRUE, useGit = TRUE,
                 updateRprofile = FALSE, verbose = 1),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "Acct/par@dev (1 of 1)", fixed = TRUE)
  expect_match(msgs, "Acct/kidB@dev (1 of 2)", fixed = TRUE)
  expect_match(msgs, "Acct/kidC@dev (2 of 2)", fixed = TRUE)
})
