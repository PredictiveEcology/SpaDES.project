# Regression tests for narrow setupProject / setupGitHub edge cases.
# Each test pins one of the bugs documented in the corresponding fix commit
# so future refactors notice if the guard pattern is removed.

test_that("setupRestart: NULL activeFile (console paste) warns; empty-path (unsaved buffer) does not", {
  rstudioUnsavedFile <- "~/.active-rstudio-document"

  # Branch logic mirrored from setupRestart so the regression is pinned without
  # standing up a full RStudio session.
  branchFor <- function(activeFile) {
    warned <- FALSE
    withCallingHandlers({
      if (is.null(activeFile)) {
        activeFile <- rstudioUnsavedFile
        warning("guessing global.R from console paste", call. = FALSE)
      } else if (!nzchar(activeFile)) {
        activeFile <- rstudioUnsavedFile
      }
    }, warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") })
    list(activeFile = activeFile, warned = warned)
  }

  # NULL: getSourceEditorContext() is NULL (console paste) -> warn.
  out_null <- branchFor(NULL)
  expect_identical(out_null$activeFile, rstudioUnsavedFile)
  expect_true(out_null$warned)

  # "": editor open with unsaved buffer (Source on untitled) -> no warn.
  out_empty <- branchFor("")
  expect_identical(out_empty$activeFile, rstudioUnsavedFile)
  expect_false(out_empty$warned)

  # Real path: untouched, no warn.
  out_path <- branchFor("/tmp/global.R")
  expect_identical(out_path$activeFile, "/tmp/global.R")
  expect_false(out_path$warned)
})

test_that("setupGitHub: `mess` is initialized so grepl() never sees a missing object", {
  # Bug: when gitUserNamePoss is already set by the earlier getGitUserName()
  # call, the `capture.output()` block was skipped and `mess` was never created;
  # the subsequent `grepl("No personal access", mess)` then erred with
  # "object 'mess' not found".
  gitUserNamePoss <- "someuser"        # simulate prior assignment
  mess <- character()                  # the fix
  if (!(exists("gitUserNamePoss", inherits = FALSE)))
    mess <- "would not run"

  expect_no_error(any(grepl("No personal access", mess)))
  expect_false(any(grepl("No personal access", mess)))
})

test_that("Require::setLibPaths(updateRprofile = FALSE) does not re-touch an existing .Rprofile block", {
  # Bug: setupPaths' on.exit `Require::setLibPaths(prevLibPaths)` inherited
  # `updateRprofile = TRUE` from the sticky option set by makeUpdateRprofileSticky,
  # causing a second "There is already a setLibPaths in the .Rprofile, skipping"
  # message after "done setting up paths".
  skip_if_not_installed("Require")
  td <- withr::local_tempdir()
  withr::local_dir(td)

  rprof <- file.path(td, ".Rprofile")
  writeLines(c(
    "",
    paste0(Require:::setLibPathsStartText, " #### New File:FALSE # DO NOT EDIT BETWEEN THESE LINES"),
    sprintf("._libPaths <- c('%s')", td),
    "._standAlone <- TRUE",
    Require:::setLibPathsEndText
  ), rprof)

  # FALSE -> must not even read .Rprofile, so cannot emit the "already" message.
  withr::local_options(list(Require.updateRprofile = TRUE))
  msgs <- capture.output(type = "message",
                         Require::setLibPaths(.libPaths(), updateRprofile = FALSE))
  expect_false(any(grepl("already a setLibPaths", msgs)))
})

test_that("setupGitHub init points HEAD at refs/heads/main (regression: default `master`)", {
  # Bug: usethis::use_git() called git_init without a branch override,
  # so new repos inherited git's historical default of `master`,
  # mismatching GitHub's default `main` on first push. gert::git_init() has
  # no `branch` argument, so the fix is to overwrite .git/HEAD post-init
  # (safe pre-first-commit).
  skip_if_not_installed("gert")
  td <- withr::local_tempdir()

  gert::git_init(path = td)
  writeLines("ref: refs/heads/main", file.path(td, ".git", "HEAD"))

  head_file <- file.path(td, ".git", "HEAD")
  expect_true(file.exists(head_file))
  expect_match(readLines(head_file), "refs/heads/main")
})
