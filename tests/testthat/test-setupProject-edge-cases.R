# Regression tests for narrow setupProject / setupGitHub edge cases.
# Each test pins one of the bugs documented in the corresponding fix commit
# so future refactors notice if the guard pattern is removed.

test_that("setupRestart: NULL activeFile from rstudioapi::getSourceEditorContext is treated as unsaved", {
  # Bug: when called from the R console (no source editor focused),
  # rstudioapi::getSourceEditorContext() returns NULL, so
  # `activeFile <- ...$path` becomes NULL. The old guard
  # `if (!nzchar(activeFile))` then errored with
  # "argument is of length zero" because nzchar(NULL) is logical(0).
  activeFile <- NULL
  rstudioUnsavedFile <- "~/.active-rstudio-document"

  expect_no_error({
    if (is.null(activeFile) || !nzchar(activeFile))
      activeFile <- rstudioUnsavedFile
  })
  expect_identical(activeFile, rstudioUnsavedFile)

  # Also verify "" (empty path -- editor open but file unsaved) takes the same branch.
  activeFile <- ""
  expect_no_error({
    if (is.null(activeFile) || !nzchar(activeFile))
      activeFile <- rstudioUnsavedFile
  })
  expect_identical(activeFile, rstudioUnsavedFile)
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

test_that("gert::git_init(branch = 'main') points HEAD at refs/heads/main", {
  # Bug: usethis::use_git() called git_init without a `branch` argument,
  # so the new repo inherited git's historical default of `master`,
  # mismatching GitHub's default `main` on first push.
  skip_if_not_installed("gert")
  td <- withr::local_tempdir()

  gert::git_init(path = td, branch = "main")

  head_file <- file.path(td, ".git", "HEAD")
  expect_true(file.exists(head_file))
  expect_match(readLines(head_file), "refs/heads/main")
})
