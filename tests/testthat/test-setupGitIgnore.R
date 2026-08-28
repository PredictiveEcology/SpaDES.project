## setupGitIgnore(): add packagePath / modulePath entries to a project's
## .gitignore. Pure filesystem work -- no git binary, no network.
##
## Note it reads and writes ".gitignore" relative to the WORKING DIRECTORY while
## testing for ".git" under paths$projectPath, so these tests set the working
## directory to projectPath, which is the arrangement setupProject() creates.

mkProj <- function(git = TRUE, gitignore = NULL) {
  proj <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(proj, "modules"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(proj, "packages"), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(git)) dir.create(file.path(proj, ".git"), showWarnings = FALSE)
  if (!is.null(gitignore)) writeLines(gitignore, file.path(proj, ".gitignore"))
  list(
    projectPath = proj,
    packagePath = file.path(proj, "packages"),
    modulePath  = file.path(proj, "modules")
  )
}

test_that("setupGitIgnore does nothing when gitignore is FALSE", {
  paths <- mkProj()
  withr::local_dir(paths$projectPath)

  SpaDES.project:::setupGitIgnore(paths, gitignore = FALSE, verbose = -1)

  expect_false(file.exists(".gitignore"))
})

test_that("setupGitIgnore does nothing outside a git repository", {
  paths <- mkProj(git = FALSE)
  withr::local_dir(paths$projectPath)

  SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1)

  # no .git directory -> the function must not create or touch .gitignore
  expect_false(file.exists(".gitignore"))
})

test_that("setupGitIgnore ignores the modulePath contents in a git repo", {
  paths <- mkProj()
  withr::local_dir(paths$projectPath)

  suppressMessages(
    SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1)
  )

  expect_true(file.exists(".gitignore"))
  expect_true("modules/*" %in% readLines(".gitignore"))
})

test_that("setupGitIgnore ignores a packagePath nested inside the project", {
  paths <- mkProj()
  withr::local_dir(paths$projectPath)

  suppressMessages(
    SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1)
  )

  gif <- readLines(".gitignore")
  # packagePath is inside projectPath, so it is recorded relative, not absolute
  expect_true("packages/*" %in% gif)
  expect_false(any(grepl(paths$projectPath, gif, fixed = TRUE)))
})

test_that("setupGitIgnore keeps pre-existing .gitignore entries", {
  paths <- mkProj(gitignore = c("*.Rproj.user", "secrets.txt"))
  withr::local_dir(paths$projectPath)

  suppressMessages(
    SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1)
  )

  gif <- readLines(".gitignore")
  expect_true(all(c("*.Rproj.user", "secrets.txt") %in% gif))
  expect_true("modules/*" %in% gif)
})

test_that("setupGitIgnore writes each entry once when run twice", {
  paths <- mkProj()
  withr::local_dir(paths$projectPath)

  suppressMessages(SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1))
  first <- readLines(".gitignore")
  suppressMessages(SpaDES.project:::setupGitIgnore(paths, gitignore = TRUE, verbose = -1))
  second <- readLines(".gitignore")

  expect_identical(sum(second == "modules/*"), 1L)
  expect_setequal(first, second)
})
