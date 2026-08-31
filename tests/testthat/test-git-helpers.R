## Small git-facing helpers from setupProject.R.
##
## setupGitHub() itself is not covered here: it couples to 20+ distinct calls
## across gert, usethis, gh, gitcreds and rprojroot, so a mocked test would
## mostly assert call order. These are its constituent pieces, which do have
## clean seams -- .shouldOfferClone() needs no mocking at all.

test_that(".shouldOfferClone is TRUE only for a plain directory", {
  plain <- withr::local_tempdir()
  expect_true(SpaDES.project:::.shouldOfferClone(plain))
})

test_that(".shouldOfferClone is FALSE inside a git repository", {
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, ".git"))

  expect_false(SpaDES.project:::.shouldOfferClone(repo))
})

test_that(".shouldOfferClone is FALSE inside an RStudio project", {
  proj <- withr::local_tempdir()
  writeLines("Version: 1.0", file.path(proj, "some.Rproj"))

  expect_false(SpaDES.project:::.shouldOfferClone(proj))
})

test_that("getGitUserName uses the GitHub login when not interactive", {
  skip_if_not_installed("gh")
  testthat::local_mocked_bindings(
    gh_whoami = function(...) list(login = "octocat"),
    .package = "gh"
  )

  res <- suppressMessages(SpaDES.project:::getGitUserName())

  expect_identical(res$gitUserName, "octocat")
  expect_identical(res$gitUserNamePoss, "octocat")
})

test_that("getGitUserName errors when GitHub returns no login", {
  skip_if_not_installed("gh")
  testthat::local_mocked_bindings(
    gh_whoami = function(...) list(login = NULL),
    .package = "gh"
  )

  # the message names the two ways to fix credentials
  expect_error(suppressMessages(SpaDES.project:::getGitUserName()),
               "gitcreds_set|gh_token_help")
})

## gitEvalWithGitConfigOnError() evaluates `expr` in the CALLER's frame, so
## these use ordinary local functions as counters.

test_that("gitEvalWithGitConfigOnError evaluates the expression once on success", {
  hits <- 0L
  f <- function() { hits <<- hits + 1L; "ok" }

  SpaDES.project:::gitEvalWithGitConfigOnError(quote(f()))

  expect_identical(hits, 1L)
})

test_that("gitEvalWithGitConfigOnError retries then gives up without erroring", {
  hits <- 0L
  f <- function() { hits <<- hits + 1L; stop("something unrelated to credentials") }

  expect_no_error(
    suppressMessages(capture.output(
      SpaDES.project:::gitEvalWithGitConfigOnError(quote(f())), type = "message"
    ))
  )
  # two attempts, and the failure is swallowed rather than propagated
  expect_identical(hits, 2L)
})

test_that("gitEvalWithGitConfigOnError honours an explicit envir", {
  e <- new.env(parent = baseenv())
  e$hits <- 0L
  e$f <- function() { e$hits <- e$hits + 1L; "ok" }

  SpaDES.project:::gitEvalWithGitConfigOnError(quote(f()), envir = e)

  expect_identical(e$hits, 1L)
})

test_that("gitEvalWithGitConfigOnError sets git config when user.name is missing", {
  skip_if_not_installed("usethis")
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    use_git_config = function(...) {
      seen$args <- list(...)
      invisible(NULL)
    },
    .package = "usethis"
  )

  suppressMessages(capture.output(
    SpaDES.project:::gitEvalWithGitConfigOnError(
      quote(stop("Error: user.name is not set"))
    ), type = "message"
  ))

  expect_false(is.null(seen$args))
  expect_identical(seen$args$scope, "project")
  expect_true(all(c("user.name", "user.email") %in% names(seen$args)))
})
