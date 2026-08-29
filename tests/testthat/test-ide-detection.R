# Front-end detection and the "am I already in this project?" test.
#
# Positron's R kernel (ark) rewrites the body of `rstudioapi::isAvailable()`
# to `TRUE`, so the old `isRstudio()` was TRUE in Positron as well.  That made
# the RStudio-only restart machinery (an .Rproj project, the
# `rstudio.sessionInit` hook) engage in a front-end that has neither.

testthat::test_that("isPositron follows the POSITRON environment variable", {
  withr::with_envvar(c(POSITRON = "1"), {
    testthat::expect_true(SpaDES.project:::isPositron())
  })
  withr::with_envvar(c(POSITRON = NA), {
    # NA unsets it; `tools:positron` is not attached in a plain R session.
    testthat::expect_identical(SpaDES.project:::isPositron(),
                               "tools:positron" %in% search())
  })
})

testthat::test_that("isRstudio is FALSE in Positron even though rstudioapi answers", {
  # Simulate what ark does: rstudioapi::isAvailable() reports TRUE.
  testthat::local_mocked_bindings(
    hasRstudioApi = function() TRUE,
    .package = "SpaDES.project"
  )
  withr::with_envvar(c(POSITRON = "1"), {
    testthat::expect_false(SpaDES.project:::isRstudio())
  })
  withr::with_envvar(c(POSITRON = NA), {
    testthat::expect_true(SpaDES.project:::isRstudio())
  })
})

testthat::test_that(".isCurrentIdeProject uses the workspace folder in Positron", {
  pp <- tempfile("ws"); dir.create(pp)
  other <- tempfile("other"); dir.create(other)
  on.exit(unlink(c(pp, other), recursive = TRUE), add = TRUE)

  # No .Rproj anywhere -- the normal Positron case.  Pre-fix this returned
  # FALSE even when the open workspace WAS pp, so setupRestart() asked for a
  # restart on every call (an endless restart loop).
  testthat::expect_true(SpaDES.project:::.isCurrentIdeProject(pp, pp, inPositron = TRUE))
  testthat::expect_false(SpaDES.project:::.isCurrentIdeProject(pp, other, inPositron = TRUE))

  # getActiveProject() is NULL when no folder is open at all.
  testthat::expect_false(SpaDES.project:::.isCurrentIdeProject(pp, NULL, inPositron = TRUE))
})

testthat::test_that(".isCurrentIdeProject still requires an .Rproj in RStudio", {
  testthat::skip_if_not_installed("rprojroot")

  pp <- tempfile("rsproj"); dir.create(pp)
  on.exit(unlink(pp, recursive = TRUE), add = TRUE)

  # Active project reported as pp, but no .Rproj file yet.
  testthat::expect_false(SpaDES.project:::.isCurrentIdeProject(pp, pp, inPositron = FALSE))

  writeLines("Version: 1.0\n", file.path(pp, paste0(basename(pp), ".Rproj")))
  testthat::expect_true(SpaDES.project:::.isCurrentIdeProject(pp, pp, inPositron = FALSE))
})

testthat::test_that(".isCurrentIdeProject is FALSE, not NA, for an empty workspace answer", {
  pp <- tempfile("ws0"); dir.create(pp)
  on.exit(unlink(pp, recursive = TRUE), add = TRUE)

  # A length-0 or NA answer must not leak an NA into the caller's `if()`.
  for (curProj in list(character(0), NA_character_, "")) {
    testthat::expect_false(
      SpaDES.project:::.isCurrentIdeProject(pp, curProj, inPositron = TRUE)
    )
  }
})
