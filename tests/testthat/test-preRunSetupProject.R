## preRunSetupProject(): parse a script and evaluate it up to (and optionally
## into) its setupProject() call.
##
## Offline: the function evaluates in the environment it is handed, so a stub
## `setupProject` there is what gets called -- no project is created, nothing is
## installed.

stubEnv <- function() {
  e <- new.env(parent = globalenv())
  e$setupProject <- function(...) list(called = TRUE, args = list(...))
  e
}

writeGlobal <- function(lines) {
  f <- withr::local_tempfile(fileext = ".R", .local_envir = parent.frame())
  writeLines(lines, f)
  f
}

test_that("preRunSetupProject evaluates the lines before setupProject", {
  e <- stubEnv()
  f <- writeGlobal(c(
    'before <- "ran"',
    'alsoBefore <- 41 + 1',
    'out <- setupProject(name = "proj", paths = list(projectPath = "."))'
  ))

  preRunSetupProject(file = f, upTo = TRUE, envir = e)

  expect_identical(e$before, "ran")
  expect_identical(e$alsoBefore, 42)
})

test_that("preRunSetupProject evaluates the whole call when upTo is TRUE", {
  e <- stubEnv()
  f <- writeGlobal(c(
    'out <- setupProject(name = "proj", paths = list(projectPath = "."), modules = "m")'
  ))

  preRunSetupProject(file = f, upTo = TRUE, envir = e)

  expect_true(e$out$called)
  expect_named(e$out$args, c("name", "paths", "modules"))
})

test_that("preRunSetupProject stops at the named argument when upTo is a string", {
  e <- stubEnv()
  f <- writeGlobal(c(
    'out <- setupProject(name = "proj", paths = list(projectPath = "."), modules = "m")'
  ))

  res <- preRunSetupProject(file = f, upTo = "paths", envir = e)

  # the call is truncated after `paths`, so `modules` is never passed
  expect_true(res$called)
  expect_named(res$args, c("name", "paths"))
})

test_that("preRunSetupProject returns the evaluated result", {
  e <- stubEnv()
  f <- writeGlobal('out <- setupProject(name = "proj")')

  res <- preRunSetupProject(file = f, upTo = TRUE, envir = e)

  expect_true(res$called)
  expect_identical(res$args$name, "proj")
})

test_that("preRunSetupProject treats NULL and '' like TRUE", {
  f <- writeGlobal(c(
    'out <- setupProject(name = "proj", paths = list(projectPath = "."))'
  ))

  for (u in list(NULL, "")) {
    e <- stubEnv()
    preRunSetupProject(file = f, upTo = u, envir = e)
    expect_named(e$out$args, c("name", "paths"))
  }
})
