test_that("parseFileLists merges unnamed list elements in order, later wins, recursively", {
  merged <- parseFileLists(list(list(a = list(x = 1, y = 2)), list(a = list(y = 3))),
                           paths = list(), envir = globalenv())
  expect_identical(merged, list(a = list(x = 1, y = 3)))

  ## a NULL element (e.g. `if (FALSE) list(...)`) is skipped
  merged <- parseFileLists(list(list(a = list(x = 1)), NULL, list(a = list(y = 3))),
                           paths = list(), envir = globalenv())
  expect_identical(merged, list(a = list(x = 1, y = 3)))
})

test_that("setupProject - unnamed lists in params are merged", {
  skip_on_cran()
  setupTest()
  pp <- list(packagePath = .libPaths()[1L])
  nm <- paste0("test_SpaDES_project_", .rndstr(1))
  quietly <- function(x) suppressWarnings(suppressMessages(x))
  hindcast <- FALSE
  for (hc in c(FALSE, TRUE)) {
    hindcast <- hc
    out <- quietly(setupProject(name = nm, paths = pp, packages = NULL,
                                params = list(list(.globals = list(a = 1, b = 2, c = 4)),
                                              if (hindcast) list(.globals = list(b = 3)))))
    expect_identical(out$params$.globals$a, 1)
    expect_identical(out$params$.globals$b, if (hc) 3 else 2)
    expect_identical(out$params$.globals$c, 4)
  }
})
