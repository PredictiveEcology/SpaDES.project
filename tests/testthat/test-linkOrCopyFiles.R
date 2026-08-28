## linkOrCopyFiles(): hard-link a set of files into a staging tree, falling back
## to copying. Pure filesystem work -- no network, no git, no simList.

mkTree <- function(root, files) {
  for (f in files) {
    p <- file.path(root, f)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines(f, p)
  }
  root
}

test_that("linkOrCopyFiles stages a directory's files under its basename", {
  src <- file.path(withr::local_tempdir(), "modA")
  mkTree(src, c("a.R", "b.R"))
  dest <- withr::local_tempdir()

  res <- SpaDES.project:::linkOrCopyFiles(fromDirs = src, toBaseDir = dest)

  expect_named(res, c("fromFilesList", "toFilesList"))
  expect_setequal(basename(unlist(res$fromFilesList)), c("a.R", "b.R"))
  # files land under <toBaseDir>/<basename(fromDir)>/
  expect_true(all(file.exists(unlist(res$toFilesList))))
  expect_setequal(basename(unlist(res$toFilesList)), c("a.R", "b.R"))
  expect_true(all(grepl("/modA/", unlist(res$toFilesList), fixed = TRUE)))
})

test_that("linkOrCopyFiles preserves nested subdirectories", {
  src <- file.path(withr::local_tempdir(), "modB")
  mkTree(src, c("R/one.R", "R/deeper/two.R", "top.txt"))
  dest <- withr::local_tempdir()

  res <- SpaDES.project:::linkOrCopyFiles(fromDirs = src, toBaseDir = dest)

  staged <- unlist(res$toFilesList)
  expect_true(all(file.exists(staged)))
  expect_true(any(grepl("modB/R/deeper/two.R$", staged)))
  expect_true(any(grepl("modB/top.txt$", staged)))
})

test_that("linkOrCopyFiles reproduces file content at the destination", {
  src <- file.path(withr::local_tempdir(), "modC")
  mkTree(src, "payload.R")
  dest <- withr::local_tempdir()

  res <- SpaDES.project:::linkOrCopyFiles(fromDirs = src, toBaseDir = dest)

  expect_identical(readLines(unlist(res$toFilesList)[[1]]), "payload.R")
})

test_that("linkOrCopyFiles handles several source directories", {
  base <- withr::local_tempdir()
  s1 <- mkTree(file.path(base, "modD"), "d.R")
  s2 <- mkTree(file.path(base, "modE"), "e.R")
  dest <- withr::local_tempdir()

  res <- SpaDES.project:::linkOrCopyFiles(fromDirs = c(s1, s2), toBaseDir = dest)

  expect_length(res$fromFilesList, 2L)
  staged <- unlist(res$toFilesList)
  expect_true(all(file.exists(staged)))
  expect_true(any(grepl("modD/d.R$", staged)))
  expect_true(any(grepl("modE/e.R$", staged)))
})

test_that("linkOrCopyFiles copes with an empty source directory", {
  src <- file.path(withr::local_tempdir(), "empty")
  dir.create(src, recursive = TRUE)
  dest <- withr::local_tempdir()

  res <- SpaDES.project:::linkOrCopyFiles(fromDirs = src, toBaseDir = dest)

  expect_length(unlist(res$fromFilesList), 0L)
  expect_length(unlist(res$toFilesList), 0L)
})

test_that("linkOrCopyFiles honours explicit from/to file lists", {
  base <- withr::local_tempdir()
  src <- mkTree(file.path(base, "src"), c("x.R", "y.R"))
  from <- list(file.path(src, c("x.R", "y.R")))
  to   <- list(file.path(base, "staged", "nested", c("x.R", "y.R")))

  res <- SpaDES.project:::linkOrCopyFiles(fromFilesList = from, toFilesList = to)

  # the destination's parent directories are created for us
  expect_true(all(file.exists(unlist(to))))
  expect_identical(res$toFilesList, to)
  expect_identical(readLines(unlist(to)[[1]]), "x.R")
})
