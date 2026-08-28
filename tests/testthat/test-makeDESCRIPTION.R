## makeDESCRIPTION() / makeDESCRIPTIONproject(): build DESCRIPTION file(s) from
## SpaDES module metadata. Offline -- fixture modules on disk, and no "(HEAD)"
## in reqdPkgs (that is the one branch which queries repos).

# Write a minimal module whose main .R file carries defineModule() metadata.
mkModule <- function(modulePath, name, reqdPkgs = '"data.table"', version = "1.2.3") {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf('  description = "desc of %s",', name),
      sprintf('  version = list(%s = "%s"),', name, version),
      '  authors = person("Ada", "Lovelace", role = c("aut", "cre")),',
      sprintf("  reqdPkgs = list(%s)", reqdPkgs),
      "))"),
    file.path(modulePath, name, paste0(name, ".R"))
  )
  modulePath
}

dcfField <- function(file, field) {
  v <- read.dcf(file, fields = field)[1, 1]
  unname(v)
}

test_that("makeDESCRIPTION builds a DESCRIPTION from one module's metadata", {
  td <- withr::local_tempdir()
  mkModule(td, "modA", reqdPkgs = '"data.table", "terra (>= 1.7)"')

  f <- makeDESCRIPTION("modA", modulePath = td, write = FALSE, verbose = 0)
  d <- readLines(f)

  expect_identical(dcfField(f, "Package"), "modA")
  expect_identical(dcfField(f, "Title"), "modA")
  expect_identical(dcfField(f, "Description"), "desc of modA")
  expect_identical(dcfField(f, "Type"), "Package")
  expect_identical(dcfField(f, "License"), "GPL-3")

  imports <- dcfField(f, "Imports")
  expect_match(imports, "data.table")
  expect_match(imports, "terra \\(>= 1.7\\)")
  # SpaDES.core is implied for every module, so it is prepended
  expect_match(imports, "SpaDES.core")

  expect_match(dcfField(f, "Suggests"), "knitr")
})

test_that("makeDESCRIPTION takes the module's own version, once", {
  td <- withr::local_tempdir()
  mkModule(td, "modA", version = "9.8.7")

  f <- makeDESCRIPTION("modA", modulePath = td, write = FALSE, verbose = 0)

  expect_identical(dcfField(f, "Version"), "9.8.7")
  # `version = list(modA = "9.8.7")` is an unevaluated call; pasting it directly
  # vectorises and emits two Version: lines ("list", then the number)
  expect_length(grep("^Version:", readLines(f)), 1L)
})

test_that("makeDESCRIPTION converts underscores in the package name", {
  td <- withr::local_tempdir()
  mkModule(td, "mod_B")

  f <- makeDESCRIPTION("mod_B", modulePath = td, write = FALSE, verbose = 0)

  # R package names cannot contain underscores; module names can
  expect_identical(dcfField(f, "Package"), "mod.B")
  expect_identical(dcfField(f, "Title"), "mod_B")
})

test_that("makeDESCRIPTION honours explicit overrides", {
  td <- withr::local_tempdir()
  mkModule(td, "modA")

  f <- makeDESCRIPTION("modA", modulePath = td, write = FALSE, verbose = 0,
                       package = "myPkg", title = "My Title",
                       description = "My description", version = "0.0.1",
                       date = "2020-01-01")

  expect_identical(dcfField(f, "Package"), "myPkg")
  expect_identical(dcfField(f, "Title"), "My Title")
  expect_identical(dcfField(f, "Description"), "My description")
  expect_identical(dcfField(f, "Version"), "0.0.1")
  expect_identical(dcfField(f, "Date"), "2020-01-01")
})

test_that("makeDESCRIPTION writes one file per module, each with its own metadata", {
  td <- withr::local_tempdir()
  mkModule(td, "modA", reqdPkgs = '"data.table"', version = "1.0.0")
  mkModule(td, "modB", reqdPkgs = '"terra"',      version = "2.0.0")

  fs <- makeDESCRIPTION(c("modA", "modB"), modulePath = td, write = TRUE, verbose = 0)

  expect_length(fs, 2L)
  expect_true(all(file.exists(fs)))
  # each DESCRIPTION lands beside its own module
  expect_setequal(basename(dirname(fs)), c("modA", "modB"))

  byModule <- setNames(fs, basename(dirname(fs)))
  # regression: a single shared `d` meant every file got the LAST module's
  # metadata, and a vector of folders made cat() error outright
  expect_identical(dcfField(byModule[["modA"]], "Package"), "modA")
  expect_identical(dcfField(byModule[["modA"]], "Version"), "1.0.0")
  expect_identical(dcfField(byModule[["modB"]], "Package"), "modB")
  expect_identical(dcfField(byModule[["modB"]], "Version"), "2.0.0")

  expect_match(dcfField(byModule[["modA"]], "Imports"), "data.table")
  expect_false(grepl("terra", dcfField(byModule[["modA"]], "Imports")))
  expect_match(dcfField(byModule[["modB"]], "Imports"), "terra")
})

test_that("singleDESCRIPTION merges every module's reqdPkgs into one file", {
  td <- withr::local_tempdir()
  pp <- withr::local_tempdir()
  mkModule(td, "modA", reqdPkgs = '"data.table"')
  mkModule(td, "modB", reqdPkgs = '"data.table (>= 1.14)", "terra"')

  f <- makeDESCRIPTION(c("modA", "modB"), modulePath = td, projectPath = pp,
                       singleDESCRIPTION = TRUE, write = TRUE, verbose = 0,
                       package = "Project", title = "Project",
                       description = "Project", version = "1.0.0",
                       authors = "someone")

  expect_length(f, 1L)
  expect_identical(normalizePath(f), normalizePath(file.path(pp, "DESCRIPTION")))

  imports <- dcfField(f, "Imports")
  expect_match(imports, "terra")
  # the redundant data.table entries collapse to the stricter constraint
  expect_match(imports, "data.table \\(>= 1.14\\)")
  expect_length(gregexpr("data\\.table", imports)[[1]], 1L)
})

test_that("singleDESCRIPTION lifts GitHub specs into Remotes", {
  td <- withr::local_tempdir()
  pp <- withr::local_tempdir()
  mkModule(td, "modA", reqdPkgs = '"PredictiveEcology/Require@development"')

  f <- makeDESCRIPTION("modA", modulePath = td, projectPath = pp,
                       singleDESCRIPTION = TRUE, write = TRUE, verbose = 0,
                       package = "Project", title = "Project",
                       description = "Project", version = "1.0.0",
                       authors = "someone")

  expect_identical(dcfField(f, "Remotes"), "PredictiveEcology/Require@development")
  # the bare package name, not the GitHub spec, goes in Imports
  expect_match(dcfField(f, "Imports"), "Require")
})

test_that("makeDESCRIPTION with write = FALSE does not touch the module folder", {
  td <- withr::local_tempdir()
  mkModule(td, "modA")

  f <- makeDESCRIPTION("modA", modulePath = td, write = FALSE, verbose = 0)

  expect_true(file.exists(f))
  expect_false(file.exists(file.path(td, "modA", "DESCRIPTION")))
})

test_that("makeDESCRIPTIONproject defaults to a single project-level DESCRIPTION", {
  td <- withr::local_tempdir()
  pp <- withr::local_tempdir()
  mkModule(td, "modA", reqdPkgs = '"data.table"')
  mkModule(td, "modB", reqdPkgs = '"terra"')

  f <- makeDESCRIPTIONproject(c("modA", "modB"), modulePath = td,
                              projectPath = pp, verbose = 0)

  expect_length(f, 1L)
  expect_true(file.exists(file.path(pp, "DESCRIPTION")))
  expect_identical(dcfField(f, "Package"), "Project")
  expect_identical(dcfField(f, "Version"), "1.0.0")

  imports <- dcfField(f, "Imports")
  expect_match(imports, "data.table")
  expect_match(imports, "terra")
})

test_that("makeDESCRIPTION accepts pre-parsed metadataList", {
  td <- withr::local_tempdir()
  mkModule(td, "modA", version = "3.3.3")
  md <- list(parse(file.path(td, "modA", "modA.R"), keep.source = TRUE))

  f <- makeDESCRIPTION("modA", modulePath = td, write = FALSE, verbose = 0,
                       metadataList = md)

  expect_identical(dcfField(f, "Version"), "3.3.3")
})
