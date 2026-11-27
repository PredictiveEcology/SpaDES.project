## Regression guard for the packages copied into a new project library.
##
## setupProject() copies SpaDES.project, Require and their dependencies into a
## fresh packagePath before Require can install anything there. It used to copy
## only the direct Depends/Imports of those two packages, so e.g. processx was
## copied without ps and the next step failed with
## ".onLoad failed in loadNamespace() for 'processx' ... there is no package
## called 'ps'". The copied set must be closed under hard dependencies.

hardDeps <- c("Depends", "Imports", "LinkingTo")

test_that("packages copied into a new project library bring their own dependencies", {
  lib <- withr::local_tempdir("projLib")
  setupSpaDES.ProjectDeps(paths = list(packagePath = lib),
                          deps = c("SpaDES.project", "Require"),
                          prevLibPaths = .libPaths(), verbose = 0)

  copied <- installed.packages(lib.loc = lib)
  expect_true("Require" %in% rownames(copied))

  needed <- unlist(tools::package_dependencies(rownames(copied), db = copied,
                                               which = hardDeps))
  missing <- setdiff(needed, c(rownames(copied), .basePkgs))
  expect_length(missing, 0)

  # the observed failure: processx copied, ps (its Import) not
  if ("processx" %in% rownames(copied))
    expect_true("ps" %in% rownames(copied))
})

## The bootstrap copy must be a copy, never a hard link: a file damaged in place
## in one library (2026-11-25: 26,525 files emptied) was damaged in every
## library that shared its inode. Tiny local packages; no user library involved.

## The copy skips a package with no INDEX (not installed); a tiny package built without help pages has none.
installTinyWithIndex <- function(srcs, lib) {
  installTiny(srcs, lib)
  for (pkg in basename(srcs)) writeLines(paste(pkg, "Tiny"), file.path(lib, pkg, "INDEX"))
}

test_that("bootstrap packages are copied into the project library, not hard-linked", {
  skip_if_not_installed("callr")
  skip_if_not_installed("fs")
  skip_on_os("windows")
  srcLib <- withr::local_tempdir("srcLib")
  installTinyWithIndex(c(mkTinyPkg("tinyA", "1.0.0"), mkTinyPkg("tinyB", "1.0.0")), srcLib)
  projLib <- withr::local_tempdir("projLib")

  setupSpaDES.ProjectDeps(paths = list(packagePath = projLib), deps = c("tinyA", "tinyB"),
                          prevLibPaths = srcLib, verbose = 0)

  expect_true(all(c("tinyA", "tinyB") %in% dir(projLib)))
  expect_length(dir(projLib, pattern = "^00CLONE-"), 0) # no temporary folder left behind
  for (pkg in c("tinyA", "tinyB")) {
    rel <- dir(file.path(srcLib, pkg), recursive = TRUE, all.files = TRUE)
    src <- file.path(srcLib, pkg, rel)
    dst <- file.path(projLib, pkg, rel)
    expect_true(all(file.exists(dst)))
    expect_true(all(fs::file_info(dst)$hard_links == 1))
    expect_true(all(fs::file_info(src)$hard_links == 1))
    expect_false(any(inodeOf(dst) %in% inodeOf(src)))
  }

  # damage in place (the incident): neither library sees the other's damage
  srcFile <- file.path(srcLib, "tinyA", "DESCRIPTION")
  dstFile <- file.path(projLib, "tinyA", "DESCRIPTION")
  full <- file.size(dstFile)
  expect_gt(full, 0)
  close(file(srcFile, "w")) # truncates in place
  expect_identical(file.size(srcFile), 0)
  expect_identical(file.size(dstFile), full)

  dstFile2 <- file.path(projLib, "tinyB", "DESCRIPTION")
  srcFile2 <- file.path(srcLib, "tinyB", "DESCRIPTION")
  full2 <- file.size(srcFile2)
  close(file(dstFile2, "w"))
  expect_identical(file.size(srcFile2), full2)
})

test_that("bootstrap packages load from the project library", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  srcLib <- withr::local_tempdir("srcLib")
  installTinyWithIndex(mkTinyPkg("tinyA", "1.0.0"), srcLib)
  projLib <- withr::local_tempdir("projLib")
  setupSpaDES.ProjectDeps(paths = list(packagePath = projLib), deps = "tinyA",
                          prevLibPaths = srcLib, verbose = 0)
  out <- callr::r(function() {
    loadNamespace("tinyA")
    c(f_tinyA = tinyA::f_tinyA(), path = dirname(getNamespaceInfo("tinyA", "path")))
  }, libpath = projLib)
  expect_identical(unname(out["f_tinyA"]), "1.0.0")
  expect_identical(normalizePath(unname(out["path"])), normalizePath(projLib))
})
