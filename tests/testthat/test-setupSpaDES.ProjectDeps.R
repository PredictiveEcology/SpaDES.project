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
