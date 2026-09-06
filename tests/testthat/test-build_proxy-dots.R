# Regression test: dots supplied by defaultDots must reach a `paths` formal as
# VALUES. Historically a proxy of active bindings handed back the unevaluated
# expression, and pathBuild()/file.path() deparsed it into a directory name like
#   outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/unlist_.samplingRange/.GCM
# The proxy is gone (dots are resolved into one scope environment, see
# R/resolveDots.R); this pins the user-visible contract that motivated it.

test_that("setupProject: dots supplied by defaultDots reach paths as values, not expressions", {
  skip_on_cran()
  setupTest()                       # temp cwd + the suite's shared temp library
  libPathsOrig <- .libPaths()
  on.exit(.libPaths(libPathsOrig), add = TRUE)

  # `packagePath` keeps setupProject() in that shared library rather than
  # creating one of its own. setupProject() narrows .libPaths() to the project
  # library (see helpers.R), so a per-test library that is removed afterwards
  # leaves every later test in the run pointing at a directory that no longer
  # exists -- and this file sorts first in tests/testthat.
  #
  # None of .ELFind/.GCM/.samplingRange exist here, so defaultDots must supply
  # them. That is what a batch spawn does (e.g. FireSenseTesting's expt.R
  # explicitly rm()s .ELFind before calling preRunSetupProject()), and it is the
  # only situation in which the bug appears.
  try(silent = TRUE, suppressWarnings(setupProject(
    .ELFind        = .ELFind,
    .GCM           = .GCM,
    .samplingRange = unlist(.samplingRange),
    defaultDots = list(.ELFind = "4.3", .GCM = "CNRM-ESM2-1",
                       .samplingRange = 1990:2020),
    paths = list(packagePath = .libPaths()[1L],
                 outputPath  = file.path("outputs", .ELFind, .GCM)),
    updateRprofile = FALSE
  )))

  op <- getOption("spades.outputPath")
  expect_false(grepl("\\.ELFind|\\.GCM|unlist_", op),
               info = paste("outputPath contains deparsed dot expressions:", op))
  expect_match(op, "outputs/4.3/CNRM-ESM2-1$")
})
