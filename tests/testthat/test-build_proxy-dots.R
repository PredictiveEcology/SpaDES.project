# Regression tests for the `...` proxy bindings built by build_proxy().
#
# Bug: build_proxy() installs an active binding per dot that closes over the
# capture-time `val`/`expr` and never consults `cur` again. bind_forward() --
# used for every *other* name -- forwards live into `cur`, and
# expose_new_bindings() only forwards names that are not already bound in
# `exec`, so a dot never gets upgraded. Once evalDots() resolves a dot into
# `cur`, the proxy still hands back the unevaluated expression, and any later
# consumer (e.g. a `paths` formal calling pathBuild()) deparses that expression
# into the value's place, producing directory names like
#   outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/unlist_.samplingRange/.GCM

# build_proxy() reads its caller's frame via sys.parent()/match.call(), so it
# has to be invoked from inside a function, exactly as setupProject() does.
mkProxy <- function(dotsAll, cur, ...) {
  build_proxy(cur = cur, caller = parent.frame(), dots = dotsAll)
}

test_that("build_proxy: a dot resolved into `cur` reads back as the value, not the expression", {
  # capture_dots() leaves `vals` NULL for anything that is not a symbol or
  # literal already available in the caller, which is the situation whenever
  # defaultDots is what supplies the dot.
  dotsAll <- list(
    exprs = list(.ELFind = quote(.ELFind),
                 .samplingRange = quote(unlist(.samplingRange))),
    vals  = list(.ELFind = NULL, .samplingRange = NULL)
  )

  cur    <- new.env(parent = globalenv())
  proxy  <- mkProxy(dotsAll, cur)

  # Before resolution there is nothing better to return than the expression.
  expect_true(is.name(get(".ELFind", envir = proxy$exec)))

  # evalDots() resolves dots by writing them into `cur`.
  assign(".ELFind", "4.3", envir = cur)
  assign(".samplingRange", 1990:2020, envir = cur)

  expect_identical(get(".ELFind", envir = proxy$exec), "4.3")
  expect_identical(get(".samplingRange", envir = proxy$exec), 1990:2020)

  # And the values must survive evaluation of an expression in the proxy env,
  # which is how setupProject() evaluates its formal arguments.
  expect_identical(
    eval(quote(file.path(.ELFind, paste(range(.samplingRange), collapse = "-"))),
         envir = proxy$exec),
    "4.3/1990-2020"
  )
})

test_that("build_proxy: a dot forced at capture time still reads back as its value", {
  # Guard the non-regressing path: when capture_dots() *could* force the dot,
  # `val` is populated and must still be returned when `cur` has nothing.
  dotsAll <- list(exprs = list(.GCM = quote(.GCM)), vals = list(.GCM = "CNRM-ESM2-1"))
  cur     <- new.env(parent = globalenv())
  proxy   <- mkProxy(dotsAll, cur)

  expect_identical(get(".GCM", envir = proxy$exec), "CNRM-ESM2-1")

  # A later resolution in `cur` takes precedence -- `cur` is the source of truth.
  assign(".GCM", "CanESM5", envir = cur)
  expect_identical(get(".GCM", envir = proxy$exec), "CanESM5")
})

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
