## A `...` argument that never resolves to a value must fail loudly. Letting it
## through means the expression gets deparsed wherever the value belonged --
## pathBuild() turns it into a directory name -- and the run continues against
## paths nobody intended.

test_that("the ordinary pass-through idiom still resolves", {
  ## `x = x` is self-referential by name and is the normal way to forward a dot;
  ## defaultDots supplies it when the caller has not. This must NOT error.
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  out <- suppressWarnings(setupProject(
    .ELFind = .ELFind,
    .GCM    = .GCM,
    defaultDots = list(.ELFind = "4.3", .GCM = "CNRM-ESM2-1"),
    paths = list(packagePath = .libPaths()[1L]),
    updateRprofile = FALSE
  ))
  expect_identical(out$.ELFind, "4.3")
  expect_identical(out$.GCM, "CNRM-ESM2-1")
})

test_that("a dot that tests exists() on its own name errors, and says why", {
  ## The trap: setupProject() binds every dot before evaluating it, so
  ## exists("<dot>") is always TRUE, `<dot>` reads that binding, and the value is
  ## the expression itself. Previously this produced
  ## `outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind`.
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  expect_error(
    suppressWarnings(setupProject(
      .ELFind = .ELFind,
      .studyAreaName = if (exists(".studyAreaName")) .studyAreaName else .ELFind,
      defaultDots = list(.ELFind = "4.3"),
      paths = list(packagePath = .libPaths()[1L]),
      updateRprofile = FALSE
    )),
    "can never resolve"
  )
})

test_that("the error names the offending dot and explains the self-reference", {
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  err <- tryCatch(
    suppressWarnings(setupProject(
      .ELFind = .ELFind,
      .studyAreaName = if (exists(".studyAreaName")) .studyAreaName else .ELFind,
      defaultDots = list(.ELFind = "4.3"),
      paths = list(packagePath = .libPaths()[1L]),
      updateRprofile = FALSE
    )),
    error = function(e) conditionMessage(e))
  expect_match(err, "\\.studyAreaName")
  expect_match(err, "refer to their own name")
  expect_match(err, "defaultDots")
})

test_that("a dot that merely fails to evaluate is still tolerated", {
  ## The narrowing that matters. evalDotsOuter() evaluates in `envir`, which is
  ## setupProject()'s own frame -- so a dot referring to something only the
  ## CALLER can see cannot be evaluated there, and the documented fallback keeps
  ## the expression. That behaviour is deliberate and pinned by
  ## test-setupProject.R "test sideEffects that are not in sideEffect".
  ##
  ## A first cut of the self-reference check errored on any dot that was still a
  ## language object, which broke exactly this. The discriminator is
  ## self-reference, not "is a call".
  setupTest()
  libPathsOrig <- .libPaths(); on.exit(.libPaths(libPathsOrig), add = TRUE)
  fn <- function(x) NULL

  out <- suppressWarnings(setupProject(
    name = "hi",
    paths = list(packagePath = .libPaths()[1L]),
    lala = fn(1),
    updateRprofile = FALSE
  ))
  expect_false(is.null(out$lala))
})
