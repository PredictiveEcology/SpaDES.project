## Tests for the setupProject diagnostics infrastructure (R/diagnostics.R).
##
## These exercise the recorder/reporter directly with synthetic records, plus
## the evalListElems / evalSUB instrumentation against a controlled scope.
## They deliberately avoid spinning up a real setupProject() call (which is
## heavy and depends on the network); the goal here is to lock the contract:
##
##   * a single end-of-call record per (context, expr, error) triple
##   * recovered = TRUE is reported but not counted as "unrecovered"
##   * strict mode stops AFTER the summary is emitted (so the user keeps it)
##   * duplicates from the multi-layer try ladder collapse to one entry

closeAllScopes <- function() {
  while (!is.null(setupDiagCurrent())) setupDiagClose()
}

test_that("setupDiagOpen / Close maintain a clean stack", {
  closeAllScopes()
  expect_null(setupDiagCurrent())

  s1 <- setupDiagOpen()
  expect_identical(setupDiagCurrent(), s1)

  s2 <- setupDiagOpen()  # nested
  expect_identical(setupDiagCurrent(), s2)

  setupDiagClose()
  expect_identical(setupDiagCurrent(), s1)

  setupDiagClose()
  expect_null(setupDiagCurrent())

  ## extra close is a no-op
  expect_silent(setupDiagClose())
  expect_null(setupDiagCurrent())
})

test_that("setupDiagRecord ignores calls when no scope is open", {
  closeAllScopes()
  expect_silent(setupDiagRecord(
    context = "X", expr = quote(foo()), error = "bad", recovered = FALSE))
  ## nothing was created
  expect_null(setupDiagCurrent())
})

test_that("setupDiagRecord drops no-op records (no error, no warnings)", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())
  setupDiagRecord(context = "X", expr = quote(foo()))
  expect_length(scope$attempts, 0L)
})

test_that("setupDiagReport prints, dedupes, and respects strict mode", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  ## three records, the 1st and 3rd identical -> one dedup
  setupDiagRecord(context = "sideEffects",
                  expr = quote(pkgload::load_all("~/GitHub/fireSenseUtils")),
                  error = "Failed to load R/ELFs.R", recovered = FALSE)
  setupDiagRecord(context = "modules",
                  expr = quote(other()),
                  error = "object x not found", recovered = TRUE)
  setupDiagRecord(context = "sideEffects",
                  expr = quote(pkgload::load_all("~/GitHub/fireSenseUtils")),
                  error = "Failed to load R/ELFs.R", recovered = FALSE)

  out <- capture.output(setupDiagReport(scope, strict = FALSE), type = "message")
  joined <- paste(out, collapse = "\n")

  ## header reflects 1 unrecovered + 1 recovered after dedup
  expect_match(joined, "1 unrecovered, 1 recovered", fixed = TRUE)
  ## the unrecovered entry is tagged ERROR with its context
  expect_match(joined, "ERROR \\[sideEffects\\]")
  ## the recovered entry is tagged "recovered"
  expect_match(joined, "recovered \\[modules\\]")
  ## load_all line is printed exactly once (dedup)
  expect_equal(sum(grepl("pkgload::load_all", out, fixed = TRUE)), 1L)
  ## the strict-mode hint is included
  expect_match(joined, "SpaDES.project.strict", fixed = TRUE)

  ## strict mode escalates -- but only AFTER printing (the user keeps the summary)
  err <- tryCatch(setupDiagReport(scope, strict = TRUE), error = identity)
  expect_s3_class(err, "simpleError")
  expect_match(conditionMessage(err), "1 unrecovered error", fixed = TRUE)
})

test_that("setupDiagReport is a no-op when no errors or warnings were recorded", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())
  expect_silent(setupDiagReport(scope))
})

test_that("evalListElems pushes a recovered=FALSE record for an unrecovered failure", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  ## bad-code shape mirrors the real swallow: a single call that errors and
  ## that evalListElems cannot recover element-wise (not a list(...) call)
  env <- new.env()
  out <- suppressMessages(
    evalListElems(quote(stop("boom-from-evalListElems")), envir = env, verbose = 0)
  )
  ## evalListElems returns the unevaluated input when nothing recovered
  expect_identical(out, quote(stop("boom-from-evalListElems")))
  expect_length(scope$attempts, 1L)
  rec <- scope$attempts[[1]]
  expect_false(rec$recovered)
  expect_match(rec$expr, "boom-from-evalListElems", fixed = TRUE)
  expect_match(paste(rec$error, collapse = "\n"), "boom-from-evalListElems")
})

test_that("evalListElems records nothing when eval succeeds quietly", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  env <- new.env()
  out <- evalListElems(quote(1 + 1), envir = env, verbose = 0)
  expect_equal(out, 2)
  expect_length(scope$attempts, 0L)
})

test_that("evalSUB records the final-outcome failure (recovered = FALSE)", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  env <- new.env()
  ## evalSUB warns rather than stopping on full failure; suppressWarnings so the
  ## test stays quiet. We just care that the diagnostic record landed.
  suppressWarnings(
    evalSUB(quote(stop("boom-from-evalSUB")),
            valObjName = "sideEffects",
            envir = env, envir2 = env)
  )
  errRecs <- Filter(function(r) length(r$error), scope$attempts)
  expect_gte(length(errRecs), 1L)
  expect_true(any(vapply(errRecs, function(r) !r$recovered, logical(1))))
  expect_true(any(vapply(errRecs, function(r)
    grepl("boom-from-evalSUB", paste(r$error, collapse = "\n")), logical(1))))
  expect_true(any(vapply(errRecs, function(r) identical(r$context, "sideEffects"),
                         logical(1))))
})
