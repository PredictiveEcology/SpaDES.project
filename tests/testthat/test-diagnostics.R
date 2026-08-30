## Tests for the setupProject diagnostics infrastructure (R/diagnostics.R).
##
## Contract:
##   * a single end-of-call record per (context, expr, error) triple
##   * when a caller resolves the value via its own fallback, the record is
##     DELETED via setupDiagClearMatching() -- nothing tolerated to surface
##   * the report distinguishes "tolerated error" from "warning-only" only
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

  expect_silent(setupDiagClose())  # extra close is a no-op
  expect_null(setupDiagCurrent())
})

test_that("setupDiagRecord ignores calls when no scope is open", {
  closeAllScopes()
  expect_silent(setupDiagRecord(
    context = "X", expr = quote(foo()), error = "bad"))
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

  ## two records identical -> one after dedup
  setupDiagRecord(context = "sideEffects",
                  expr = quote(pkgload::load_all("~/GitHub/fireSenseUtils")),
                  error = "Failed to load R/ELFs.R")
  setupDiagRecord(context = "options",
                  expr = quote(other()),
                  error = "boom")
  setupDiagRecord(context = "sideEffects",
                  expr = quote(pkgload::load_all("~/GitHub/fireSenseUtils")),
                  error = "Failed to load R/ELFs.R")

  out <- capture.output(setupDiagReport(scope, strict = FALSE), type = "message")
  joined <- paste(out, collapse = "\n")

  ## header uses the "tolerated error" language
  expect_match(joined, "2 tolerated errors", fixed = TRUE)
  ## bullets are tagged "tolerated error", not "ERROR"/"recovered"
  expect_match(joined, "tolerated error [sideEffects]", fixed = TRUE)
  expect_no_match(joined, "recovered")
  ## load_all line is printed exactly once (dedup)
  expect_equal(sum(grepl("pkgload::load_all", out, fixed = TRUE)), 1L)
  ## strict-mode hint includes the new phrasing
  expect_match(joined, "stop on tolerated errors", fixed = TRUE)

  ## strict mode escalates after printing
  err <- tryCatch(setupDiagReport(scope, strict = TRUE), error = identity)
  expect_s3_class(err, "simpleError")
  expect_match(conditionMessage(err), "tolerated error", fixed = TRUE)
})

test_that("setupDiagReport is a no-op when no errors or warnings were recorded", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())
  expect_silent(setupDiagReport(scope))
})

test_that("setupDiagClearMatching deletes matching records, leaves others", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  setupDiagRecord(context = "defaultDots",
                  expr = quote(unlist(.samplingRange)),
                  error = "object '.samplingRange' not found")
  setupDiagRecord(context = "sideEffects",
                  expr = quote(pkgload::load_all("X")),
                  error = "boom")

  n <- setupDiagClearMatching("defaultDots", quote(unlist(.samplingRange)))
  expect_equal(n, 1L)
  expect_length(scope$attempts, 1L)
  expect_identical(scope$attempts[[1]]$context, "sideEffects")  # the other one stayed

  ## no-op when there's no match
  expect_equal(setupDiagClearMatching("defaultDots", quote(nothing_here())), 0L)
  expect_length(scope$attempts, 1L)
})

test_that("evalListElems pushes a record for an unrecovered failure", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  env <- new.env()
  out <- suppressMessages(
    evalListElems(quote(stop("boom-from-evalListElems")), envir = env, verbose = 0)
  )
  expect_identical(out, quote(stop("boom-from-evalListElems")))
  expect_length(scope$attempts, 1L)
  expect_match(scope$attempts[[1]]$expr, "boom-from-evalListElems", fixed = TRUE)
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

test_that("evalSUB records a tolerated-error record (no recovered flag)", {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  env <- new.env()
  suppressWarnings(
    evalSUB(quote(stop("boom-from-evalSUB")),
            valObjName = "sideEffects",
            envir = env, envir2 = env)
  )
  errRecs <- Filter(function(r) length(r$error), scope$attempts)
  expect_gte(length(errRecs), 1L)
  expect_true(any(vapply(errRecs, function(r)
    grepl("boom-from-evalSUB", paste(r$error, collapse = "\n")), logical(1))))
  expect_true(any(vapply(errRecs, function(r) identical(r$context, "sideEffects"),
                         logical(1))))
  ## records no longer carry a `recovered` field
  expect_true(all(vapply(scope$attempts, function(r) is.null(r$recovered),
                         logical(1))))
})

test_that(paste("evalDots: when defaultDots absorbs a failure, no record",
                "survives -- regression test for the .samplingRange case"), {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  callingEnv <- new.env(parent = globalenv())
  envirCur   <- new.env(parent = callingEnv)
  dots <- list(.samplingRange = quote(unlist(.samplingRange)))
  defaultDots <- list(.samplingRange = 1990:2020)

  out <- suppressWarnings(
    evalDots(dots = dots, dotsSUB = dots, defaultDots = defaultDots,
             envir = envirCur, callingEnv = callingEnv)
  )
  expect_equal(out$.samplingRange, 1990:2020)
  ## no .samplingRange entry survives in the diagnostic scope
  expect_false(
    any(vapply(scope$attempts, function(r)
      grepl("unlist(.samplingRange)", r$expr, fixed = TRUE), logical(1))),
    info = "defaultDots resolved the value; the prior record should be deleted"
  )
})

test_that(paste("evalDots: with the real build_proxy shape that surfaced",
                "the bug, no .samplingRange record survives"), {
  closeAllScopes()
  scope <- setupDiagOpen()
  on.exit(closeAllScopes())

  ## set up exactly as setupProject does: pass through a function that calls
  ## capture_dots() + build_proxy(). When the dot's expression cannot be
  ## eagerly forced (.samplingRange not found in the caller env), build_proxy
  ## installs an active binding that returns the unevaluated expression. This
  ## is the shape that produced the user-visible "object '.samplingRange'
  ## not found" before the recovery clear was wired in.
  fnNm <- function(...) {
    dotsSUB <- as.list(substitute(list(...)))[-1L]
    dotsAll <- capture_dots(...)
    proxy <- build_proxy(cur = environment(), caller = parent.frame(),
                         dots = dotsAll)
    list(proxyExec = proxy$exec, dotsSUB = dotsSUB)
  }
  got <- fnNm(.samplingRange = unlist(.samplingRange))
  defaultDots <- list(.samplingRange = 1990:2020)

  out <- suppressWarnings(
    evalDots(dots = got$dotsSUB, dotsSUB = got$dotsSUB, defaultDots = defaultDots,
             envir = environment(), callingEnv = got$proxyExec)
  )
  expect_equal(out$.samplingRange, 1990:2020)
  expect_false(
    any(vapply(scope$attempts, function(r)
      grepl("unlist(.samplingRange)", r$expr, fixed = TRUE), logical(1))),
    info = "defaultDots resolved the value; the prior record should be deleted"
  )
})
