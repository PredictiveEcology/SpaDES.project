## setupProject diagnostics ---------------------------------------------------
##
## setupProject's resolution layer (evalSUB + evalListElems and the fallbacks
## they call) is intentionally tolerant: an entry that fails in `envir` is
## retried in `envir2`, then against sys.frames(), then against a synthetic env;
## an entry that fails as a whole `list(...)`/block is retried element-wise. The
## design is right -- a missing optional file or an undefined name shouldn't
## abort the whole setup -- but the cost is that a *real* failure (e.g.
## pkgload::load_all() failing because a package's R/ file has a parse error)
## gets demoted to a verbose-gated `message()` and the run continues with a
## half-loaded or stale namespace.
##
## This module adds a per-`setupProject()` diagnostic scope:
##   - `setupDiagOpen()` pushes a scope onto a stack
##   - `evalSUB` / `evalListElems` push records onto the current scope when
##     they end on a try-error -- and the *caller* deletes that record via
##     `setupDiagClearMatching()` if it resolved the value via its own fallback
##     (e.g. evalDots() substituting from defaultDots). What survives in the
##     scope at the end is therefore exactly the failures the run continued
##     past without resolving.
##   - `setupDiagReport()` prints those as "tolerated errors" (plus any
##     warnings collected along the way) at the end of setupProject.
##   - `options(SpaDES.project.strict = TRUE)` escalates the summary to a stop.

.spadesProjectDiag <- new.env(parent = emptyenv())
.spadesProjectDiag$stack <- list()

## Push a fresh diagnostic scope; returns it (callers don't usually need it).
setupDiagOpen <- function() {
  scope <- new.env(parent = emptyenv())
  scope$attempts <- list()
  s <- .spadesProjectDiag$stack
  s[[length(s) + 1L]] <- scope
  .spadesProjectDiag$stack <- s
  scope
}

## Pop the current scope. Safe to call when none is open.
setupDiagClose <- function() {
  s <- .spadesProjectDiag$stack
  if (length(s)) .spadesProjectDiag$stack <- s[-length(s)]
  invisible()
}

## The active scope, or NULL if no setupProject call is on the stack.
setupDiagCurrent <- function() {
  s <- .spadesProjectDiag$stack
  if (length(s)) s[[length(s)]] else NULL
}

## Record one tolerated-error / warning attempt.
##
## - `context`   short tag (`"sideEffects"`, `"modules"`, ...); use NA if unknown
## - `expr`      the original (unevaluated) expression the caller was resolving
## - `error`     try-error or condition collected from the failing path; NULL if
##               the only thing of note was a warning
## - `warnings`  warning messages collected via withCallingHandlers
setupDiagRecord <- function(context = NA_character_, expr, error = NULL,
                            warnings = NULL) {
  scope <- setupDiagCurrent()
  if (is.null(scope)) return(invisible())
  if (is.null(error) && !length(warnings)) return(invisible())  # nothing to report

  exprStr <- tryCatch(paste(deparse(expr, width.cutoff = 500L), collapse = " "),
                      error = function(e) "<unparseable>")
  if (!is.null(error)) error <- trimws(as.character(error))

  scope$attempts[[length(scope$attempts) + 1L]] <- list(
    context = if (is.null(context)) NA_character_ else context,
    expr = exprStr,
    error = error,
    warnings = if (length(warnings)) as.character(warnings) else NULL
  )
  invisible()
}

## Delete matching prior records. Use this from a caller that *did* eventually
## resolve a value (e.g. evalDots() substituting a defaultDots fallback after
## evalSUB failed): the prior record is removed entirely, so the run continues
## without a spurious entry in the summary.
##
## Match by (context, expr-deparse). Returns invisibly the number of records
## that were deleted.
setupDiagClearMatching <- function(context, expr) {
  scope <- setupDiagCurrent()
  if (is.null(scope)) return(invisible(0L))
  if (!length(scope$attempts)) return(invisible(0L))

  exprStr <- tryCatch(paste(deparse(expr, width.cutoff = 500L), collapse = " "),
                      error = function(e) "<unparseable>")
  contextChr <- if (is.null(context) || is.na(context)) NA_character_ else context

  keep <- vapply(scope$attempts, function(r) {
    !(identical(r$expr, exprStr) &&
        (is.na(contextChr) || identical(r$context, contextChr)))
  }, logical(1))
  n <- sum(!keep)
  scope$attempts <- scope$attempts[keep]
  invisible(n)
}

## Truncate a one-line string for the summary header.
.diagTrunc <- function(s, n = 120L) {
  if (!nzchar(s) || nchar(s) <= n) s else paste0(substr(s, 1L, n - 3L), "...")
}

## Render and optionally escalate the diagnostic scope.
##
## Output goes via `message()` so it lands on stderr and is not swallowed by
## suppressMessages() at the user's call-site by accident; strict mode raises
## a final error after the summary so the user keeps the full diagnostic and
## the stop trace.
setupDiagReport <- function(scope = setupDiagCurrent(),
                            strict = getOption("SpaDES.project.strict", FALSE),
                            verbose = getOption("Require.verbose", 1L)) {
  if (is.null(scope)) return(invisible())
  atts <- scope$attempts
  if (!length(atts)) return(invisible())

  ## dedupe by (context, exprStr, error) so the same swallow seen at multiple
  ## layers (e.g. evalSUB's block-level retry + evalListElems' per-statement
  ## retry) collapses to one entry
  keys <- vapply(atts, function(r)
    paste(r$context, r$expr,
          if (is.null(r$error)) "" else paste(r$error, collapse = "\n"),
          sep = ""), character(1))
  atts <- atts[!duplicated(keys)]

  errs  <- Filter(function(r) length(r$error), atts)
  wOnly <- Filter(function(r) is.null(r$error) && length(r$warnings), atts)

  if (!length(errs) && !length(wOnly)) return(invisible())

  header <- sprintf("setupProject diagnostics: %d tolerated error%s, %d warning-only",
                    length(errs), if (length(errs) != 1L) "s" else "",
                    length(wOnly))
  message(header)

  pretty <- function(rec, tag) {
    ctx <- if (is.na(rec$context) || !nzchar(rec$context)) "" else paste0(" [", rec$context, "]")
    message(sprintf("  - %s%s: %s", tag, ctx, .diagTrunc(rec$expr)))
    if (length(rec$error))
      for (line in unlist(strsplit(rec$error, "\n", fixed = TRUE)))
        if (nzchar(line)) message("      ", line)
    if (length(rec$warnings))
      for (w in rec$warnings)
        for (line in unlist(strsplit(w, "\n", fixed = TRUE)))
          if (nzchar(line)) message("      warning: ", line)
  }
  for (r in errs)  pretty(r, "tolerated error")
  for (r in wOnly) pretty(r, "warning")

  if (length(errs))
    message("  (run setupProject(...) with options(SpaDES.project.strict = TRUE) to stop on tolerated errors)")
  if (isTRUE(strict) && length(errs)) {
    stop(sprintf("setupProject: %d tolerated error%s (see diagnostics above)",
                 length(errs), if (length(errs) != 1L) "s" else ""),
         call. = FALSE)
  }
  invisible()
}

## Run `expr` with a fresh diagnostic scope, then report. Caller-friendly
## wrapper used by setupProject().
withSetupDiagnostics <- function(expr,
                                 strict = getOption("SpaDES.project.strict", FALSE),
                                 verbose = getOption("Require.verbose", 1L)) {
  scope <- setupDiagOpen()
  on.exit({
    tryCatch(setupDiagReport(scope, strict = strict, verbose = verbose),
             error = function(e) {
               setupDiagClose()
               stop(e)
             })
    setupDiagClose()
  }, add = TRUE)
  force(expr)
}
