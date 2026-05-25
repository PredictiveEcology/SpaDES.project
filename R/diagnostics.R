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
##   - `evalSUB` / `evalListElems` push records onto the current scope, BUT
##     only after they know the *final* outcome (so an expected first-try
##     failure recovered by a fallback is not reported as an error)
##   - `setupDiagReport()` summarises records at the end of setupProject
##   - `options(SpaDES.project.strict = TRUE)` escalates the summary to a stop
##
## Recording the final outcome (rather than every internal try) is the trick
## that avoids drowning the report in expected-fallback noise; the report still
## dedupes by deparsed expression in case the same expression bubbles through
## both evalSUB and evalListElems.

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

## Record one resolved attempt. Callers must only call this AFTER the final
## outcome of their try-ladder is known -- so `recovered = TRUE` actually means
## "the eval eventually succeeded" and `recovered = FALSE` means "all paths
## failed; the value handed back was the unevaluated input."
##
## - `context`   short tag (`"sideEffects"`, `"modules"`, ...); use NA if unknown
## - `expr`      the original (unevaluated) expression the caller was resolving
## - `error`     try-error or condition collected from the failing path; NULL if
##               the only thing of note was a warning
## - `warnings`  warning messages collected via withCallingHandlers
## - `recovered` TRUE if a fallback path produced a usable value
setupDiagRecord <- function(context = NA_character_, expr, error = NULL,
                            warnings = NULL, recovered = FALSE) {
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
    warnings = if (length(warnings)) as.character(warnings) else NULL,
    recovered = isTRUE(recovered)
  )
  invisible()
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
          sep = ""), character(1))
  atts <- atts[!duplicated(keys)]

  hard <- Filter(function(r) length(r$error) && !r$recovered, atts)
  soft <- Filter(function(r) length(r$error) &&  r$recovered, atts)
  wOnly <- Filter(function(r) is.null(r$error) && length(r$warnings), atts)

  if (!length(hard) && !length(soft) && !length(wOnly)) return(invisible())

  header <- sprintf(
    "setupProject diagnostics: %d unrecovered, %d recovered, %d warning-only",
    length(hard), length(soft), length(wOnly))
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
  for (r in hard)  pretty(r, "ERROR")
  for (r in soft)  pretty(r, "recovered")
  for (r in wOnly) pretty(r, "warning")

  if (length(hard))
    message("  (run setupProject(...) with options(SpaDES.project.strict = TRUE) to stop on unrecovered errors)")
  if (isTRUE(strict) && length(hard)) {
    stop(sprintf("setupProject: %d unrecovered error(s) (see diagnostics above)",
                 length(hard)),
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
