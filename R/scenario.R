#' Scenario records: one canonical form, multiple representations
#'
#' A "scenario" identifies a single simulation run. Field *names* and
#' *values* are discovered from the driver queue (Google Sheet); they
#' are not hardcoded in this package. The same run can be referred to
#' in three interchangeable ways:
#'
#'   1. Field values (one column per field in the queue), e.g.
#'      `(.ELFind = "6.3.1", .samplingRange = 2071:2100, ...)`.
#'   2. An output directory path under `outputs/`.
#'   3. An upload tar filename (path with `/` -> `_` and `.tar.gz` suffix).
#'
#' This file defines:
#'
#'   * a canonical record (S3 class `"scenario"`);
#'   * the generic [as_scenario()] for coercing any representation into it;
#'   * formatters [as_path()] / [as_tarname()] for going back;
#'   * default builders [pathBuild()] / [pathParse()]: each non-empty
#'     field's *value* (no label) is one path segment, joined by `/`,
#'     in the order given by [scenarioFields()]. Integer-and-contiguous
#'     vectors render as `start-end`. Empty / `NA` fields are skipped
#'     entirely (one fewer segment); see [pathParse()] for the
#'     trailing-NA round-trip caveat.
#'
#' Per-project format overrides: define your own `pathBuild` (and
#' matching `pathParse`) in the global environment, or register them
#' explicitly with [register_scenario_format()]. Lookup order, highest
#' first: `register_scenario_format` slot -> a `pathBuild`/`pathParse`
#' in the global environment -> the package default.
#'
#' Field discovery: [queueRead()] caches the queue's non-meta column
#' names as the active field set. Subsequent [pathParse()] calls use
#' those labels for positional decoding. If you parse paths without
#' first reading a queue, pass `fields = c(...)` explicitly (or call
#' [scenarioFieldsSet()]).
#'
#' @examples
#' \dontrun{
#' ## --- Default (generic) format -----------------------------------------
#' queue <- queueRead(folder = ss_id, name = "longRuns")
#' #  -> data.table with columns .ELFind, .samplingRange, .GCM, .SSP, .rep
#' #     plus meta columns (status, started_at, ...). Non-meta columns are
#' #     auto-cached as scenarioFields().
#'
#' scens <- as_scenario(queue)                # list of `scenario` objects
#' as_path(scens[[1]])
#' #> "outputs/6.3.1/2071-2100/CNRM-ESM2-1/370/5"
#' as_tarname(scens[[1]])
#' #> "6.3.1_2071-2100_CNRM-ESM2-1_370_5.tar.gz"
#'
#' # Round-trip
#' s2 <- as_scenario("outputs/6.3.1/2071-2100/CNRM-ESM2-1/370/5")
#' identical(unclass(scens[[1]]), unclass(s2))   # TRUE
#'
#' # Cross-reference queue against uploaded tarballs
#' uploads <- outScenarios(.uploadGSdir)         # list of scenarios
#' missing <- queueUploadMissing(folder = ss_id, name = "longRuns",
#'                               uploadFolder = .uploadGSdir)  # queue rows only
#'
#' ## --- Per-field labels in the path -------------------------------------
#' # `withFieldLabel` accepts two forms.
#'
#' # 1) Unnamed character vector: prefix with the field name itself.
#' as_path(scens[[1]], withFieldLabel = c(".rep", ".SSP"))
#' #> "outputs/6.3.1/2071-2100/CNRM-ESM2-1/.SSP370/.rep5"
#'
#' # 2) Named character vector: prefix with the *mapped* label
#' #    (e.g., emit `.rep` as `rep`, `.SSP` as `_ssp`).
#' as_path(scens[[1]], withFieldLabel = c(.rep = "rep", .SSP = "_ssp"))
#' #> "outputs/6.3.1/2071-2100/CNRM-ESM2-1/_ssp370/rep5"
#'
#' # Set once for every subsequent as_path() / as_tarname():
#' register_scenario_format(withFieldLabel = c(.rep = "rep", .SSP = "_ssp"))
#' as_path(scens[[1]])
#' #> "outputs/6.3.1/2071-2100/CNRM-ESM2-1/_ssp370/rep5"
#' as_tarname(scens[[1]])
#' #> "6.3.1_2071-2100_CNRM-ESM2-1__ssp370_rep5.tar.gz"
#' # Round-trip parses back to canonical fields:
#' as_scenario("outputs/6.3.1/2071-2100/CNRM-ESM2-1/_ssp370/rep5")
#'
#' ## --- Project-specific format (FireSenseTesting layout) ----------------
#' # Layout: outputs/<.ELFind>/<range>/<GCM>_ssp<SSP>/rep<.rep>
#' # E.g.    outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5
#'
#' myBuild <- function(.ELFind, .samplingRange, .GCM, .SSP, .rep,
#'                     pre = "outputs") {
#'   sr <- if (is.numeric(.samplingRange)) .samplingRange
#'         else                            eval(parse(text = .samplingRange))
#'   file.path(pre, .ELFind,
#'             paste(range(sr), collapse = "-"),
#'             paste0(.GCM, ifelse(is.na(.SSP), "", paste0("_ssp", .SSP))),
#'             paste0("rep", .rep))
#' }
#'
#' myParse <- function(path, fields = scenarioFields(), pre = "outputs") {
#'   clean <- sub("\\.tar\\.gz$", "", path)
#'   clean <- sub(paste0("^", pre, "[/_]"), "", clean)
#'   parts <- if (grepl("/", clean)) strsplit(clean, "/")[[1L]]
#'            else                    strsplit(clean, "_")[[1L]]
#'   repIdx   <- which(grepl("^rep[0-9]+$",   parts))
#'   rangeIdx <- which(grepl("^[0-9]+-[0-9]+$", parts))
#'   gcmSsp   <- paste(parts[(rangeIdx + 1L):(repIdx - 1L)], collapse = "_")
#'   gs       <- if (grepl("_ssp", gcmSsp)) strsplit(gcmSsp, "_ssp")[[1L]]
#'               else                       c(gcmSsp, NA_character_)
#'   rng      <- as.integer(strsplit(parts[rangeIdx], "-")[[1L]])
#'   list(.ELFind        = paste(parts[seq_len(rangeIdx - 1L)], collapse = "_"),
#'        .samplingRange = rng[1L]:rng[2L],
#'        .GCM           = gs[1L],
#'        .SSP           = gs[2L],
#'        .rep           = as.integer(sub("^rep", "", parts[repIdx])))
#' }
#'
#' register_scenario_format(build = myBuild, parse = myParse)
#' as_path(scens[[1]])
#' #> "outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5"
#' as_tarname(scens[[1]])
#' #> "6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz"
#'
#' # Equivalent: define pathBuild / pathParse in your global environment
#' # (e.g. in a project global.R) -- they will be auto-detected.
#' pathBuild <- myBuild
#' pathParse <- myParse
#' }
#' @name scenario_family
NULL


# --- queue: the GS sheet that drives simulations ----------------------------

#' Read the driver queue (local RDS or Google Sheet).
#'
#' Two call shapes:
#'
#' \describe{
#'   \item{Local: `queueRead("path/to/queue.rds")`}{When the first
#'     argument is an existing local `.rds` file and `name` is not
#'     supplied, the queue is loaded via `readRDS()`. Useful for the
#'     file-backed queues written by `experimentTmux()` /
#'     `experimentFuture()` / `experimentSBATCH()` when no `ss_id` was
#'     supplied.}
#'   \item{Google Sheet: `queueRead(folder, name)`}{Convenience wrapper
#'     around `googledrive::drive_ls()` + `googlesheets4::read_sheet()`.
#'     `folder` is the Drive folder URL/id, `name` is the spreadsheet
#'     name within it.}
#' }
#'
#' Either way the result is passed through `revertDotNames()` so callers
#' see canonical `.ELFind`/`.GCM`/... column names rather than the
#' `dotELFind`/`dotGCM`/... names Google Sheets forces. As a side
#' effect, the non-meta column names are cached as the active scenario
#' field set (see [scenarioFields()]).
#'
#' @param folder    Either a local path to an `.rds` queue file (when
#'   `name` is missing), or a Drive folder URL / `dribble` of the
#'   parent folder containing the queue spreadsheet.
#' @param name      Spreadsheet name (exact match) within `folder`.
#'   Omit for local-RDS reads.
#' @param sheet     Optional worksheet/tab name (passed to `read_sheet()`).
#'   Ignored for local-RDS reads.
#' @param col_types Column-types spec for `read_sheet()` (default `"c"`).
#'   Ignored for local-RDS reads.
#' @return A `data.table`. Pipe through [as_scenario()] for scenario records.
#' @seealso [queueUploadMissing()], [outList()], [outScenarios()],
#'   [experimentFuture()], [experimentTmux()], [experimentSBATCH()]
#' @export
queueRead <- function(folder, name, sheet = NULL, col_types = "c") {
  # Local-RDS shortcut: queueRead("path/to/queue.rds")
  if (missing(name) && is.character(folder) && length(folder) == 1L &&
      file.exists(folder) && grepl("\\.rds$", folder, ignore.case = TRUE)) {
    q <- readRDS(folder)
    q <- revertDotNames(data.table::as.data.table(q))
    scenarioFieldsSet(.discoverFields(q))
    return(q)
  }

  # Google Sheet path
  reproducible::.requireNamespace("googledrive",    stopOnFALSE = TRUE)
  reproducible::.requireNamespace("googlesheets4",  stopOnFALSE = TRUE)
  sheets <- googledrive::drive_ls(folder, type = "spreadsheet")
  hit    <- sheets[sheets$name == name, , drop = FALSE]
  if (nrow(hit) == 0L)
    stop("No spreadsheet named '", name, "' in folder. Found: ",
         paste(sheets$name, collapse = ", "))
  if (nrow(hit) > 1L)
    stop("Multiple spreadsheets named '", name, "' in folder; ",
         "disambiguate via id.")
  args <- list(ss = hit$id[[1L]], col_types = col_types)
  if (!is.null(sheet)) args$sheet <- sheet
  q <- suppressMessages(do.call(googlesheets4::read_sheet, args))
  q <- revertDotNames(data.table::as.data.table(q))
  scenarioFieldsSet(.discoverFields(q))
  q
}

#' Queue rows whose tarball is missing from the upload folder.
#'
#' Anti-join of the driver queue against the upload folder's `.tar.gz`
#' listing, keyed on rendered tarname (see [as_tarname()]). Independent
#' of the queue's `status` column.
#'
#' @param folder       Folder URL of the *queue* (driver) Drive folder.
#' @param name         Queue spreadsheet name within `folder`.
#' @param uploadFolder Folder URL of the *upload* Drive folder.
#' @param ...          Extra args forwarded to [queueRead()].
#' @return Subset of the queue data.table for rows whose expected tarball
#'   is not present in `uploadFolder`.
#' @seealso [queueRead()], [outList()]
#' @export
queueUploadMissing <- function(folder, name, uploadFolder, ...) {
  q        <- queueRead(folder, name, ...)
  uploaded <- outList(uploadFolder)
  qScens   <- as_scenario(q)
  hasTar   <- as_tarname(qScens) %in% uploaded$name
  q[!hasTar, , drop = FALSE]
}


# --- outs: the GS folder of uploaded .tar.gz outputs ------------------------

#' List uploaded scenario output archives.
#'
#' @param folder  Folder URL or `dribble` of the upload folder.
#' @param pattern Regex matched against the `name` column. `NULL`
#'   disables filtering.
#' @return A `dribble` of the matching files.
#' @seealso [outScenarios()], [queueUploadMissing()]
#' @export
outList <- function(folder, pattern = "\\.tar\\.gz$") {
  reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
  d <- googledrive::drive_ls(folder)
  if (!is.null(pattern)) d <- d[grepl(pattern, d$name), , drop = FALSE]
  d
}

#' Uploaded outputs as scenario records.
#' @inheritParams outList
#' @return A list of `scenario` objects.
#' @seealso [outList()], [as_scenario()]
#' @export
outScenarios <- function(folder, pattern = "\\.tar\\.gz$") {
  as_scenario(outList(folder, pattern = pattern))
}


# --- field discovery & format-override registry ----------------------------

.scenario_env <- new.env(parent = emptyenv())
.scenario_env$fields         <- NULL              # cached non-meta column names
.scenario_env$build          <- NULL              # explicit pathBuild override
.scenario_env$parse          <- NULL              # explicit pathParse override
.scenario_env$withFieldLabel <- character(0L)     # fields whose value is prefixed
                                                  #   with the field name in path

#' Active scenario field labels.
#'
#' Returns the field labels currently used to (a) parse paths/tarnames
#' back into scenario records and (b) determine which queue columns
#' constitute the scenario (vs. queue meta-columns). Set automatically by
#' [queueRead()]; can be set manually with [scenarioFieldsSet()].
#'
#' @return Character vector of field names, or `NULL` if not yet known.
#' @export
scenarioFields <- function() .scenario_env$fields

#' @rdname scenarioFields
#' @param fields Character vector of field labels.
#' @export
scenarioFieldsSet <- function(fields) {
  .scenario_env$fields <- fields
  invisible(fields)
}

#' Register a project-specific path builder / parser.
#'
#' Pass a function (or, for `withFieldLabel`, a character vector) to
#' register it; pass `NULL` explicitly to clear that slot; omit the
#' argument to leave it untouched. Call with no arguments to inspect.
#'
#' Lookup precedence (highest first): registered slot ->
#' `pathBuild`/`pathParse` defined in the global environment -> the
#' package defaults.
#'
#' Override signature contract:
#'   `build(..., pre = "outputs")` --- receives the scenario as named
#'     `...` args (one per field) plus `pre`; returns a path string.
#'   `parse(path, pre = "outputs")` --- returns a named list of fields.
#'
#' @param build Function (custom path builder), or `NULL` to clear.
#' @param parse Function (custom path parser), or `NULL` to clear.
#' @param withFieldLabel Either:
#'   * a *character vector* of field names (e.g. `c(".rep", ".SSP")`)
#'     -- those fields' path segments get prefixed with the field name
#'     itself (`.rep5`, `.SSP370`); or
#'   * a *named character vector* mapping field name to display label
#'     (e.g. `c(.rep = "rep", .SSP = "_ssp")`) -- those fields' path
#'     segments get prefixed with the *mapped* label (`rep5`, `_ssp370`).
#'   Pass `character(0)` or `NULL` to clear.
#' @return Invisibly, the current `(build, parse, withFieldLabel)` triple.
#' @export
register_scenario_format <- function(build, parse, withFieldLabel) {
  if (!missing(build)) .scenario_env$build <- build
  if (!missing(parse)) .scenario_env$parse <- parse
  if (!missing(withFieldLabel)) {
    .scenario_env$withFieldLabel <- if (is.null(withFieldLabel)) {
      character(0L)
    } else {
      # Preserve names (named-vector form encodes field -> label map).
      setNames(as.character(withFieldLabel), names(withFieldLabel))
    }
  }
  invisible(list(build          = .scenario_env$build,
                 parse          = .scenario_env$parse,
                 withFieldLabel = .scenario_env$withFieldLabel))
}

# Meta columns shared with tmux.R; defined here defensively in case scenario.R
# is sourced standalone (the package definition wins at runtime).
.scenario_default_meta <- c(
  "status", "claimed_by", "started_at", "finished_at",
  "DEoptimElapsedTime", "machine_name", "process_id",
  "heartbeat_at", "heartbeat_iter", "iterationsTotal", "interrupted_at"
)

.discoverFields <- function(df) {
  meta <- if (exists("meta_cols", inherits = TRUE, mode = "character"))
    get("meta_cols") else .scenario_default_meta
  setdiff(names(df), meta)
}


# --- constructor -------------------------------------------------------------

#' Construct a scenario record.
#'
#' Accepts any named arguments. Each name becomes a field label; each
#' value the field's value. No specific field set is required by the
#' package (fields are project-defined via the queue).
#'
#' Light coercion: a single character of the form `"a:b"` is `eval`ed
#' as an R expression (so queue cells like `"1991:2020"` become integer
#' vectors).
#'
#' @param ... Named field/value pairs.
#' @return An S3 object of class `"scenario"`.
#' @export
scenario <- function(...) {
  args <- list(...)
  if (length(args) == 0L) stop("scenario(): supply at least one named field")
  if (is.null(names(args)) || any(!nzchar(names(args))))
    stop("scenario(): all arguments must be named")
  for (nm in names(args)) {
    v <- args[[nm]]
    if (is.character(v) && length(v) == 1L && grepl("^[0-9]+:[0-9]+$", v)) {
      args[[nm]] <- eval(parse(text = v))
    }
  }
  structure(args, class = "scenario")
}


# --- generic + methods -------------------------------------------------------

#' Coerce any scenario representation to a canonical record.
#'
#' @param x   A scenario, a character path/tarname, a named list, or a
#'   data.frame / tibble / dribble.
#' @param mapping Optional named character vector renaming columns:
#'   `c(<canonical> = <actual>)`.
#' @param name_col For data.frame input: column holding a path or tarname.
#'   If `NULL`, field columns are used; if no field columns are found and
#'   a `name`/`path`/`tarname`/`file`/`filename` column exists, that is
#'   used as a fallback.
#' @param fields Optional character vector of field labels to extract
#'   (overrides the cached set).
#' @param ... Unused.
#' @return A `scenario` (single input) or list of `scenario`s.
#' @export
as_scenario <- function(x, ...) UseMethod("as_scenario")

#' @export
as_scenario.scenario <- function(x, ...) x

#' @export
as_scenario.character <- function(x, withFieldLabel = .scenario_env$withFieldLabel,
                                  ...) {
  if (length(x) == 0L) return(list())
  parseOne <- function(xi)
    do.call(scenario, .runParse(xi, withFieldLabel = withFieldLabel))
  if (length(x) == 1L) parseOne(x) else lapply(x, parseOne)
}

#' @export
as_scenario.list <- function(x, mapping = NULL, fields = NULL, ...) {
  if (length(x) > 0L && all(vapply(x, inherits, logical(1L), "scenario"))) {
    return(x)
  }
  do.call(scenario, .scenarioResolve(x, mapping = mapping, fields = fields))
}

#' @export
as_scenario.data.frame <- function(x, mapping = NULL, name_col = NULL,
                                   fields = NULL, ...) {
  if (!is.null(name_col)) {
    return(as_scenario.character(as.character(x[[name_col]]), ...))
  }
  if (any(grepl(paste0("^", dotTxt), names(x)))) {
    x <- revertDotNames(data.table::as.data.table(x))
  }
  effFields <- fields %||% scenarioFields()
  isQueue   <- !is.null(effFields) && any(effFields %in% names(x))
  if (!isQueue) {
    nameLike <- intersect(c("name", "path", "tarname", "file", "filename"),
                          tolower(names(x)))
    if (length(nameLike)) {
      col <- names(x)[match(nameLike[1L], tolower(names(x)))]
      return(as_scenario.character(as.character(x[[col]]), ...))
    }
    if (is.null(effFields)) {
      effFields <- intersect(.discoverFields(x), names(x))
      if (length(effFields)) scenarioFieldsSet(effFields)
    }
  } else if (is.null(scenarioFields())) {
    scenarioFieldsSet(effFields)
  }
  rowsToList <- function(i) as.list(x[i, , drop = FALSE])
  parseOne   <- function(rl) {
    do.call(scenario, .scenarioResolve(rl, mapping = mapping, fields = effFields))
  }
  if (nrow(x) == 1L) parseOne(rowsToList(1L))
  else lapply(seq_len(nrow(x)), function(i) parseOne(rowsToList(i)))
}


# --- formatters --------------------------------------------------------------

#' Render a scenario (or list of them) as the canonical output path.
#' @param x              A scenario, or anything coercible via [as_scenario()].
#' @param pre            Path prefix (default `"outputs"`).
#' @param withFieldLabel Character vector of field names whose value
#'   should be prefixed with the field name in the path
#'   (`paste0(label, value)` instead of bare `value`). Defaults to the
#'   value registered via [register_scenario_format()]; pass
#'   `character(0)` to force the bare-value default.
#' @export
as_path <- function(x, pre = "outputs",
                    withFieldLabel = .scenario_env$withFieldLabel) {
  s <- as_scenario(x)
  if (inherits(s, "scenario")) {
    fn    <- .getPathBuild()
    extra <- if ("withFieldLabel" %in% names(formals(fn)))
      list(withFieldLabel = withFieldLabel) else list()
    do.call(fn, c(unclass(s), list(pre = pre), extra))
  } else {
    vapply(s, as_path, character(1L), pre = pre,
           withFieldLabel = withFieldLabel)
  }
}

#' Render a scenario as an upload tar filename.
#' @inheritParams as_path
#' @param ext File extension (default `".tar.gz"`).
#' @export
as_tarname <- function(x, ext = ".tar.gz", pre = "outputs",
                       withFieldLabel = .scenario_env$withFieldLabel) {
  s <- as_scenario(x)
  if (inherits(s, "scenario")) {
    runName <- gsub("/", "_",
                    as_path(s, pre = pre, withFieldLabel = withFieldLabel))
    runName <- sub(paste0("^", pre, "_"), "", runName)
    paste0(runName, ext)
  } else {
    vapply(s, as_tarname, character(1L), ext = ext, pre = pre,
           withFieldLabel = withFieldLabel)
  }
}

#' @export
format.scenario <- function(x, style = c("fields", "path", "tarname"),
                            pre = "outputs", ...) {
  style <- match.arg(style)
  switch(style,
    path    = as_path(x, pre = pre),
    tarname = as_tarname(x, pre = pre),
    fields  = paste(vapply(names(x),
                           function(nm) paste0(nm, "=", .formatVal(x[[nm]])),
                           character(1L)), collapse = " | ")
  )
}

#' @export
print.scenario <- function(x, ...) {
  cat("<scenario> ", format(x, "fields"), "\n", sep = "")
  cat("  path:    ", as_path(x), "\n", sep = "")
  cat("  tarname: ", as_tarname(x), "\n", sep = "")
  invisible(x)
}


# --- default pathBuild / pathParse (generic, no project knowledge) ---------

#' Default scenario path builder.
#'
#' Generic format: each non-empty field's value becomes one path segment.
#' Field order is taken from the order of the input (or, for positional
#' calls, from [scenarioFields()]). Integer-and-contiguous vectors are
#' encoded as `start-end`. Empty / `NA` fields are dropped entirely
#' (yielding one fewer segment); for round-tripping see [pathParse()].
#'
#' Fields whose name appears in `withFieldLabel` get their segment
#' prefixed by the field name itself (e.g. `.rep` with value `5`
#' renders as `.rep5` instead of bare `5`). Useful when path readers
#' must distinguish two integer fields, or when round-tripping with
#' mid-list `NA`s (the label disambiguates which segments are present).
#'
#' Accepts three calling styles, all equivalent:
#'   * `pathBuild(scenarioObj)` --- a single named-list / scenario;
#'   * `pathBuild(.fieldA = vA, .fieldB = vB, ...)` --- explicit named args;
#'   * `pathBuild(vA, vB, ...)` --- positional, in cached-field order.
#'
#' @param ... Either a single named-list / scenario, or named/positional
#'   field/value pairs.
#' @param pre Path prefix (default `"outputs"`).
#' @param withFieldLabel Character vector of field names whose segment
#'   should carry a `paste0(label, value)` prefix. Defaults to the
#'   registered value (see [register_scenario_format()]).
#' @return Character scalar.
#' @export
pathBuild <- function(..., pre = "outputs",
                     withFieldLabel = .scenario_env$withFieldLabel) {
  args <- list(...)
  if (length(args) == 1L && is.list(args[[1L]]) &&
      !is.null(names(args[[1L]]))) {
    s <- args[[1L]]
  } else if (!is.null(names(args)) && all(nzchar(names(args)))) {
    s <- args
  } else {
    flds <- scenarioFields()
    if (is.null(flds) || length(flds) != length(args)) {
      # Positional call with no usable cache -- try to recover field
      # names from the caller's syntax: pathBuild(.ELFind, .samplingRange,
      # ..., .rep) -> use those symbols as field names. Only kicks in
      # when *every* dot-arg is a bare symbol; literals keep the original
      # cached-fields requirement.
      dotExprs <- match.call(expand.dots = FALSE)$`...`
      if (!is.null(dotExprs) &&
          length(dotExprs) == length(args) &&
          all(vapply(dotExprs, is.symbol, logical(1L)))) {
        flds <- vapply(dotExprs, as.character, character(1L))
        scenarioFieldsSet(flds)         # cache for subsequent calls
      } else {
        stop("pathBuild(): positional call needs cached fields ",
             "(via queueRead) of matching length, or bare-symbol ",
             "arguments to infer them. Got ", length(args),
             " value(s); cached fields = ",
             if (is.null(flds)) "<none>" else paste(flds, collapse = ", "))
      }
    }
    s <- setNames(args, flds)
  }
  labelMap <- .normalizeLabelMap(withFieldLabel)
  parts <- character(0L)
  for (nm in names(s)) {
    v <- s[[nm]]
    if (.isEmptyVal(v)) next
    seg <- .encodeVal(v)
    if (nm %in% names(labelMap)) seg <- paste0(labelMap[[nm]], seg)
    parts <- c(parts, seg)
  }
  do.call(file.path, c(list(pre), as.list(parts)))
}

#' Default scenario path parser.
#'
#' Inverse of the default [pathBuild()]: splits the path on `/` (or, for
#' tarname inputs, on `_`), strips archive extensions and the `pre`
#' prefix, then matches segments **positionally** to [scenarioFields()].
#' Integer ranges of the form `start-end` decode to integer vectors.
#'
#' Without per-segment labels there is no way to recover which field a
#' missing segment corresponds to, so when the path has fewer segments
#' than there are fields, the *trailing* fields are treated as `NA`.
#' Round-trip is therefore only lossless when `NA`-bearing fields are
#' last in the field order *unless* you label the ambiguous fields
#' through `withFieldLabel`: any field named there has its label
#' prefix stripped from the segment, and segments not starting with a
#' labeled field's name are assigned positionally to the next
#' unlabeled field. With every potentially-`NA` field labeled, mid-list
#' `NA`s round-trip cleanly.
#'
#' @param path   A single character string (path or tarname).
#' @param fields Field labels in scenario order; defaults to
#'   [scenarioFields()] (set by [queueRead()]).
#' @param pre    Path prefix to strip (default `"outputs"`).
#' @param withFieldLabel Character vector of field names that were
#'   built with `paste0(label, value)` prefixing.
#' @return Named list of field values (in `fields` order).
#' @export
pathParse <- function(path, fields = scenarioFields(), pre = "outputs",
                     withFieldLabel = .scenario_env$withFieldLabel) {
  if (is.null(fields))
    stop("pathParse(): no fields known. Call queueRead() first ",
         "or pass `fields = c(...)`.")
  labelMap <- .normalizeLabelMap(withFieldLabel)
  clean <- sub("\\.tar\\.gz$", "", path)
  clean <- sub("\\.(zip|tar|gz|rds|qs)$", "", clean)
  clean <- sub(paste0("^", pre, "[/_]"), "", clean)
  parts <- if (grepl("/", clean)) unlist(strsplit(clean, "/"))
           else                    strsplit(clean, "_")[[1L]]
  out <- setNames(rep(list(NA), length(fields)), fields)
  if (length(labelMap) == 0L) {
    if (length(parts) > length(fields))
      stop("pathParse(): path has ", length(parts), " segment(s) but only ",
           length(fields), " field(s) cached: ",
           paste(fields, collapse = ", "), ". Path: ", path)
    for (i in seq_along(parts)) out[[fields[i]]] <- .decodeVal(parts[i])
    return(out)
  }
  # Label-aware parsing: walk segments left-to-right, consume labeled
  # fields by their (possibly remapped) prefix and unlabeled fields
  # positionally.
  labeled  <- intersect(fields, names(labelMap))
  unlabIdx <- which(!fields %in% names(labelMap))
  # Order labels by descending nchar so prefix overlaps disambiguate.
  byLen <- labeled[order(-nchar(labelMap[labeled]))]
  nextUnlab <- 1L
  for (seg in parts) {
    hit <- NULL
    for (cand in byLen) {
      if (startsWith(seg, labelMap[[cand]])) { hit <- cand; break }
    }
    if (!is.null(hit)) {
      raw <- substr(seg, nchar(labelMap[[hit]]) + 1L, nchar(seg))
      out[[hit]] <- .decodeVal(raw)
    } else {
      if (nextUnlab > length(unlabIdx))
        stop("pathParse(): segment '", seg, "' has no recognised label ",
             "and no unlabeled field left to claim it. Path: ", path)
      out[[fields[unlabIdx[nextUnlab]]]] <- .decodeVal(seg)
      nextUnlab <- nextUnlab + 1L
    }
  }
  out
}


# --- internals --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

.canon_nm <- function(s) tolower(gsub("[^a-z0-9]", "", tolower(s)))

# Accept either form for `withFieldLabel`:
#   c(".rep", ".SSP")            -> labels = field names themselves
#   c(.rep = "rep", .SSP = "_ssp")-> labels = the named values
# Returns a named character vector (names = field, value = label),
# or character(0) for null/empty input.
.normalizeLabelMap <- function(x) {
  if (is.null(x) || length(x) == 0L) return(setNames(character(0L), character(0L)))
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    if (!is.null(names(x)) && any(nzchar(names(x))))
      stop(".normalizeLabelMap: mix of named and unnamed entries; ",
           "use either all-unnamed (label = field name) or all-named ",
           "(c(field = 'label')).")
    return(setNames(as.character(x), as.character(x)))
  }
  setNames(as.character(x), names(x))
}

.scenarioResolve <- function(x, mapping = NULL, fields = NULL) {
  nm <- names(x)
  if (is.null(nm)) stop("scenario input must be named")
  if (is.null(fields)) fields <- scenarioFields()
  # If still no fields, fall back to "everything that's named is a field"
  if (is.null(fields)) fields <- nm
  out <- list()
  canonNm <- .canon_nm(nm)
  for (f in fields) {
    if (!is.null(mapping) && f %in% names(mapping)) {
      col <- mapping[[f]]
      if (!col %in% nm) stop("Mapped column '", col, "' not found for ", f)
      out[[f]] <- x[[col]]
      next
    }
    idx <- which(canonNm == .canon_nm(f))
    if (length(idx) == 0L) {
      out[[f]] <- NA
      next
    }
    out[[f]] <- x[[nm[idx[1L]]]]
  }
  out
}

.isEmptyVal <- function(v) {
  if (length(v) == 0L) return(TRUE)
  if (all(is.na(v)))   return(TRUE)
  if (is.character(v) && all(!nzchar(v))) return(TRUE)
  FALSE
}

.encodeVal <- function(v) {
  # numeric, multi-element, all-integer, contiguous -> "start-end"
  if (is.numeric(v) && length(v) > 1L) {
    if (all(v == suppressWarnings(as.integer(v))) &&
        all(diff(sort(v)) == 1L)) {
      return(paste0(min(v), "-", max(v)))
    }
    return(paste(v, collapse = "_"))
  }
  if (length(v) > 1L) return(paste(v, collapse = "_"))
  as.character(v)
}

.decodeVal <- function(raw) {
  if (grepl("^-?[0-9]+-[0-9]+$", raw)) {
    rng <- as.integer(strsplit(raw, "-")[[1L]])
    return(rng[1L]:rng[2L])
  }
  if (grepl("^-?[0-9]+$", raw)) return(as.integer(raw))
  raw
}

.formatVal <- function(v) {
  if (.isEmptyVal(v)) return("")
  .encodeVal(v)
}

.getPathBuild <- function() {
  if (!is.null(.scenario_env$build)) return(.scenario_env$build)
  if (exists("pathBuild", envir = globalenv(), mode = "function",
             inherits = FALSE))
    return(get("pathBuild", envir = globalenv(), mode = "function",
               inherits = FALSE))
  pathBuild
}

.getPathParse <- function() {
  if (!is.null(.scenario_env$parse)) return(.scenario_env$parse)
  if (exists("pathParse", envir = globalenv(), mode = "function",
             inherits = FALSE))
    return(get("pathParse", envir = globalenv(), mode = "function",
               inherits = FALSE))
  pathParse
}

.runParse <- function(path, withFieldLabel = .scenario_env$withFieldLabel) {
  fn   <- .getPathParse()
  fmls <- names(formals(fn))
  args <- list(path = path)
  if ("fields"         %in% fmls) args$fields         <- scenarioFields()
  if ("withFieldLabel" %in% fmls) args$withFieldLabel <- withFieldLabel
  do.call(fn, args)
}
