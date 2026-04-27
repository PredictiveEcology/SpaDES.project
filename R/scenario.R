#' Scenario records: one canonical form, three representations
#'
#' A "scenario" identifies a single simulation run. The same run can be
#' referred to in three ways:
#'
#'   1. Five field values (typically read from the Google Sheet queue):
#'      `.ELFind`, `.samplingRange`, `.GCM`, `.SSP`, `.rep`.
#'   2. An output directory path:
#'      `outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5`.
#'   3. An upload tar filename:
#'      `6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz`.
#'
#' This file defines a canonical record (an S3 class `"scenario"`) and the
#' generic [as_scenario()] for coercing any of the three representations
#' into it, plus formatters [as_path()] and [as_tarname()] for going back.
#'
#' Column-name flexibility: queue tables sometimes use different column names
#' (e.g. `dotELFind` vs `ELFind` vs something project-specific). The default
#' resolver matches case-insensitively and ignores non-alphanumeric
#' characters; for genuinely different names pass `mapping = c(.ELFind =
#' "myCol", ...)`. Aliases can also be registered globally with
#' [register_scenario_aliases()].
#'
#' @examples
#' \dontrun{
#' s <- scenario(.ELFind = "6.3.1", .samplingRange = 2071:2100,
#'               .GCM = "CNRM-ESM2-1", .SSP = "370", .rep = 5)
#' as_path(s)        # "outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5"
#' as_tarname(s)     # "6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz"
#'
#' as_scenario("outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5")
#' as_scenario("6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz")
#'
#' # GS queue row (column names like dotELFind, dotrep, ...): auto-resolved
#' as_scenario(queueRow)
#'
#' # Dribble from googledrive::drive_ls(): parse the `name` column
#' as_scenario(allOnGS, name_col = "name")
#'
#' # Read the queue (driver) sheet living inside a Drive folder
#' queue <- queueRead(folder = "https://drive.google.com/drive/folders/...",
#'                    name = "longRuns")
#' queueScenarios <- as_scenario(queue)
#'
#' # List uploaded outputs and convert to scenarios in one call
#' uploadScenarios <- outScenarios(.uploadGSdir)
#'
#' # Queue rows whose tar.gz hasn't appeared in the upload folder yet
#' pending <- queuePending(folder = ss_id, name = "longRuns",
#'                         uploadFolder = .uploadGSdir)
#' }
#' @name scenario_family
NULL


# --- queue: the GS sheet that drives simulations ----------------------------

#' Read the driver queue sheet from a Drive folder.
#'
#' Convenience wrapper around `googledrive::drive_ls()` +
#' `googlesheets4::read_sheet()` for the common case where the queue
#' sheet lives inside a shared Drive folder and you only know its name
#' (e.g. `"longRuns"`). The result is passed through [revertDotNames()]
#' so callers see canonical `.ELFind`/`.GCM`/... columns rather than the
#' `dotELFind`/`dotGCM`/... names Google Sheets forces.
#'
#' @param folder    Folder URL or `dribble` of the parent folder.
#' @param name      Spreadsheet name (exact match) within `folder`.
#' @param sheet     Optional worksheet/tab name within the spreadsheet
#'   (passed to `googlesheets4::read_sheet()`).
#' @param col_types Column-types spec for `read_sheet()` (default `"c"`,
#'   read everything as character — matches `.gs_read_queue` and avoids
#'   googlesheets4's type-guessing surprises on mixed columns).
#' @return A `data.table`. Pipe through [as_scenario()] for scenario records.
#' @seealso [queuePending()], [outList()], [outScenarios()]
#' @export
queueRead <- function(folder, name, sheet = NULL, col_types = "c") {
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
  revertDotNames(data.table::as.data.table(q))
}

#' Queue rows whose output hasn't been uploaded yet.
#'
#' Cross-references the driver queue against the upload folder by
#' [as_tarname()] and returns the rows that have no matching tar.gz
#' on the upload side.
#'
#' @param folder       Folder URL of the *queue* (driver) Drive folder.
#' @param name         Queue spreadsheet name within `folder`.
#' @param uploadFolder Folder URL of the *upload* Drive folder containing
#'   the `.tar.gz` outputs.
#' @param ... Extra args forwarded to [queueRead()].
#' @return Subset of the queue data.table containing only pending rows.
#' @seealso [queueRead()], [outList()]
#' @export
queuePending <- function(folder, name, uploadFolder, ...) {
  q          <- queueRead(folder, name, ...)
  uploaded   <- outList(uploadFolder)
  qScens     <- as_scenario(q)
  hasTar     <- as_tarname(qScens) %in% uploaded$name
  q[!hasTar, , drop = FALSE]
}


# --- outs: the GS folder of uploaded .tar.gz outputs ------------------------

#' List uploaded scenario output archives.
#'
#' Thin wrapper over `googledrive::drive_ls()` that filters to files
#' matching `pattern` (defaults to `.tar.gz`).
#'
#' @param folder  Folder URL or `dribble` of the upload folder.
#' @param pattern Regex matched against the `name` column. `NULL`
#'   disables filtering.
#' @return A `dribble` (subclass of tibble) of the matching files.
#' @seealso [outScenarios()], [queuePending()]
#' @export
outList <- function(folder, pattern = "\\.tar\\.gz$") {
  reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
  d <- googledrive::drive_ls(folder)
  if (!is.null(pattern)) d <- d[grepl(pattern, d$name), , drop = FALSE]
  d
}

#' Uploaded outputs as scenario records.
#'
#' Equivalent to `as_scenario(outList(folder, pattern))`.
#'
#' @inheritParams outList
#' @return A list of `scenario` objects.
#' @seealso [outList()], [as_scenario()]
#' @export
outScenarios <- function(folder, pattern = "\\.tar\\.gz$") {
  as_scenario(outList(folder, pattern = pattern))
}


# --- canonical fields & alias registry ---------------------------------------

.scenario_env <- new.env(parent = emptyenv())
.scenario_env$fields <- c(".ELFind", ".samplingRange", ".GCM", ".SSP", ".rep")
.scenario_env$aliases <- list(
  .ELFind        = c(".ELFind",        "ELFind"),
  .samplingRange = c(".samplingRange", "samplingRange", "samplingrange"),
  .GCM           = c(".GCM",           "GCM"),
  .SSP           = c(".SSP",           "SSP"),
  .rep           = c(".rep",           "rep", "replicate")
)
# Note: `dotXxx` columns (used in Google Sheets, which forbids leading ".")
# are handled by [revertDotNames()] before resolution -- not by aliases.

#' Extend the default scenario column-name aliases.
#'
#' @param ... Named arguments. Each name is a canonical field
#'   (e.g. `.ELFind`); each value is a character vector of additional
#'   column-name aliases to recognise.
#' @return The updated alias list (invisibly).
#' @seealso [as_scenario()]
#' @export
register_scenario_aliases <- function(...) {
  new <- list(...)
  for (nm in names(new)) {
    .scenario_env$aliases[[nm]] <- unique(c(.scenario_env$aliases[[nm]], new[[nm]]))
  }
  invisible(.scenario_env$aliases)
}


# --- constructor -------------------------------------------------------------

#' Construct a scenario record.
#'
#' @param .ELFind        Character. Scenario family / experiment label.
#' @param .samplingRange Integer vector, two-element start/end pair, or a
#'   single string like `"1991:2020"` (parsed via `eval(parse(text = ))`).
#' @param .GCM           Character. Global climate model name.
#' @param .SSP           Character or `NA`. Shared Socioeconomic Pathway code.
#' @param .rep           Integer. Replicate number.
#' @return An S3 object of class `"scenario"`.
#' @export
scenario <- function(.ELFind, .samplingRange, .GCM, .SSP = NA_character_, .rep) {
  if (is.character(.samplingRange) && length(.samplingRange) == 1L) {
    .samplingRange <- eval(parse(text = .samplingRange))
  }
  if (length(.samplingRange) == 2L && diff(range(.samplingRange)) > 1) {
    .samplingRange <- as.integer(.samplingRange[1L]):as.integer(.samplingRange[2L])
  }
  structure(
    list(
      .ELFind        = as.character(.ELFind),
      .samplingRange = as.integer(.samplingRange),
      .GCM           = as.character(.GCM),
      .SSP           = if (length(.SSP) == 0L || is.na(.SSP)) NA_character_ else as.character(.SSP),
      .rep           = as.integer(.rep)
    ),
    class = "scenario"
  )
}


# --- generic + methods -------------------------------------------------------

#' Coerce any scenario representation to a canonical record.
#'
#' @param x One of:
#'   * a `scenario` (returned as-is);
#'   * a character path or tarname (parsed via [pathParse()]); vectorised;
#'   * a named list with the 5 fields (or aliases of them);
#'   * a single- or multi-row data.frame / tibble / dribble whose columns
#'     map to the 5 fields, OR which has a `name`/`path`/`tarname` column
#'     holding a path or tarname (auto-detected; force with `name_col`).
#' @param mapping Optional named character vector. Names are canonical
#'   fields (`.ELFind`, ...); values are the actual column names in `x`.
#'   Overrides the alias resolver.
#' @param name_col For data.frame input: column holding a path or tarname
#'   to parse from. If `NULL`, field-mapping is tried first; if that fails
#'   and a `name`/`path`/`tarname`/`file`/`filename` column exists, it is
#'   used.
#' @param ... Unused.
#' @return A `scenario` (single input) or a list of `scenario`s
#'   (vector / multi-row input).
#' @export
as_scenario <- function(x, ...) UseMethod("as_scenario")

#' @export
as_scenario.scenario <- function(x, ...) x

#' @export
as_scenario.character <- function(x, ...) {
  if (length(x) == 0L) return(list())
  if (length(x) == 1L) return(do.call(scenario, pathParse(x)))
  lapply(x, function(xi) do.call(scenario, pathParse(xi)))
}

#' @export
as_scenario.list <- function(x, mapping = NULL, ...) {
  if (length(x) > 0L && all(vapply(x, inherits, logical(1L), "scenario"))) {
    return(x)
  }
  do.call(scenario, .scenario_resolve(x, mapping))
}

#' @export
as_scenario.data.frame <- function(x, mapping = NULL, name_col = NULL, ...) {
  if (!is.null(name_col)) {
    return(as_scenario.character(as.character(x[[name_col]]), ...))
  }
  # Google Sheets forbids leading "." in column names, so the queue uses
  # `dotELFind`, `dotrep`, ... Use the existing helper to undo that
  # before column-name resolution.
  if (any(grepl(paste0("^", dotTxt), names(x)))) {
    x <- revertDotNames(data.table::as.data.table(x))
  }
  rowsToList <- function(i) as.list(x[i, , drop = FALSE])
  parseOne   <- function(rl) {
    res <- tryCatch(.scenario_resolve(rl, mapping), error = function(e) e)
    if (inherits(res, "error")) {
      nameLike <- intersect(c("name", "path", "tarname", "file", "filename"),
                            tolower(names(rl)))
      if (length(nameLike)) {
        col <- names(rl)[match(nameLike[1L], tolower(names(rl)))]
        return(do.call(scenario, pathParse(as.character(rl[[col]]))))
      }
      stop(res)
    }
    do.call(scenario, res)
  }
  if (nrow(x) == 1L) parseOne(rowsToList(1L))
  else lapply(seq_len(nrow(x)), function(i) parseOne(rowsToList(i)))
}


# --- formatters --------------------------------------------------------------

#' Render a scenario (or list of them) as the canonical output path.
#' @param x A scenario, or anything coercible via [as_scenario()].
#' @param pre Path prefix (default `"outputs"`).
#' @export
as_path <- function(x, pre = "outputs") {
  s <- as_scenario(x)
  if (inherits(s, "scenario")) {
    pathBuild(s$.ELFind, s$.samplingRange, s$.GCM, s$.SSP, s$.rep, pre = pre)
  } else {
    vapply(s, as_path, character(1L), pre = pre)
  }
}

#' Render a scenario as an upload tar filename.
#' @param x A scenario, or anything coercible via [as_scenario()].
#' @param ext File extension (default `".tar.gz"`).
#' @export
as_tarname <- function(x, ext = ".tar.gz") {
  s <- as_scenario(x)
  if (inherits(s, "scenario")) {
    runName <- gsub("/", "_", as_path(s, pre = "outputs"))
    runName <- sub("^outputs_", "", runName)
    paste0(runName, ext)
  } else {
    vapply(s, as_tarname, character(1L), ext = ext)
  }
}

#' @export
format.scenario <- function(x, style = c("fields", "path", "tarname"),
                            pre = "outputs", ...) {
  style <- match.arg(style)
  switch(style,
    path    = as_path(x, pre = pre),
    tarname = as_tarname(x),
    fields  = sprintf("%s | %s | %s%s | rep%d",
                      x$.ELFind,
                      paste(range(x$.samplingRange), collapse = "-"),
                      x$.GCM,
                      if (is.na(x$.SSP)) "" else paste0("_ssp", x$.SSP),
                      x$.rep)
  )
}

#' @export
print.scenario <- function(x, ...) {
  cat("<scenario> ", format(x, "fields"), "\n", sep = "")
  cat("  path:    ", as_path(x), "\n", sep = "")
  cat("  tarname: ", as_tarname(x), "\n", sep = "")
  invisible(x)
}


# --- internals: column resolver ---------------------------------------------

.canon_nm <- function(s) tolower(gsub("[^a-z0-9]", "", tolower(s)))

.scenario_resolve <- function(x, mapping = NULL,
                              fields  = .scenario_env$fields,
                              aliases = .scenario_env$aliases) {
  nm <- names(x)
  if (is.null(nm)) stop("scenario input must be named")
  out <- list()
  canonNm <- .canon_nm(nm)
  for (f in fields) {
    if (!is.null(mapping) && f %in% names(mapping)) {
      col <- mapping[[f]]
      if (!col %in% nm) stop("Mapped column '", col, "' not found for ", f)
      out[[f]] <- x[[col]]
      next
    }
    cands <- aliases[[f]] %||% f
    canonCands <- .canon_nm(cands)
    idx <- which(canonNm %in% canonCands)
    if (length(idx) == 0L && f == ".SSP") {
      out[[f]] <- NA_character_
      next
    }
    if (length(idx) == 0L) {
      stop("Cannot resolve column for ", f,
           ". Available columns: ", paste(nm, collapse = ", "),
           ". Pass `mapping = c(`", f, "` = '<col>')` to disambiguate, ",
           "or extend with register_scenario_aliases().")
    }
    out[[f]] <- x[[nm[idx[1L]]]]
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# --- pathBuild / pathParse (the underlying string<->fields converters) ------

#' Build the canonical output path from scenario fields.
#'
#' @inheritParams scenario
#' @param pre Path prefix (default `"outputs"`).
#' @return Character scalar.
#' @export
pathBuild <- function(.ELFind, .samplingRange, .GCM, .SSP, .rep, pre = "outputs") {
  sr <- if (is.numeric(.samplingRange)) .samplingRange else eval(parse(text = .samplingRange))
  file.path(pre, .ELFind,
            paste(range(sr), collapse = "-"),
            paste0(.GCM, ifelse(is.na(.SSP), "", paste0("_ssp", .SSP))),
            paste0("rep", .rep))
}

#' Parse a scenario path or tar filename into its 5 fields.
#'
#' Accepts `outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5` (slash-delimited
#' path, possibly with archive extension stripped) or
#' `6.3.1_2071-2100_CNRM-ESM2-1_ssp370_rep5.tar.gz` (underscore-flattened
#' tarname). Used internally by [as_scenario()].
#'
#' @param path A single character string.
#' @param pre Output prefix that may be present and should be stripped
#'   (default `"outputs"`).
#' @return A named list of the 5 scenario fields.
#' @export
pathParse <- function(path, pre = "outputs") {
  clean <- sub("\\.tar\\.gz$", "", path)
  clean <- sub("\\.(zip|tar|gz|rds|qs)$", "", clean)
  clean <- sub(paste0("^", pre, "[/_]"), "", clean)
  parts <- if (grepl("/", clean)) unlist(strsplit(clean, "/"))
           else                    strsplit(clean, "_")[[1L]]
  repIdx <- which(grepl("^rep\\d+$", parts))
  if (length(repIdx) == 0L) stop("No rep component found in: ", path)
  repIdx <- repIdx[length(repIdx)]
  .rep   <- as.integer(sub("^rep", "", parts[repIdx]))
  rangeIdx <- which(grepl("^\\d+-\\d+$", parts))
  if (length(rangeIdx) == 0L) stop("No sampling range found in: ", path)
  rangeIdx  <- rangeIdx[length(rangeIdx)]
  rangeNums <- as.numeric(strsplit(parts[rangeIdx], "-")[[1L]])
  .samplingRange <- rangeNums[1L]:rangeNums[2L]
  gcmSspStr <- paste(parts[(rangeIdx + 1L):(repIdx - 1L)], collapse = "_")
  if (grepl("_ssp", gcmSspStr)) {
    gs   <- strsplit(gcmSspStr, "_ssp")[[1L]]
    .GCM <- gs[1L]
    .SSP <- gs[2L]
  } else {
    .GCM <- gcmSspStr
    .SSP <- NA_character_
  }
  .ELFind <- paste(parts[seq_len(rangeIdx - 1L)], collapse = "_")
  list(.ELFind = .ELFind, .samplingRange = .samplingRange,
       .GCM = .GCM, .SSP = .SSP, .rep = .rep)
}
