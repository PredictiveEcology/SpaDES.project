#' Inventory of the external data a set of modules reads
#'
#' Lists every external dataset (a URL) that a set of `SpaDES` modules reads, one row per
#' dataset URL and module, so that a project can see which modules use which data and which
#' data each module needs. It looks at three layers: what the modules declare, what their
#' code does, and, if a finished run is given, what the run downloaded.
#'
#' @details
#' ## Layer 1: metadata
#' Every `expectsInput(..., sourceURL = )` in a module's metadata, as read by
#' [SpaDES.core::moduleMetadata()], is a row (a `sourceURL` that is `NA` or empty is skipped).
#' `role` is `"fallback"` when another module in `modules` lists the same object name in
#' `createsOutput`, because that is what `suppliedElsewhere()` tests at run time: the
#' module's own download then only runs if no other module supplies the object. Otherwise
#' `role` is `"default"`. The metadata cannot know a parameter-dependent choice (for example
#' `dataSource = "KNN"` against `"SCANFI"`), so a `"default"` row is the declared source,
#' not necessarily the one a given run uses.
#'
#' ## Layer 2: code
#' A static scan of each module's `.R` files (the module file and its `R/` folder, not
#' `tests/`) with the R parser ([utils::getParseData()]) for calls to `prepInputs`,
#' `preProcess`, `download.file`, `googledrive::drive_download`, `httr::GET`,
#' `httr2::req_perform`, `curl::curl_download` and similar, also when wrapped in `Cache()`.
#' Each call gives the literal URL if the `url` argument is a string, the metadata
#' `sourceURL` for `extractURL("object")`, the file and line (`where`), the enclosing
#' function (`call` holds the function that downloads, `event` the event it belongs to when
#' the module's `doEvent` function calls it directly, and `.inputObjects` counts as
#' `"init"`), and the object the result is assigned to when it is `sim$object <- `.
#' Calls into package helpers such as `LandR::prepInputs_CWIM()` are opaque: the row has
#' `url = NA` and the helper name in `call`, so there is a download there that the scan
#' cannot name. A `url` built at run time (`paste0()`, a variable) also gives `url = NA`,
#' or the literal start of it, with the expression in `note`. A row found by both layers
#' is one row, with `source = "metadata+code"`.
#'
#' ## Layer 3: run
#' If `sim` is given, `reproducible::urlLog(sim)` (SpaDES.core's method for a `simList`)
#' supplies the module, event, URL and cache id of every `prepInputs`/`preProcess` access
#' of the run. Rows are joined on module and URL; a match has `observed = TRUE` and the
#' `cacheId`; a URL the run recorded that neither other layer knew becomes a row with
#' `source = "run"`: a download the static layers missed.
#' A ledger can miss downloads that did happen: fetches that bypass
#' `prepInputs`/`preProcess` (for example `googledrive` or `download.file`); cache entries
#' created before URL tagging, which have no tags to replay on a cache hit or a
#' `cacheChaining` jump; and fallbacks that did not run because another module supplied the
#' object. So `observed = FALSE` means "not seen in this run", not "not used". The ledger is
#' kept only if `options(reproducible.urlLog)` is not `FALSE` and `options(spades.urlLog)`
#' is not `FALSE` (both are on by default) while the run is made.
#'
#' ## Reading the result
#' `source` lists the layers that saw the row, joined by `"+"`, in the order metadata, code,
#' run. Use `grepl("run", source)` rather than `source == "run"` to select by layer.
#' ```
#' inv <- dataInventory(modules, paths)
#' inv[!is.na(inv$url), ]                          # all named datasets
#' split(inv$module, inv$url)                      # by dataset: which modules use it
#' split(inv$url, inv$module)                      # by module: which data it needs
#' inv[inv$role == "fallback", ]                   # downloads that may never run
#' inv[is.na(inv$url), ]                           # downloads the scan could not name
#' ```
#' `reproducible::prepInputsLog()` is the session-wide record of every URL accessed, with
#' no module attached, and `reproducible::urlLog(sim)` is the per-run record;
#' `dataInventory()` adds the static view, which exists before anything has run.
#'
#' @param modules Character vector of module names, as returned by [setupProject()]
#'   (`"owner/repo@ref"` is reduced to the module name). The children of a parent module
#'   (its `childModules`) are included.
#' @param paths The `paths` list returned by [setupProject()] (its `modulePath` is used),
#'   or the module path itself as a character vector.
#' @param sim An optional `simList` from a finished run, for layer 3.
#' @param sources Which layers to use: any of `"metadata"`, `"code"`, `"run"`. `"run"` is
#'   used only if `sim` is given.
#' @param verbose Numeric. Above 0 prints a one-line summary.
#'
#' @return A `data.table`, one row per dataset URL and module (rows without a URL are one
#'   per call), with columns `module`, `object`, `url`, `source`, `event`, `call`, `where`
#'   (`file:line` for rows from code), `role` (`"default"` or `"fallback"`), `observed`
#'   (`TRUE` if the run's ledger has it), `cacheId` (from the run) and `note`.
#'
#' @seealso [SpaDES.core::moduleMetadata()], `reproducible::urlLog()`,
#'   `reproducible::prepInputsLog()`
#' @export
#' @examples
#' if (requireNamespace("SpaDES.core", quietly = TRUE)) {
#'   mp <- tempfile("modules")
#'   dir.create(file.path(mp, "demo"), recursive = TRUE)
#'   writeLines(c(
#'     'defineModule(sim, list(name = "demo", description = "d", keywords = "k",',
#'     '  authors = person("A", "B", email = "a@b.c", role = c("aut", "cre")),',
#'     '  childModules = character(0), version = list(demo = "0.0.1"),',
#'     '  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list(),',
#'     '  documentation = list(), reqdPkgs = list(), parameters = bindrows(),',
#'     '  inputObjects = bindrows(expectsInput("dem", "SpatRaster", "elevation",',
#'     '    sourceURL = "https://example.org/dem.tif")),',
#'     '  outputObjects = bindrows()))',
#'     'Init <- function(sim) {',
#'     '  sim$dem <- prepInputs(url = extractURL("dem"), destinationPath = tempdir())',
#'     '  sim$roads <- prepInputs(url = "https://example.org/roads.zip")',
#'     '  sim',
#'     '}'), file.path(mp, "demo", "demo.R"))
#'   dataInventory("demo", list(modulePath = mp))
#' }
dataInventory <- function(modules, paths, sim = NULL,
                          sources = c("metadata", "code", "run"),
                          verbose = getOption("Require.verbose", 1L)) {
  if (!requireNamespace("SpaDES.core", quietly = TRUE))
    stop("dataInventory() needs the SpaDES.core package")
  sources <- match.arg(sources, several.ok = TRUE)
  modulePath <- if (is.list(paths)) paths$modulePath else paths
  mods <- .diReadModules(modules, modulePath)
  inv <- .diBuild(mods, modulePath, sim, sources)
  if (verbose > 0)
    message("dataInventory: ", NROW(inv), " rows from ", length(mods), " modules (",
            sum(!is.na(inv$url)), " with a url, ", sum(inv$observed), " seen in the run)")
  inv[]
}

## The inventory table from the modules read by .diReadModules().
.diBuild <- function(mods, modulePath, sim = NULL, sources = c("metadata", "code", "run")) {
  meta <- .diMetadata(mods, modulePath)
  rows <- list(
    if ("metadata" %in% sources) meta$urls,
    if ("code" %in% sources) .diCode(names(mods), modulePath, meta$urls)
  )
  if ("run" %in% sources && !is.null(sim)) rows <- c(rows, list(.diRun(sim)))
  .diJoin(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE), meta$outputs)
}

## The columns of the result, in order; every layer builds rows with (a subset of) these.
.diColumns <- c("module", "object", "url", "source", "event", "call", "where", "role",
                "observed", "cacheId", "note")

## A data.table of the result's columns; absent ones are NA (character, except `observed`).
.diRows <- function(...) {
  x <- data.table::as.data.table(list(...))
  for (nm in setdiff(.diColumns, names(x)))
    data.table::set(x, j = nm, value = if (nm == "observed") FALSE else NA_character_)
  data.table::setcolorder(x, .diColumns)
  x
}

## The metadata of each module in `modules` ("owner/repo@ref/modules/child" -> "child") and of
## the children of each parent, recursively: a named list of SpaDES.core::moduleMetadata()
## results (inputObjects, outputObjects, parameters, childModules). A module without a
## folder in `modulePath` is skipped, with a warning if `warn`.
.diReadModules <- function(modules, modulePath, warn = TRUE) {
  todo <- unique(basename(sub("@[^/]*", "", unlist(modules))))
  out <- list()
  while (length(todo)) {
    m <- todo[1L]
    todo <- todo[-1L]
    if (m %in% names(out)) next
    mp <- .diModulePath(m, modulePath)
    if (is.na(mp)) {
      if (warn)
        warning("module '", m, "' not found in ", paste(modulePath, collapse = ", "),
                "; skipped", call. = FALSE)
      next
    }
    md <- tryCatch(SpaDES.core::moduleMetadata(
      module = m, path = mp,
      defineModuleListItems = c("inputObjects", "outputObjects", "parameters", "childModules")),
      error = function(e) NULL)
    if (is.null(md)) next
    out[[m]] <- md
    todo <- c(todo, basename(sub("@[^/]*", "", as.character(md$childModules))))
  }
  out
}

## The module path (one of `modulePath`) that holds module `m`, or NA.
.diModulePath <- function(m, modulePath) {
  hit <- modulePath[file.exists(file.path(modulePath, m, paste0(m, ".R")))]
  if (length(hit)) hit[1L] else NA_character_
}

.diModuleFile <- function(m, modulePath) {
  p <- .diModulePath(m, modulePath)
  if (is.na(p)) NA_character_ else file.path(p, m, paste0(m, ".R"))
}

## Layer 1. Returns the sourceURL rows and, per module, the objects it creates.
.diMetadata <- function(mods, modulePath) {
  urls <- list()
  outputs <- list()
  for (m in names(mods)) {
    md <- mods[[m]]
    outputs[[m]] <- as.character(md$outputObjects$objectName)
    inp <- md$inputObjects
    u <- as.character(inp$sourceURL)
    keep <- !is.na(u) & nzchar(u)
    if (!any(keep)) next
    pd <- .diParse(.diModuleFile(m, modulePath))
    urls[[m]] <- .diRows(module = m, object = as.character(inp$objectName)[keep], url = u[keep],
                         source = "metadata",
                         where = vapply(u[keep], function(x) .diUrlLine(pd, x, m), character(1)))
  }
  list(urls = data.table::rbindlist(urls), outputs = outputs)
}

## "file:line" of the first string constant equal to `url` in a parsed file, or NA.
.diUrlLine <- function(pd, url, m) {
  if (is.null(pd)) return(NA_character_)
  s <- pd[pd$token == "STR_CONST", ]
  hit <- s$line1[vapply(s$text, function(t) identical(.diUnquote(t), url), logical(1))]
  if (length(hit)) paste0(m, ".R:", hit[1L]) else NA_character_
}

.diUnquote <- function(txt) {
  v <- tryCatch(eval(parse(text = txt, keep.source = FALSE)[[1L]]), error = function(e) NA_character_)
  if (is.character(v) && length(v) == 1L) v else NA_character_
}

## getParseData() of a file, NULL if it does not parse.
.diParse <- function(file) {
  ex <- tryCatch(parse(file = file, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  utils::getParseData(ex, includeText = TRUE)
}

## Functions whose call downloads: the argument that holds the address, and the function's
## formals up to it (to match a positional address).
.diDownloaders <- list(
  prepInputs = list(formals = c("targetFile", "url"), url = "url"),
  preProcess = list(formals = c("targetFile", "url"), url = "url"),
  download.file = list(formals = c("url", "destfile"), url = "url"),
  curl_download = list(formals = c("url", "destfile"), url = "url"),
  curl_fetch_disk = list(formals = c("url", "path"), url = "url"),
  drive_download = list(formals = c("file", "path"), url = "file"),
  GET = list(formals = "url", url = "url"),
  request = list(formals = "base_url", url = "base_url")
)

## Opaque helpers: package functions that download but are not analysed (a name that starts
## with `prepInputs` other than `prepInputs` itself, e.g. LandR::prepInputs_CWIM).
.diIsHelper <- function(fn) grepl("^prepInputs[._]", fn)

## Layer 2.
.diCode <- function(modNames, modulePath, metaUrls) {
  out <- list()
  for (m in modNames) {
    base <- file.path(.diModulePath(m, modulePath), m)
    files <- c(file.path(base, paste0(m, ".R")),
               list.files(file.path(base, "R"), pattern = "\\.[Rr]$", full.names = TRUE,
                          recursive = TRUE))
    pds <- stats::setNames(lapply(files, .diParse), basename(files))
    pds <- pds[!vapply(pds, is.null, logical(1))]
    defined <- unique(unlist(lapply(pds, function(pd) .diAssigned(pd, functionsOnly = TRUE))))
    events <- .diEventMap(pds[[paste0(m, ".R")]], m)
    mu <- metaUrls[metaUrls$module == m, ]
    for (f in names(pds))
      out[[length(out) + 1L]] <- .diScanFile(pds[[f]], f, m, defined, events, mu)
  }
  data.table::rbindlist(out)
}

## Children of an expr node `id`.
.diKids <- function(pd, id) pd[pd$parent == id, ]

## For each assignment `name <- <rhs>` / `name = <rhs>`: the lhs expr id, name, rhs id.
.diAssignments <- function(pd) {
  asg <- pd[pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), ]
  data.table::rbindlist(lapply(seq_len(nrow(asg)), function(i) {
    k <- .diKids(pd, asg$parent[i])
    k <- k[k$token != "COMMENT", ]
    if (nrow(k) != 3L || k$token[2L] != "LEFT_ASSIGN" && k$token[2L] != "EQ_ASSIGN") return(NULL)
    data.table::data.table(node = k$parent[1L], lhsId = k$id[1L], lhs = k$text[1L], rhsId = k$id[3L])
  }))
}

.diAssigned <- function(pd, functionsOnly = FALSE) {
  a <- .diAssignments(pd)
  if (!NROW(a)) return(character())
  if (functionsOnly)
    a <- a[vapply(a$rhsId, function(i) "FUNCTION" %in% .diKids(pd, i)$token, logical(1)), ]
  a$lhs
}

## Name of the nearest enclosing named function of node `id`, NA if none.
.diEnclosingFn <- function(pd, id, asg) {
  repeat {
    p <- pd$parent[match(id, pd$id)]
    if (is.na(p) || p == 0L) return(NA_character_)
    if ("FUNCTION" %in% .diKids(pd, p)$token) {
      hit <- asg[asg$rhsId == p, ]
      if (nrow(hit)) return(hit$lhs[1L])
    }
    id <- p
  }
}

## Ids of the ancestors of `id`, nearest first.
.diAncestors <- function(pd, id) {
  out <- integer()
  repeat {
    id <- pd$parent[match(id, pd$id)]
    if (is.na(id) || id == 0L) return(out)
    out <- c(out, id)
  }
}

## function name -> event, from the arms of the switch(eventType, ...) in doEvent.<module>;
## `.inputObjects` runs in "init".
.diEventMap <- function(pd, m) {
  map <- c(.inputObjects = "init")
  if (is.null(pd)) return(map)
  asg <- .diAssignments(pd)
  de <- asg[asg$lhs == paste0("doEvent.", m), ]
  if (!nrow(de)) return(map)
  within <- function(x, node) {
    n <- pd[pd$id == node, ]
    (x$line1 > n$line1 | x$line1 == n$line1 & x$col1 >= n$col1) &
      (x$line2 < n$line2 | x$line2 == n$line2 & x$col2 <= n$col2)
  }
  body <- pd[within(pd, de$rhsId[1L]), ]
  sw <- body[body$token == "SYMBOL_FUNCTION_CALL" & body$text == "switch", ]
  if (!nrow(sw)) return(map)
  call <- pd$parent[match(pd$parent[match(sw$id[1L], pd$id)], pd$id)]
  k <- .diKids(pd, call)
  k <- k[order(k$line1, k$col1), ]
  for (i in which(k$token == "EQ_SUB")) {
    ev <- gsub("[\"'`]", "", k$text[i - 1L])
    arm <- k$id[i + 1L]
    calls <- pd[within(pd, arm) & pd$token == "SYMBOL_FUNCTION_CALL", ]
    for (fn in setdiff(calls$text, names(map))) map[[fn]] <- ev
  }
  map
}

## The downloading function a call `cl` makes, as list(fn, pkg, call) with `Cache(fn, ...)`
## turned into `fn(...)`; NULL if it is not one. Helpers defined in the module itself are
## not (their bodies are scanned).
.diTarget <- function(cl, defined) {
  isTarget <- function(fn) fn %in% names(.diDownloaders) || (.diIsHelper(fn) && !fn %in% defined)
  split <- function(h) {
    if (is.name(h)) return(c(NA_character_, as.character(h)))
    if (is.call(h) && identical(h[[1L]], quote(`::`))) return(c(as.character(h[[2L]]), as.character(h[[3L]])))
    NULL
  }
  h <- split(cl[[1L]])
  if (is.null(h)) return(NULL)
  if (h[2L] == "Cache" && length(cl) > 1L) {
    inner <- split(cl[[2L]])
    if (is.null(inner) || !isTarget(inner[2L])) return(NULL)
    cl[[1L]] <- cl[[2L]]
    cl[[2L]] <- NULL
    h <- inner
  }
  if (!isTarget(h[2L])) return(NULL)
  list(fn = h[2L], pkg = h[1L], call = cl)
}

## Scan one parsed file for downloading calls and for extractURL() calls outside them.
.diScanFile <- function(pd, file, m, defined, events, metaUrls) {
  asg <- .diAssignments(pd)
  rows <- list()
  for (i in which(pd$token == "SYMBOL_FUNCTION_CALL")) {
    fnExpr <- pd$parent[i]
    callId <- pd$parent[match(fnExpr, pd$id)]
    cl <- .diCallOf(pd, callId)
    if (is.null(cl)) next
    tg <- .diTarget(cl, defined)
    if (is.null(tg)) {
      if (pd$text[i] != "extractURL") next
      ## an extractURL() inside a downloading call is reported by that call
      inside <- vapply(.diAncestors(pd, callId), function(a) {
        ca <- if ("'('" %in% .diKids(pd, a)$token) .diCallOf(pd, a) else NULL
        !is.null(ca) && !is.null(.diTarget(ca, defined))
      }, logical(1))
      if (any(inside)) next
      tg <- list(fn = "extractURL", pkg = NA_character_, call = cl)
    }
    rows[[length(rows) + 1L]] <- .diCallRows(pd, tg, callId, pd$line1[i], file, m, asg, events, metaUrls)
  }
  data.table::rbindlist(rows)
}

## The call at expr node `id` as an R call, NULL if its text does not parse to one.
.diCallOf <- function(pd, id) {
  ex <- tryCatch(parse(text = utils::getParseText(pd, id), keep.source = FALSE)[[1L]],
                 error = function(e) NULL)
  if (is.call(ex)) ex else NULL
}

## The url(s) in one argument expression `e` as list(url, object, note) items: string
## constants, extractURL("object") (resolved to the metadata url), else the expression is kept
## in `note` with the literal start of it as the url.
.diUrlsOf <- function(e, metaUrls, m) {
  res <- function(url, object = NA_character_, note = NA_character_)
    list(url = url, object = object, note = note)
  if (is.character(e)) return(lapply(e, res))
  obj <- .diExtractURLs(e)
  if (length(obj)) return(lapply(obj, function(o) res(.diResolve(o, metaUrls, m), o)))
  if (is.call(e) && all(all.names(e) == "c") && all(vapply(as.list(e)[-1L], is.character, logical(1))))
    return(lapply(unlist(as.list(e)[-1L]), res))
  txt <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
  lit <- grep("://", .diStrings(e), value = TRUE)
  list(res(if (length(lit)) lit[1L] else NA_character_, note = txt))
}

## Rows for one downloading call `tg` (see .diTarget()).
.diCallRows <- function(pd, tg, callId, line, file, m, asg, events, metaUrls) {
  cl <- tg$call
  encl <- .diEnclosingFn(pd, callId, asg)
  label <- if (is.na(tg$pkg)) tg$fn else paste0(tg$pkg, "::", tg$fn)
  dl <- .diDownloaders[[tg$fn]]
  urls <- if (tg$fn == "extractURL") {
    .diUrlsOf(cl, metaUrls, m)
  } else if (!is.null(dl)) {
    fm <- c(dl$formals, "...")
    ## a call with `...` in it cannot be matched; fall back to the argument by name
    mc <- tryCatch(match.call(as.function(c(stats::setNames(rep(list(quote(expr = )), length(fm)), fm),
                                            list(NULL))),
                              cl, expand.dots = FALSE),
                   error = function(e) cl)
    if (is.null(mc[[dl$url]])) list() else .diUrlsOf(mc[[dl$url]], metaUrls, m)
  } else {
    ## opaque helper: any extractURL() or literal url among its arguments
    unlist(lapply(as.list(cl)[-1L], function(a)
      if (is.character(a) && grepl("://", a)) .diUrlsOf(a, metaUrls, m) else
        if (length(.diExtractURLs(a))) .diUrlsOf(a, metaUrls, m)), recursive = FALSE)
  }
  if (!length(urls)) urls <- list(list(url = NA_character_, object = NA_character_, note = NA_character_))
  obj <- .diAssignedObject(pd, callId, asg)
  data.table::rbindlist(lapply(urls, function(u)
    .diRows(module = m, object = if (!is.na(obj)) obj else u$object, url = u$url, source = "code",
            event = if (!is.na(encl) && encl %in% names(events)) events[[encl]] else NA_character_,
            call = if (is.na(encl)) label else paste0(label, " in ", encl),
            where = paste0(file, ":", line), note = u$note)))
}

## Strings inside an expression.
.diStrings <- function(e) {
  if (is.character(e)) return(e)
  if (is.call(e)) return(unlist(lapply(as.list(e), .diStrings)))
  character()
}

## Object names of every extractURL("obj") call inside expression `e`.
.diExtractURLs <- function(e) {
  if (!is.call(e)) return(character())
  if (identical(e[[1L]], quote(extractURL))) {
    a <- e[[2L]]
    return(if (is.character(a)) a else character())
  }
  unlist(lapply(as.list(e), .diExtractURLs))
}

## The metadata url of object `obj` in the module (NA if the module declares none).
.diResolve <- function(obj, metaUrls, m) {
  u <- metaUrls$url[metaUrls$module == m & metaUrls$object == obj]
  if (length(u)) u[1L] else NA_character_
}

## The object in `sim$object <- <call>` / `sim[["object"]] <- <call>`, climbing through pipes and
## wrapping calls; NA if the call is not assigned to a `$` or `[[` target.
.diAssignedObject <- function(pd, id, asg) {
  for (i in 1:4) {
    p <- pd$parent[match(id, pd$id)]
    if (is.na(p) || p == 0L) return(NA_character_)
    hit <- asg[asg$node == p & asg$rhsId == id, ]
    if (nrow(hit)) {
      lhs <- tryCatch(parse(text = hit$lhs[1L], keep.source = FALSE)[[1L]], error = function(e) NULL)
      if (is.call(lhs) && identical(lhs[[1L]], quote(`$`))) return(as.character(lhs[[3L]]))
      if (is.call(lhs) && identical(lhs[[1L]], quote(`[[`)) && is.character(lhs[[3L]])) return(lhs[[3L]])
      return(NA_character_)
    }
    if ("FUNCTION" %in% .diKids(pd, p)$token || "'{'" %in% .diKids(pd, p)$token) return(NA_character_)
    id <- p
  }
  NA_character_
}

## Layer 3: the run's ledger as rows, one per module + url.
.diRun <- function(sim) {
  if (!requireNamespace("reproducible", quietly = TRUE))
    stop("dataInventory(sim = ) needs the reproducible package")
  ul <- reproducible::urlLog(sim, which = c("cacheId", "module", "event", "url"))
  ul <- ul[!is.na(ul$url), ]
  if (!NROW(ul)) return(NULL)
  ul <- ul[, list(event = paste(unique(stats::na.omit(event)), collapse = "; "),
                  cacheId = paste(unique(stats::na.omit(cacheId)), collapse = "; ")),
           by = c("module", "url")]
  .diRows(module = ul$module, url = ul$url, source = "run", event = ul$event, cacheId = ul$cacheId,
          observed = TRUE)
}

## Combine the layers: one row per (module, url) where there is a url; rows without a url stay
## as they are. Then decide `role`.
.diJoin <- function(x, outputs) {
  if (!NROW(x)) return(.diRows()[0L, ])
  x$key <- ifelse(is.na(x$url), paste0("row", seq_len(nrow(x))),
                  paste(x$module, sub("/+$", "", x$url), sep = "\r"))
  first <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (length(v)) v[1L] else NA_character_ }
  all_ <- function(v) { v <- unique(v[!is.na(v) & nzchar(v)]); if (length(v)) paste(v, collapse = "; ") else NA_character_ }
  layers <- c("metadata", "code", "run")
  out <- x[, list(
    module = module[1L], object = first(object), url = first(url),
    source = paste(layers[layers %in% source], collapse = "+"),
    event = first(event),
    call = all_(call), where = all_(where), role = NA_character_,
    observed = any(observed), cacheId = all_(cacheId), note = all_(note)
  ), by = "key"]
  out$key <- NULL
  out$role <- ifelse(vapply(seq_len(nrow(out)), function(i) {
    !is.na(out$object[i]) &&
      any(vapply(setdiff(names(outputs), out$module[i]), function(o) out$object[i] %in% outputs[[o]], logical(1)))
  }, logical(1)), "fallback", "default")
  data.table::setcolorder(out, .diColumns)
  out
}
