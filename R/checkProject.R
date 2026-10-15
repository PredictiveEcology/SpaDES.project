utils::globalVariables(c("object", "event", "where", "observed", "cacheId", "note"))

#' Preflight check of a project set up by `setupProject()`
#'
#' Looks at what [setupProject()] returned (the modules, paths, `params` and `times`) and
#' reports the things that are likely to go wrong in `simInit()` or `spades()`, so a new user
#' who passes little more than `modules` learns about them first. It reads module metadata
#' and code only: it downloads nothing and does not run any module. The metadata is read from
#' the module files without `simInit()` (see [dataInventory()]) and kept for the session, so a
#' repeated call is fast; the checks always run against the current `inSim`. [setupProject()]
#' calls it at its end, only printing messages, when `options(SpaDES.project.checkProject = TRUE)`
#' (default `FALSE`); it never stops and never changes what [setupProject()] returns. Each
#' finding is printed once per session (the same check, module and message is not repeated)
#' unless `forceShow = TRUE`; the returned table always has all of them. A check that cannot
#' decide (a module file that does not parse, a missing tool)
#' says nothing.
#'
#' @details
#' Every check gives rows with `severity` `"error-likely"` (the run will probably fail),
#' `"warning"` (the result is probably not what was intended) or `"note"` (for information).
#' To silence one kind of finding, remove it with the `fix` given; there is no per-check
#' switch, only the one option above for the call made by [setupProject()].
#'
#' ## times (`check = "times"`)
#' `times` is missing, so `simInit()` uses 0 to 10, or its `start` is before 1800, while a
#' module appears to read year-indexed data: a `sourceURL` or a parameter default holding a
#' year from 1900 to 2100, or a parameter whose name contains "Year". Such modules look data
#' up by calendar year, so model years give no data or the wrong year. Fix: set
#' `times = list(start = 2020, end = 2050)` (calendar years).
#'
#' ## studyAreaOrder (`check = "studyAreaOrder"`)
#' Module A lists a study-area object X (a name starting with `studyArea` or
#' `rasterToMatch`) in `createsOutput`, module B lists it in `expectsInput`, and B's
#' `.inputObjects` function uses `sim$X` or `sim[["X"]]`. Unless
#' `options(spades.allowInitDuringSimInit = TRUE)`, B's `.inputObjects` runs before A's
#' `init` has created X, so B builds its own default X instead of using A's. Fix: set that
#' option; see `?SpaDES.core::spadesOptions`, section "Running some init events early". A
#' module that itself expects objects from other modules cannot run early even with the
#' option; with the option set, such an A is reported as a `"note"`.
#'
#' ## googleDrive (`check = "googleDrive"`)
#' A dataset in [dataInventory()] (metadata or code layer) is on `drive.google.com`, and no
#' Google Drive credentials are configured: no `gargle_oauth_email` option, no
#' `GOOGLEDRIVE_AUTH` or `GARGLE_SERVICE_ACCOUNT` environment variable, and `googledrive` is
#' not installed or `googledrive::drive_has_token()` is `FALSE`. A batch run fails at the
#' download; an interactive one stops to ask. Fix: authenticate before the run
#' (`googledrive::drive_auth()`), or set one of those settings. The row is `"error-likely"`
#' when the session is not interactive, otherwise `"warning"`.
#'
#' ## disk (`check = "disk"`)
#' Free space on the filesystem of `paths$inputPath`, `cachePath`, `outputPath`,
#' `scratchPath` and `getOption("reproducible.destinationPathShared")` is below
#' `getOption("SpaDES.project.checkProject.minFreeGB", 50)` GB (one row per filesystem; not
#' checked on Windows). Fix: free space or point those paths at a larger disk, or lower the
#' option.
#'
#' ## linkedParams (`check = "linkedParams"`)
#' A parameter that a module reads from the other modules with
#' `SpaDES.core::paramCheckOtherMods()` (found by scanning its code), defined by two or more
#' modules whose effective values differ. The effective value of a module is its own value in
#' `params`, else the one in `params$.globals`, else its default in the metadata. It is
#' reported only when a value was supplied in `params` for at least one of them: modules'
#' own defaults are never compared with each other. Numbers that are
#' equal count as equal (`10L` and `10`). The standard SpaDES parameters (`.plots`, `.useCache`, `.saveInterval` and
#' similar) are not compared. Fix: set the value once in `params$.globals`, or per module.
#'
#' ## fallbackStudyArea (`check = "fallbackStudyArea"`)
#' A module has an input named `studyArea` or `rasterToMatch` (or one starting with those
#' names) with a `sourceURL`, no module in the set creates it, and it was not supplied
#' (`objects`, or an element of `inSim` of that name). The module will use that default,
#' which is usually a large national or test area. Fix: supply the object, or add a module
#' that creates it.
#'
#' @param inSim A list as returned by [setupProject()]; the elements used are `modules`,
#'   `paths` (with `modulePath`, `inputPath`, `cachePath`, `outputPath`, `scratchPath`),
#'   `params`, `times` and `objects`. A module that is not in `paths$modulePath` is skipped.
#' @param ... Elements that replace those of `inSim` (e.g. `times = list(start = 2020, end = 2030)`).
#' @param verbose Numeric. Above 0 prints the summary.
#' @param forceShow Logical. `TRUE` prints findings already printed earlier in the session.
#'
#' @return Invisibly, a `data.table` with columns `check`, `severity`, `module`, `message`
#'   and `fix`, one row per finding (zero rows when nothing was found).
#'
#' @seealso [dataInventory()], [setupProject()]
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
#'     '  documentation = list(), reqdPkgs = list(),',
#'     '  parameters = bindrows(defineParameter("dataYear", "numeric", 2020, NA, NA, "year")),',
#'     '  inputObjects = bindrows(expectsInput("studyArea", "sf", "area",',
#'     '    sourceURL = "https://example.org/area.shp")),',
#'     '  outputObjects = bindrows()))'), file.path(mp, "demo", "demo.R"))
#'   inSim <- list(modules = "demo", paths = list(modulePath = mp),
#'                 times = list(start = 0, end = 10))
#'   res <- checkProject(inSim)
#'   res$check
#' }
checkProject <- function(inSim, ..., verbose = getOption("Require.verbose", 1L),
                         forceShow = FALSE) {
  if (!requireNamespace("SpaDES.core", quietly = TRUE))
    stop("checkProject() needs the SpaDES.core package")
  inSim <- utils::modifyList(as.list(inSim), list(...))
  modulePath <- if (is.list(inSim$paths)) inSim$paths$modulePath else inSim$paths
  mods <- .diReadModules(inSim$modules, modulePath, warn = FALSE)
  ctx <- list(inSim = inSim, mods = mods, modulePath = modulePath)
  if (length(mods))
    ctx$inv <- tryCatch(.diBuild(mods, modulePath, sources = c("metadata", "code")),
                        error = function(e) NULL)
  res <- data.table::rbindlist(lapply(
    list(.cpTimes, .cpOrder, .cpDrive, .cpDisk, .cpParams, .cpStudyArea),
    function(f) if (length(mods)) tryCatch(f(ctx), error = function(e) NULL)),
    use.names = TRUE)
  if (!NROW(res)) res <- .cpRows()[0L, ]
  .cpReport(res, verbose, forceShow)
  invisible(res)
}

## The call made at the end of setupProject(): messages only, never an error.
.checkProjectOnSetup <- function(out, verbose = 1L) {
  if (isTRUE(getOption("SpaDES.project.checkProject", FALSE)))
    try(checkProject(out, verbose = verbose), silent = TRUE)
  invisible()
}

## Findings already printed in this session, by check + module + message.
.cpShown <- new.env(parent = emptyenv())

.cpRows <- function(check = character(), severity = character(), module = character(),
                    message = character(), fix = character())
  data.table::data.table(check = check, severity = severity, module = module,
                         message = message, fix = fix)

.cpReport <- function(res, verbose, forceShow = FALSE) {
  if (verbose <= 0) return(invisible())
  if (!NROW(res)) return(message("checkProject: no problems found"))
  key <- paste(res$check, res$module, res$message, sep = "\r")
  new <- forceShow | !vapply(key, exists, logical(1), envir = .cpShown, inherits = FALSE)
  for (k in key[new]) assign(k, TRUE, envir = .cpShown)
  if (!any(new)) return(message("checkProject: ", NROW(res), " finding(s), all shown earlier in this session"))
  res <- res[new, ]
  n <- table(factor(res$severity, c("error-likely", "warning", "note")))
  message("checkProject: ", paste(n[n > 0], names(n)[n > 0], collapse = ", "),
          " (silence with options(SpaDES.project.checkProject = FALSE))")
  for (i in seq_len(NROW(res)))
    message("  [", res$severity[i], "] ", res$check[i],
            if (!is.na(res$module[i])) paste0(" (", res$module[i], ")"), ": ", res$message[i],
            "\n      fix: ", res$fix[i])
}

## Standard SpaDES parameters, which modules define alike and which are not compared.
.cpStandardParams <- c(".plots", ".plotInitialTime", ".plotInterval", ".saveInitialTime",
                       ".saveInterval", ".tempdir", ".useCache", ".useParallel", ".seed")

.cpYearRegex <- "(^|[^0-9])(19[0-9]{2}|20[0-9]{2}|2100)([^0-9]|$)"

## 1. times
.cpTimes <- function(ctx) {
  yearly <- names(ctx$mods)[vapply(ctx$mods, function(md) {
    urls <- as.character(md$inputObjects$sourceURL)
    defs <- md$parameters$default
    any(grepl(.cpYearRegex, urls)) || any(grepl("[Yy]ear", md$parameters$paramName)) ||
      any(vapply(defs, function(d)
        (is.numeric(d) && any(d == round(d) & d >= 1900 & d <= 2100, na.rm = TRUE)) ||
          (is.character(d) && any(grepl(.cpYearRegex, d))), logical(1)))
  }, logical(1))]
  if (!length(yearly)) return(NULL)
  start <- ctx$inSim$times$start
  why <- if (is.null(ctx$inSim$times)) {
    "`times` is not set, so simInit() uses 0 to 10"
  } else if (is.numeric(start) && start < 1800) {
    paste0("`times` start is ", start, ": the times look like model years, not calendar years")
  } else return(NULL)
  .cpRows("times", "warning", paste(yearly, collapse = ", "),
          paste0(why, ", but these modules read year-indexed data (calendar years)"),
          "set times = list(start = <calendar year>, end = <calendar year>)")
}

## Object names used as sim$X / sim[["X"]] in the module's `.inputObjects` function.
.cpInputObjectsRefs <- function(file) {
  ex <- tryCatch(parse(file = file, keep.source = FALSE), error = function(e) NULL)
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (identical(e[[1L]], quote(`$`)) && identical(e[[2L]], quote(sim)))
      found <<- c(found, as.character(e[[3L]]))
    if (identical(e[[1L]], quote(`[[`)) && identical(e[[2L]], quote(sim)) && is.character(e[[3L]]))
      found <<- c(found, e[[3L]])
    for (el in as.list(e)) if (!missing(el)) walk(el)
  }
  for (e in ex)
    if (is.call(e) && as.character(e[[1L]])[1L] %in% c("<-", "=") &&
        identical(e[[2L]], quote(.inputObjects))) walk(e[[3L]])
  unique(found)
}

.cpStudyAreaRegex <- "^(studyArea|rasterToMatch)"

## 2. study-area (init) ordering: one row per module that uses another's object in .inputObjects
.cpOrder <- function(ctx) {
  early <- isTRUE(getOption("spades.allowInitDuringSimInit", FALSE))
  mods <- ctx$mods
  outs <- lapply(mods, function(md) as.character(md$outputObjects$objectName))
  ins <- lapply(mods, function(md) as.character(md$inputObjects$objectName))
  needsOthers <- function(a) intersect(ins[[a]], unlist(outs[setdiff(names(mods), a)]))
  pairs <- as.data.frame(data.table::rbindlist(lapply(names(mods), function(b) {
    x <- intersect(.cpInputObjectsRefs(.diModuleFile(b, ctx$modulePath)), ins[[b]])
    x <- grep(.cpStudyAreaRegex, x, value = TRUE)
    p <- lapply(x, function(o) {
      a <- setdiff(names(mods)[vapply(outs, function(z) o %in% z, logical(1))], b)
      if (length(a)) data.frame(b = b, x = o, a = a)
    })
    data.table::rbindlist(p)
  })))
  if (!NROW(pairs)) return(NULL)
  cannotEarly <- function(a) {
    n <- needsOthers(a)
    if (length(n)) paste0("'", a, "' itself expects ", paste(utils::head(n, 3L), collapse = ", "),
                          if (length(n) > 3L) paste0(" and ", length(n) - 3L, " more"),
                          " from other modules, so it cannot run early even with the option")
  }
  if (!early) {
    rows <- lapply(unique(pairs$b), function(b) {
      p <- pairs[pairs$b == b, ]
      cannot <- unlist(lapply(unique(p$a), cannotEarly))
      .cpRows("studyAreaOrder", "warning", b,
              paste0("the .inputObjects of '", b, "' uses ", paste0("sim$", p$x, " (created by '", p$a, "')",
                                                                   collapse = ", "),
                     "; .inputObjects runs before those init events, so '", b, "' builds its own defaults",
                     if (length(cannot)) paste0("; ", paste(cannot, collapse = "; "))),
              "options(spades.allowInitDuringSimInit = TRUE); see ?SpaDES.core::spadesOptions, section 'Running some init events early'")
    })
  } else {
    rows <- lapply(unique(pairs$a), function(a) {
      cannot <- cannotEarly(a)
      if (is.null(cannot)) return(NULL)
      p <- pairs[pairs$a == a, ]
      .cpRows("studyAreaOrder", "note", a,
              paste0(cannot, "; ", paste0("'", unique(p$b), "'", collapse = ", "),
                     " use what it creates in .inputObjects"),
              "let it take those objects from an input or a default, or supply them")
    })
  }
  data.table::rbindlist(rows)
}

## TRUE if Google Drive credentials look configured.
.cpDriveCredentials <- function() {
  email <- getOption("gargle_oauth_email")
  (!is.null(email) && !is.na(email[1L]) && !isFALSE(email)) ||
    nzchar(Sys.getenv("GOOGLEDRIVE_AUTH")) || nzchar(Sys.getenv("GARGLE_SERVICE_ACCOUNT")) ||
    (requireNamespace("googledrive", quietly = TRUE) && isTRUE(googledrive::drive_has_token()))
}

## 3. Google Drive datasets without credentials
.cpDrive <- function(ctx) {
  inv <- ctx$inv
  if (is.null(inv) || .cpDriveCredentials()) return(NULL)
  d <- inv[!is.na(inv$url) & grepl("drive.google.com", inv$url, fixed = TRUE), ]
  if (!NROW(d)) return(NULL)
  rows <- lapply(unique(d$module), function(m) {
    x <- d[d$module == m, ]
    .cpRows("googleDrive", if (interactive()) "warning" else "error-likely", m,
            paste0("no Google Drive credentials, and ", m, " reads: ",
                   paste0(ifelse(is.na(x$object), "", paste0(x$object, " ")), x$url,
                          ifelse(is.na(x$note), "", " (built at run time)"),
                          ifelse(x$role == "fallback", " (fallback)", ""), collapse = "; "),
                   if (interactive()) "; the run will prompt" else "; this run cannot ask, so it will fail"),
            "googledrive::drive_auth() first, or set options(gargle_oauth_email = ), GOOGLEDRIVE_AUTH or GARGLE_SERVICE_ACCOUNT")
  })
  data.table::rbindlist(rows)
}

## Free GB and mount point of the filesystem holding `path` (its nearest existing parent), or NULL.
.cpFreeGB <- function(path) {
  while (!dir.exists(path) && dirname(path) != path) path <- dirname(path)
  df <- tryCatch(suppressWarnings(system2("df", c("-Pk", shQuote(path)), stdout = TRUE, stderr = FALSE)),
                 error = function(e) NULL)
  if (length(df) < 2L) return(NULL)
  f <- strsplit(df[length(df)], "[[:space:]]+")[[1L]]
  gb <- suppressWarnings(as.numeric(f[4L])) / 1024^2
  if (is.na(gb)) NULL else list(mount = f[length(f)], gb = gb)
}

## 4. disk space
.cpDisk <- function(ctx) {
  if (.Platform$OS.type == "windows") return(NULL)
  p <- unlist(ctx$inSim$paths[c("inputPath", "cachePath", "outputPath", "scratchPath")])
  shared <- getOption("reproducible.destinationPathShared")
  p <- c(p, destinationPathShared = if (is.character(shared)) shared)
  if (!length(p)) return(NULL)
  free <- lapply(p, .cpFreeGB)
  ok <- !vapply(free, is.null, logical(1))
  if (!any(ok)) return(NULL)
  min <- getOption("SpaDES.project.checkProject.minFreeGB", 50)
  mount <- vapply(free[ok], `[[`, "", "mount")
  rows <- lapply(unique(mount), function(mt) {
    gb <- free[ok][mount == mt][[1L]]$gb
    if (gb >= min) return(NULL)
    .cpRows("disk", "warning", NA_character_,
            sprintf("%.1f GB free on %s (%s), below %g GB", gb, mt,
                    paste(names(p)[ok][mount == mt], collapse = ", "), min),
            "free space, point these paths at a larger disk, or lower options(SpaDES.project.checkProject.minFreeGB)")
  })
  data.table::rbindlist(rows)
}

## Parameter names each module passes to SpaDES.core::paramCheckOtherMods(sim, "<name>", ...).
.cpParamCheckNames <- function(m, modulePath) {
  base <- file.path(.diModulePath(m, modulePath), m)
  files <- c(file.path(base, paste0(m, ".R")),
             list.files(file.path(base, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    h <- e[[1L]]
    nm <- if (is.name(h)) as.character(h) else if (is.call(h) && identical(h[[1L]], quote(`::`))) as.character(h[[3L]])
    if (identical(nm, "paramCheckOtherMods")) {
      a <- as.list(e)[-1L]
      p <- if ("paramToCheck" %in% names(a)) a[["paramToCheck"]] else
        { un <- a[!nzchar(names(a) %||% rep("", length(a)))]; if (length(un) >= 2L) un[[2L]] }
      if (is.character(p)) found <<- c(found, p)
    }
    for (el in as.list(e)) if (!missing(el)) walk(el)
  }
  for (f in files) for (e in tryCatch(parse(file = f, keep.source = FALSE), error = function(e) NULL))
    walk(e)
  unique(found)
}

## 5. parameters shared by modules with different effective values
.cpParams <- function(ctx) {
  mods <- ctx$mods
  params <- ctx$inSim$params
  all <- data.table::rbindlist(lapply(names(mods), function(m) {
    pm <- mods[[m]]$parameters
    if (!NROW(pm)) return(NULL)
    data.table::data.table(module = m, name = pm$paramName, default = I(pm$default))
  }))
  if (!NROW(all)) return(NULL)
  checkers <- stats::setNames(lapply(names(mods), .cpParamCheckNames, modulePath = ctx$modulePath), names(mods))
  rows <- list()
  for (nm in setdiff(unique(all$name[duplicated(all$name)]), .cpStandardParams)) {
    x <- all[all$name == nm, ]
    vals <- lapply(seq_len(NROW(x)), function(i) {
      own <- params[[x$module[i]]]
      if (nm %in% names(own)) own[[nm]]
      else if (nm %in% names(params$.globals)) params$.globals[[nm]]
      else x$default[[i]]
    })
    ## 10L and 10, and NA of any type, are the same value
    norm <- function(v) if (is.numeric(v)) as.numeric(v) else if (length(v) == 1L && is.na(v)) NA else v
    txt <- vapply(vals, function(v) paste(deparse(norm(v)), collapse = ""), "")
    if (length(unique(txt)) < 2L) next
    by <- names(checkers)[vapply(checkers, function(z) nm %in% z, logical(1))]
    ## only a name some module checks with paramCheckOtherMods() is a conflict, and only when the
    ## user supplied a value: modules' own defaults are never compared with each other
    set <- vapply(x$module, function(m) nm %in% c(names(params[[m]]), names(params$.globals)), logical(1))
    if (!length(by) || !any(set)) next
    rows[[length(rows) + 1L]] <- .cpRows(
      "linkedParams", "warning", paste(x$module, collapse = ", "),
      paste0("parameter '", nm, "' differs between modules: ", paste0(x$module, "=", txt, collapse = ", "),
             if (length(by)) paste0(" (", paste(by, collapse = ", "), " check it with paramCheckOtherMods())")),
      paste0("set it once in params = list(.globals = list(", nm, " = ...)) or per module"))
  }
  data.table::rbindlist(rows)
}

## 6. a default studyArea / rasterToMatch that will be used
.cpStudyArea <- function(ctx) {
  mods <- ctx$mods
  made <- unlist(lapply(mods, function(md) as.character(md$outputObjects$objectName)))
  supplied <- c(names(ctx$inSim$objects), names(ctx$inSim))
  rows <- list()
  for (m in names(mods)) {
    inp <- mods[[m]]$inputObjects
    u <- as.character(inp$sourceURL)
    nm <- as.character(inp$objectName)
    hit <- grepl(.cpStudyAreaRegex, nm) & !is.na(u) & nzchar(u) &
      !nm %in% c(made, supplied)
    for (i in which(hit))
      rows[[length(rows) + 1L]] <- .cpRows(
        "fallbackStudyArea", "note", m,
        paste0("'", m, "' will use its default ", nm[i], " from ", u[i],
               ": no module in the set creates it and none was supplied"),
        paste0("supply ", nm[i], " (objects = list(", nm[i], " = ...)) or add a module that creates it"))
  }
  data.table::rbindlist(rows)
}
