## setupProject(alreadyRun = ): the one run of a CacheGeo ledger that `alreadyRun$scenario`
## names, and the modules it ran. Read before setupModules(), so the modules that run already
## ran are neither fetched nor have their packages installed.
## Several runs (a list, or a character vector, of runs; see .alreadyRunIsMulti()): a named list
## of the same, one per run, with attribute "multi" TRUE.
.alreadyRunRead <- function(alreadyRun, destDir, verbose = getOption("Require.verbose", 1L)) {
  if (.alreadyRunIsMulti(alreadyRun))
    return(.alreadyRunReadMulti(alreadyRun, destDir, verbose = verbose))
  need <- c("ledger", "scenario") # `remote = NULL` is a local ledger in `destDir`
  if (!is.list(alreadyRun) || !all(need %in% names(alreadyRun)))
    stop("`alreadyRun` must be a list with elements ", paste(need, collapse = ", "),
         ", or a list of runs", call. = FALSE)
  ## SpaDES.core's simInit() replays a run since 3.2.1.9058 (no floor in DESCRIPTION: only
  ## `alreadyRun` needs it)
  if (!requireNamespace("SpaDES.core", quietly = TRUE) ||
      utils::packageVersion("SpaDES.core") < "3.2.1.9058")
    stop("`alreadyRun` needs SpaDES.core >= 3.2.1.9058; please update it ",
         "(PredictiveEcology/SpaDES.core@development)", call. = FALSE)
  ledger <- reproducible::CacheGeoLedger(alreadyRun$ledger, remote = alreadyRun$remote,
                                         destinationPath = destDir, key = "runName")
  rows <- reproducible::CacheGeoRead(ledger, area = NULL, verbose = verbose - 1)
  run <- .alreadyRunScenario(rows, alreadyRun$scenario, alreadyRun$ledger)
  sim <- .alreadyRunShell(run, destDir)
  ## `sim` and `fetch` are what SpaDES.core::simInit(alreadyRun = ) replays the run from.
  ## `parents`: each parent module's children, as SpaDES.core's simInit() recorded them in
  ## sim$._modulesGiven; NULL for a run saved before it did.
  list(run = run, modules = .alreadyRunModules(sim), parents = sim$._modulesGiven$parents,
       sim = sim, fetch = attr(sim, "fetchOutputs"))
}

## The one row of `rows` whose columns match every element of `scenario`; else an error
## that lists what there is.
.alreadyRunScenario <- function(rows, scenario, file) {
  absent <- setdiff(names(scenario), names(rows))
  if (length(absent))
    stop("`alreadyRun$scenario` names column(s) that ", file, " does not have: ",
         paste(absent, collapse = ", "), call. = FALSE)
  keep <- rep(TRUE, NROW(rows))
  for (nam in names(scenario)) keep <- keep & as.character(rows[[nam]]) %in% as.character(scenario[[nam]])
  if (sum(keep) != 1L) {
    cols <- intersect(c("runName", names(scenario)), names(rows))
    listed <- as.data.frame(rows)[if (any(keep)) keep else TRUE, cols, drop = FALSE]
    stop("`alreadyRun$scenario` must identify one run of ", file, "; it matches ", sum(keep), ".\n",
         if (any(keep)) "The matches:\n" else "The runs:\n",
         paste(utils::capture.output(print(unique(listed), row.names = FALSE)), collapse = "\n"),
         call. = FALSE)
  }
  rows[keep, ]
}

## The run's saved simList. Only the simList shell and its manifest are fetched (no
## object, no output file: `outputsPattern` matches no file name); its "fetchOutputs"
## attribute fetches output files on request.
.alreadyRunShell <- function(run, destDir) {
  reGetUntarLoad(run$driveId, destDir = destDir, projectPath = destDir,
                 pathRemap = c(old = run$outputPath, new = file.path(destDir, run$runName)),
                 remote = TRUE, parse = FALSE, outputsPattern = "^$", verbose = FALSE)[[1]]
}

## The modules the run ran, with their versions, from its saved simList. A parent
## module is not in a simList, only its children.
.alreadyRunModules <- function(sim) {
  deps <- SpaDES.core::depends(sim)@dependencies
  data.frame(module = names(deps),
             version = vapply(deps, function(d) as.character(d@version), character(1)),
             row.names = NULL)
}

## Which of `modules` the run already ran: a module by name (versions are not
## compared), a parent when every module below it was. A parent's children are read from its
## local copy; else from `parents`, the run's own record of them; else from the parent's .R
## file fetched from GitHub into `scratch`, never into `modulePath`.
.isAlreadyRunModule <- function(module, ran, modulePath, scratch, parents = NULL, seen = character()) {
  name <- extractModName(module)
  if (name %in% ran) return(TRUE)
  if (name %in% seen) return(FALSE)
  isLocal <- dir.exists(file.path(whichModulePath(name, modulePath), name))
  if (!isLocal && name %in% names(parents)) {
    kids <- parents[[name]]
    versions <- NULL
  } else {
    mp <- modulePath
    if (!isLocal) {
      if (!isGitHub(module) || nzchar(fileRelPathFromFullGHpath(module))) return(FALSE)
      mp <- scratch
      spec <- trimVersionNumber(module)
      if (!grepl("@", spec)) spec <- paste0(spec, "@HEAD")
      getGithubFile(paste0(spec, "/", name, ".R"), destDir = file.path(scratch, name),
                    overwrite = TRUE, verbose = -1)
    }
    kids <- .childModuleEntries(name, mp)
    versions <- .moduleVersions(name, mp)
  }
  if (!length(kids)) return(FALSE)
  kids <- vapply(kids, .childModuleSpec, character(1), parent = module,
                 versions = versions, USE.NAMES = FALSE)
  all(vapply(kids, .isAlreadyRunModule, logical(1), ran = ran, modulePath = modulePath,
             scratch = scratch, parents = parents, seen = c(seen, name)))
}

## Is `alreadyRun` several runs? A character vector (saved simList files) or a list whose
## elements are each a ledger run `list(ledger, remote, scenario)`, a saved simList file, or
## `list(file = <saved simList file>, pathRemap = c(old = , new = ))`.
.alreadyRunIsMulti <- function(alreadyRun) {
  isLedger <- function(x) is.list(x) && all(c("ledger", "scenario") %in% names(x))
  ## one run, complete or not
  if (any(c("ledger", "remote", "scenario", "file", "pathRemap") %in% names(alreadyRun))) return(FALSE)
  isFile <- function(x) (is.character(x) && length(x) == 1L) || (is.list(x) && is.character(x[["file"]]))
  if (is.character(alreadyRun) && length(alreadyRun)) return(TRUE)
  if (is.list(alreadyRun) && length(alreadyRun) &&
      all(vapply(alreadyRun, function(x) isLedger(x) || isFile(x), logical(1))))
    return(TRUE)
  FALSE
}

## Several runs: each read as one run is (a ledger run) or loaded from its saved simList file,
## its objects left on disk until used (lazy). Named by run (unnamed: "1", "2", ...).
.alreadyRunReadMulti <- function(alreadyRun, destDir, verbose = getOption("Require.verbose", 1L)) {
  ## a SpaDES.core whose simInit() takes several runs (no floor in DESCRIPTION: only this needs it)
  if (!requireNamespace("SpaDES.core", quietly = TRUE) ||
      !exists(".alreadyRunMulti", envir = asNamespace("SpaDES.core"), inherits = FALSE))
    stop("`alreadyRun` with several runs needs a SpaDES.core whose simInit() takes them; ",
         "please update it (PredictiveEcology/SpaDES.core@development)", call. = FALSE)
  runs <- as.list(alreadyRun)
  if (is.null(names(runs))) names(runs) <- as.character(seq_along(runs))
  runs <- lapply(runs, function(x) {
    if (is.list(x) && is.null(x[["file"]]))
      return(.alreadyRunRead(x, destDir = destDir, verbose = verbose))
    if (is.character(x)) x <- list(file = x)
    sim <- SpaDES.core::loadSimList(x$file, parse = FALSE)
    if (!is.null(x$pathRemap))
      sim <- .remapOutputs(list(sim), x$pathRemap)[[1]]
    list(run = x$file, modules = .alreadyRunModules(sim), parents = sim$._modulesGiven$parents,
         sim = sim, fetch = NULL)
  })
  messageVerbose("Runs already done (", length(runs), "): ",
                 paste(names(runs), collapse = ", "), "; no module is skipped", verbose = verbose)
  structure(runs, multi = TRUE)
}
