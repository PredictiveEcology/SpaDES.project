## setupProject(alreadyRun = ): the one run of a CacheGeo ledger that `alreadyRun$scenario`
## names, and the modules it ran. Read before setupModules(), so the modules that run already
## ran are neither fetched nor have their packages installed.
.alreadyRunRead <- function(alreadyRun, destDir, verbose = getOption("Require.verbose", 1L)) {
  need <- c("ledger", "scenario") # `remote = NULL` is a local ledger in `destDir`
  if (!is.list(alreadyRun) || !all(need %in% names(alreadyRun)))
    stop("`alreadyRun` must be a list with elements ", paste(need, collapse = ", "), call. = FALSE)
  ledger <- reproducible::CacheGeoLedger(alreadyRun$ledger, remote = alreadyRun$remote,
                                         destinationPath = destDir, key = "runName")
  rows <- reproducible::CacheGeoRead(ledger, area = NULL, verbose = verbose - 1)
  run <- .alreadyRunScenario(rows, alreadyRun$scenario, alreadyRun$ledger)
  sim <- .alreadyRunShell(run, destDir)
  ## `sim` and `fetch` are what SpaDES.core::simInit(alreadyRun = ) replays the run from
  list(run = run, modules = .alreadyRunModules(sim), sim = sim, fetch = attr(sim, "fetchOutputs"))
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
## compared), a parent when every module below it was. A parent's `childModules` are read from
## its local copy, else from its own .R file fetched from GitHub into `scratch`, never into
## `modulePath`.
.isAlreadyRunModule <- function(module, ran, modulePath, scratch, seen = character()) {
  name <- extractModName(module)
  if (name %in% ran) return(TRUE)
  if (name %in% seen) return(FALSE)
  mp <- modulePath
  if (!dir.exists(file.path(whichModulePath(name, modulePath), name))) {
    if (!isGitHub(module) || nzchar(fileRelPathFromFullGHpath(module))) return(FALSE)
    mp <- scratch
    spec <- trimVersionNumber(module)
    if (!grepl("@", spec)) spec <- paste0(spec, "@HEAD")
    getGithubFile(paste0(spec, "/", name, ".R"), destDir = file.path(scratch, name),
                  overwrite = TRUE, verbose = -1)
  }
  kids <- .childModuleEntries(name, mp)
  if (!length(kids)) return(FALSE)
  kids <- vapply(kids, .childModuleSpec, character(1), parent = module,
                 versions = .moduleVersions(name, mp), USE.NAMES = FALSE)
  all(vapply(kids, .isAlreadyRunModule, logical(1), ran = ran, modulePath = modulePath,
             scratch = scratch, seen = c(seen, name)))
}
