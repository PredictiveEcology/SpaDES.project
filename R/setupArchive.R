## setupProject(archive = ): the one run of a CacheGeo ledger that `archive$scenario` names,
## and the modules it ran. Read before setupModules(), so the modules that run already ran
## are neither fetched nor have their packages installed.
.archiveRun <- function(archive, destDir, verbose = getOption("Require.verbose", 1L)) {
  need <- c("ledger", "scenario") # `remote = NULL` is a local ledger in `destDir`
  if (!is.list(archive) || !all(need %in% names(archive)))
    stop("`archive` must be a list with elements ", paste(need, collapse = ", "), call. = FALSE)
  ledger <- reproducible::CacheGeoLedger(archive$ledger, remote = archive$remote,
                                         destinationPath = destDir, key = "runName")
  rows <- reproducible::CacheGeoRead(ledger, area = NULL, verbose = verbose - 1)
  run <- .archiveSelectScenario(rows, archive$scenario, archive$ledger)
  list(run = run, modules = .archiveRanModules(run, destDir))
}

## The one row of `rows` whose columns match every element of `scenario`; else an error
## that lists what there is.
.archiveSelectScenario <- function(rows, scenario, file) {
  absent <- setdiff(names(scenario), names(rows))
  if (length(absent))
    stop("`archive$scenario` names column(s) that ", file, " does not have: ",
         paste(absent, collapse = ", "), call. = FALSE)
  keep <- rep(TRUE, NROW(rows))
  for (nam in names(scenario)) keep <- keep & as.character(rows[[nam]]) %in% as.character(scenario[[nam]])
  if (sum(keep) != 1L) {
    cols <- intersect(c("runName", names(scenario)), names(rows))
    listed <- as.data.frame(rows)[if (any(keep)) keep else TRUE, cols, drop = FALSE]
    stop("`archive$scenario` must identify one run of ", file, "; it matches ", sum(keep), ".\n",
         if (any(keep)) "The matches:\n" else "The runs:\n",
         paste(utils::capture.output(print(unique(listed), row.names = FALSE)), collapse = "\n"),
         call. = FALSE)
  }
  rows[keep, ]
}

## The modules the archived run ran, with their versions, from its saved simList. Only the
## simList shell and its manifest are fetched (no object, no output file: `outputsPattern`
## matches no file name). A parent module is not in a simList, only its children.
.archiveRanModules <- function(run, destDir) {
  sim <- reGetUntarLoad(run$driveId, destDir = destDir, projectPath = destDir,
                        pathRemap = c(old = run$outputPath, new = file.path(destDir, run$runName)),
                        remote = TRUE, parse = FALSE, outputsPattern = "^$", verbose = FALSE)[[1]]
  deps <- SpaDES.core::depends(sim)@dependencies
  data.frame(module = names(deps),
             version = vapply(deps, function(d) as.character(d@version), character(1)),
             row.names = NULL)
}

## Which of `modules` the archived run already ran: a module by name (versions are not
## compared), a parent when every module below it was. A parent's `childModules` are read from
## its local copy, else from its own .R file fetched from GitHub into `scratch`, never into
## `modulePath`.
.isArchivedModule <- function(module, ran, modulePath, scratch, seen = character()) {
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
  all(vapply(kids, .isArchivedModule, logical(1), ran = ran, modulePath = modulePath,
             scratch = scratch, seen = c(seen, name)))
}
