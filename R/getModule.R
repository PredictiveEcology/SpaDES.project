utils::globalVariables(c(
  c("Account", "GitSubFolder", "Repo", "destFile", "filepath",
    "hasSubFolder", "repoLocation", "isGH", "canDownload", "OKtoDL",
    "downloaded", "hasVersionSpec", "inequ", "moduleFullName",
    "needDownload", "pkg", "status", "sufficient", "versionSpec",
    "modulesNoVersion", "modPath", "modDir")
))

#' Simple function to download a SpaDES module as GitHub repository
#'
#' @param modules Character vector of one or more github repositories as character strings that contain
#'   SpaDES modules. These should be presented in the standard R way, with
#'   `account/repository@branch`. If `account` is omitted, then `"PredictiveEcology` will
#'   be assumed.
#' @param overwrite A logical vector of same length (or length 1) \code{gitRepo}.
#'   If \code{TRUE}, then the download will delete any
#'   existing folder with the same name as the \code{repository}
#'   provided in \code{gitRepo}
#' @param modulePath A local path in which to place the full module, within
#'   a subfolder ... i.e., the source code will be downloaded to here:
#'   \code{file.path(modulePath, repository)}. If omitted, and `options(spades.modulePath)` is
#'   set, it will use `getOption("spades.modulePath")`, otherwise it will use `"."`.
#'
#' @details
#' A parent module lists its children in `childModules`. After a module is fetched (or
#' found locally), its children are fetched too, recursively: an entry written as
#' `"owner/repo@branch"` is fetched as written; a plain name (or `"name@branch"`) is fetched
#' from the parent's GitHub account (and branch, unless one is given). When the parent is
#' fetched at a version tag (e.g. `"owner/parent@v1.1.0"`), a plain-named child is fetched at
#' `v<version>` instead, its version taken from the parent's own `version` list at that tag
#' (e.g. `version = list(parent = "1.1.0", child = "2.2.0")` gives `child@v2.2.0`), so one
#' parent release names the release of every child. The same holds when the parent has no
#' ref (`"owner/parent"`): it comes from its default branch, normally its latest release, and
#' its children at the releases its list names. A child missing from that list, or listed at
#' a development version (four components, e.g. `"2.2.0.9000"`), falls back to the parent's ref. A module also named in
#' `modules` is fetched only as it is written there, never from a parent's entry.
#'
#' @return A list with `success` and `failed`, the module specifications (children included)
#'   that are, or are not, available locally.
#'
#' @export
#' @seealso [getGithubFile]
#' @include imports.R
#' @inheritParams Require::Require
#' @importFrom data.table rbindlist set
#' @importFrom Require checkPath extractPkgGitHub extractInequality extractVersionNumber
#' @importFrom Require normPath trimVersionNumber
#' @importFrom utils capture.output
getModule <- function(modules, modulePath, overwrite = FALSE,
                      verbose = getOption("Require.verbose", 1L)) {
  out <- .getModuleNoChildren(modules, modulePath, overwrite = overwrite, verbose = verbose)
  kids <- .getChildModules(out$success, explicit = modules, modulePath = modulePath,
                           overwrite = overwrite, verbose = verbose)
  list(success = c(out$success, kids$success), failed = c(out$failed, kids$failed))
}

.getModuleNoChildren <- function(modules, modulePath, overwrite = FALSE,
                                 verbose = getOption("Require.verbose", 1L)) {

  modulePath <- normPath(modulePath)
  modulePath <- checkPath(modulePath, create = TRUE)
  modulesOrig <- modules
  modNam <- extractPkgName(modules)
  m <- fileRelPathFromFullGHpath(modulesOrig)

  modulesOrigPkgName <- extractPkgName(modulesOrig)
  modulesOrigNestedName <- extractModName(modulesOrig)

  modPath <- whichModulePath(modulesOrigNestedName, modulePath)
  localExists <- dir.exists(file.path(modPath, modulesOrigNestedName)) # |
    # dir.exists(file.path(modulePath, m))

  stateDT <- data.table(moduleFullName = modules, modNam = extractPkgName(modules),
                        modPath = modPath, modDir = file.path(modPath, modulesOrigNestedName),
                        versionSpec = extractVersionNumber(modules),
                        modulesNoVersion = Require::trimVersionNumber(modules),
                        sufficient = NA, version = NA_character_,
                        localExists = localExists,
                        status = c(NA, "already local")[localExists + 1],
                        hasVersionSpec = !is.na(extractVersionNumber(modules)))

  stateDT[localExists & is.na(versionSpec), sufficient := TRUE]
  if (any(!overwrite %in% FALSE)) {
    if (is.logical(overwrite)) {
      mess <- paste(overwrite, collapse = ", ")
      modsToOverwrite <- unique(extractPkgName(modules[overwrite]))
      mess <- paste0("c(", mess, ")")
    } else {
      mess <- paste(overwrite, collapse = "', '")
      modsToOverwrite <- overwrite
      mess <- paste0("c('", mess, "')")
    }
    stateDT[localExists & (modNam %in% modsToOverwrite | moduleFullName %in% modsToOverwrite),
            sufficient := FALSE]
    if (isTRUE(any(stateDT$localExists)))
      messageVerbose("overwrite = ", mess,"; redownloading ", paste(modsToOverwrite, collapse = ", "))
  }

  ## Check the version of every local copy not already marked for overwrite.
  ## Only those rows: checkModuleVersion() sets `sufficient` on all it is given.
  toCheck <- stateDT$localExists %in% TRUE & !stateDT$sufficient %in% FALSE
  if (any(toCheck)) {
    messageVerbose("Local copies: ", verbose = verbose)
    stateDT <- rbindlist(list(checkModuleVersion(stateDT[toCheck], verbose = getOption("Require.verbose")),
                              stateDT[!toCheck]), fill = TRUE)
  }
  stateDT[localExists %in% FALSE | sufficient %in% FALSE, needDownload := TRUE]

  if (any(stateDT$needDownload %in% TRUE)) {
    modsToDL <- stateDT[stateDT$needDownload %in% TRUE]
    tmpdir <- file.path(tempdir(), .rndstr(1))
    Require::checkPath(tmpdir, create = TRUE)
    od <- setwd(tmpdir)
    on.exit(setwd(od))

    noAt <- stateDT[["needDownload"]] %in% TRUE & isGitHub(stateDT[["moduleFullName"]]) &
      !grepl("@", stateDT[["moduleFullName"]])
    if (any(noAt))
      stateDT[noAt %in% TRUE, moduleFullName := paste0(moduleFullName, "@HEAD")]
    stateDT[needDownload %in% TRUE,
            isGH := isGitHub(moduleFullName) & grepl("@", moduleFullName)] # the default isGitHub allows no branch]
    stateDT[, canDownload := needDownload %in% TRUE & isGH %in% TRUE]
    stateDT[needDownload %in% TRUE, OKtoDL := canDownload %in% TRUE]

    if (any(stateDT$OKtoDL %in% TRUE)) {
      stateDT[OKtoDL %in% TRUE, c("acct", "repo", "br") := {
        ## Take the three fields by NAME. Dropping `versionSpec` and assigning
        ## whatever is left assumes splitGitRepo() returns exactly four
        ## elements; it now also returns `subFolder`, which made this "Supplied
        ## 3 columns to be assigned 4 items".
        a <- splitGitRepo(modulesNoVersion)
        lapply(a[c("acct", "repo", "br")], unlist)
      }
      ]

      stateDT[OKtoDL %in% TRUE, {
        downloadGHRepoOuter(modToDL = moduleFullName[[1]],
                            overwrite = OKtoDL[[1]],
                            modulePath = modPath[[1]],
                            verbose = verbose)
      }
      , by = c("acct", "repo")] # if there is one large repository with many SpaDES modules, download only once

      stateDT[OKtoDL %in% TRUE, downloaded :=
                dir.exists(file.path(modPath, Require::extractPkgName(moduleFullName)))]

      if (any(stateDT$downloaded %in% TRUE)) {
        messageVerbose("Downloaded copies: ", verbose = verbose)
        downloadedDT <- split(stateDT, by = "downloaded")
        downloadedDT[["TRUE"]] <- checkModuleVersion(downloadedDT[["TRUE"]], verbose = getOption("Require.verbose"))
        stateDT <- rbindlist(downloadedDT, fill = TRUE)
      }
      stateDT[sufficient %in% TRUE & downloaded %in% TRUE, status := "downloaded"]
      stateDT[sufficient %in% FALSE & downloaded %in% TRUE, status := "downloaded but incorrect version"]
      stateDT[sufficient %in% FALSE & !downloaded %in% TRUE, status := "failed"]
    }

    if (any(stateDT$OKtoDL %in% FALSE)) {
      stop("These modules -- ", green(paste0(stateDT$moduleFullName[stateDT$OKtoDL %in% FALSE], collapse = ", ")),
           " -- do not exist locally and are not specified as GitHub repositories (with an '@' for branch);\n",
           "Please point to existing local modules or correct the GitHub specification")
    }
  }

  successes <- stateDT$moduleFullName[stateDT$sufficient %in% TRUE]
  failed <- stateDT$moduleFullName[!stateDT$sufficient %in% TRUE]
  df <- stateDT[, list(moduleFullName, status, modulePath = modDir)]
  messageDF(df, verbose = verbose)

  return(list(success = successes, failed = failed))
}




# PredictiveEcology/LandWeb/master/01-init.R


#' A simple way to get a Github file, authenticated
#'
#' This can be used within e.g., the `options` or `params` arguments for
#' `setupProject` to get a ready-made file for a project.
#'
#' @export
#' @param gitRepoFile Character string that follows the convention
#'   *GitAccount/GitRepo@Branch/File*, if @Branch is omitted, then it will be
#'   assumed to be `master` or `main`.
#' @param destDir A directory to put the file that is to be downloaded.
#' @inheritParams getModule
#' @seealso [getModule]
#' @examples
#' filename <- getGithubFile("PredictiveEcology/LandWeb@development/01b-options.R",
#'                           destDir = Require::tempdir2())
getGithubFile <- function(gitRepoFile, overwrite = FALSE, destDir = ".",
                          verbose = getOption("Require.verbose")) {
  gitRepo <- extractGitHubRepoFromFile(gitRepoFile)
  file <- extractGitHubFileRelativePath(gitRepoFile, gitRepo)
  if (nchar(dirname(file)))
    checkPath(file.path(destDir, dirname(file)), create = TRUE)

  out <- downloadFile(gitRepo, file, overwrite = overwrite, destDir = destDir,
                      verbose = verbose)
  if (!isTRUE(out))
    messageVerbose("  ... Did not download ", file, verbose = verbose)
  else {
    messageVerbose("downloaded ", file, verbose = verbose)
  }
  out <- if (!is.null(out))
    names(out)
  else
    normPath(file.path(destDir, file))
  return(out)
}

extractGitHubFileRelativePath <- function(gitRepoFile, gitRepo) {
  if (missing(gitRepo))
    gitRepo <- extractGitHubRepoFromFile(gitRepoFile)
  file <- gsub(gitRepo, "", gitRepoFile)
  gsub("^\\/", "", file) # file is now relative path
}

extractGitHubRepoFromFile <- function(gitRepoFile) {
  gitRepo <- splitGitRepo(gitRepoFile)
  file.path(gitRepo$acct, paste0(gitRepo$repo, "@", gitRepo$br))
}


#' @importFrom Require .downloadFileMasterMainAuth
downloadFile <- function(gitRepo, file, overwrite = FALSE, destDir = ".",
                         verbose = getOption("Require.verbose")) {
  tryDownload <- TRUE

  localFile <- stripQuestionMark(file)

  out <- NULL
  if (file.exists(file)) # file is expected to be relative path
    if (overwrite %in% FALSE) {
      messageVerbose(file, " already exists and overwrite = FALSE", verbose = verbose)
      tryDownload <- FALSE
    }

  if (isTRUE(tryDownload)) {
    destDir <- checkPath(destDir, create = TRUE)
    gr <- splitGitRepo(gitRepo)
    ar <- file.path(gr$acct, gr$repo)
    masterMain <- c("main", "master")
    br <- if (any(gr$br %in% masterMain)) {
      # possibly change order -- i.e., put user choice first
      masterMain[rev(masterMain %in% gr$br + 1)]
    } else {
      gr$br
    }

    url <- file.path(rawGithubDotCom, ar, br, file)
    tf <- tempfile()
    out <- suppressWarnings(
      try(
        .downloadFileMasterMainAuth(url, destfile = tf, need = "master"), silent = FALSE)
    )
    if (is(out[[1]], "try-error")) {
      warn <- gsub("(https://)(.+)(raw)", "\\1\\3", out[[1]][1])
      warning(warn, "\nIs the url misspelled or unavailable?")
    }
    if (file.exists(tf)) {
      file <- file.path(destDir, localFile)
      out <- file.copy(tf, file, overwrite = TRUE)
      names(out) <- file
    }

  }
  out

}

# For each module, the first of `modulePath` that contains it; `modulePath[1]`
# (where a download goes) if none does. `modulePath` may have several entries.
whichModulePath <- function(modules, modulePath) {
  vapply(modules, function(mod) {
    modulePath[c(which(dir.exists(file.path(modulePath, mod))), 1L)[1]]
  }, character(1), USE.NAMES = FALSE)
}

checkModuleVersion <- function(stateDT, verbose = getOption("Require.verbose")) {
  stateDT$moduleFullName
  set(stateDT, NULL, "hasVersionSpec", !is.na(stateDT$versionSpec))
  stateDT[!sufficient %in% TRUE, sufficient := !hasVersionSpec]

  if (any(stateDT$hasVersionSpec)) {
    stateDT[hasVersionSpec %in% TRUE,
            `:=`(inequ = extractInequality(moduleFullName),
                 pkg = extractPkgGitHub(moduleFullName))]
    stateDT[hasVersionSpec %in% TRUE,
            `:=`(version = vapply(seq_along(pkg), function(i)
              as.character(metadataInModules(modules = pkg[i], metadataItem = "version",
                                             modulePath = modPath[i])), character(1)))]
    stateDT[hasVersionSpec %in% TRUE,
            sufficient := compareVersion2(as.character(version),
                            versionSpec = versionSpec[hasVersionSpec], inequality = inequ)]
    # versionSpec <- extractVersionNumber(moduleFullName[hasVersionSpec])
    #inequ <- extractInequality(moduleFullName[hasVersionSpec])
    #pkg <- extractPkgGitHub(moduleFullName[hasVersionSpec])
    #version <- metadataInModules(modules = pkg, metadataItem = "version", modulePath = modulePath)
    #sufficient[hasVersionSpec] <- compareVersion2(as.character(version),
    #                              versionSpec = versionSpec[hasVersionSpec], inequality = inequ)
    if (any(stateDT$sufficient %in% TRUE))
      messageVerbose("  Version OK for: ",
                     paste(stateDT$moduleFullName[stateDT$sufficient %in% TRUE],
                           collapse = ", "), verbose = verbose)
    if (any(!(stateDT$sufficient %in% TRUE)))
      messageVerbose("  Version not OK for: ",
                     paste(stateDT$moduleFullName[stateDT$sufficient %in% FALSE],
                           collapse = ", "), verbose = verbose)

  }
  stateDT[]
}

stripQuestionMark <- function(file) {
  gsub("\\?.+$", "", file)
}



downloadGHRepoOuter <- function(modToDL, verbose, overwrite, modulePath) {
  dd <- .rndstr(1)
  modNameShort <- Require::extractPkgName(modToDL)
  Require::checkPath(dd, create = TRUE)
  messageVerbose(modToDL, " ...", verbose = verbose)
  isGH <- isGitHub(modToDL) && grepl("@", modToDL) # the default isGitHub allows no branch

  if (isGH) {

    mess <- capture.output(type = "message",
                           out <- withCallingHandlers({
                             downloadRepo(modToDL, subFolder = NA,
                                          destDir = dd, overwrite = overwrite,
                                          verbose = verbose + 1)},
                             warning = function(w) {
                               warns <- grep("No such file or directory|extracting from zip file", w$message,
                                             value = TRUE, invert = TRUE)
                               if (length(warns))
                                 warning(warns)
                               invokeRestart("muffleWarning")
                             }
                           ))
    files <- dir(file.path(dd, modNameShort), recursive = TRUE)
    if (length(files)) {
      newFiles <- file.path(modulePath, modNameShort, files)
      out <- lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
      fromFiles <- file.path(dd, modNameShort, files)
      toFiles <- file.path(modulePath, modNameShort, files)
      if (isTRUE(any(overwrite %in% TRUE)))
        unlink(toFiles)
      out <- linkOrCopy(fromFiles, toFiles)
      messageVerbose("\b Done!", verbose = verbose)

    } else {
      messageVerbose("\b could not be downloaded; does it exist? and are permissions correct?",
                     verbose = verbose)
    }
  } else {
    messageVerbose(modToDL, " could not be found locally (in ",
                   file.path(modulePath, modToDL),
                   "; if this is a GitHub module, please specify @Branch ",
                   "using format: GitAccount/GitRepo@Branch", "\n --> does it exist on GitHub.com? and are permissions correct?",
                   verbose = verbose)
  }
}

## The `childModules` entries of each module in `modules` (names or specs), as written.
.childModuleEntries <- function(modules, modulePath) {
  unlist(lapply(extractModName(modules), function(mod) {
    kids <- metadataInModules(modules = mod, metadataItem = "childModules",
                              modulePath = whichModulePath(mod, modulePath), verbose = -1)
    kids <- as.character(unlist(kids, use.names = FALSE))
    kids[!is.na(kids) & nzchar(kids)]
  }), use.names = FALSE)
}

## Where to fetch a child from: an "owner/repo..." entry as written; a plain name, or
## "name@branch", from the parent's account and (unless given) branch. A parent that is
## local only, or nested in another repository, leaves its plain children as they are.
.childModuleSpec <- function(kid, parent, versions = NULL) {
  if (grepl("/", sub("@.*$", "", kid))) return(kid)
  parent <- trimVersionNumber(parent)
  if (!isGitHub(parent)) return(kid)
  gr <- lapply(splitGitRepo(parent)[c("acct", "br", "subFolder")], unlist)
  if (!is.na(gr$subFolder)) return(kid)
  kidName <- sub("@.*$", "", kid)
  br <- if (grepl("@", kid)) {
    sub("^[^@]*@", "", kid)
  } else if ((.isVersionTag(gr$br) || identical(unname(gr$br), "HEAD")) &&
             .isReleaseVersion(versions[kidName])) {
    ## a parent release (a version tag, or no ref: its default branch, i.e. its latest
    ## release) names each child's release in its own `version` list
    paste0("v", versions[[kidName]])
  } else {
    gr$br
  }
  paste0(gr$acct, "/", kidName, "@", br)
}

## A ref that is a release tag, "v" then a version ("v1.1.0"), rather than a branch.
.isVersionTag <- function(br) isTRUE(grepl("^v[0-9]+([.-][0-9]+)*$", br))

## A release version ("2.2.0"), which has a `v` tag; a development version ("2.2.0.9000",
## four or more components) does not.
.isReleaseVersion <- function(v) {
  length(v) == 1L && !is.na(v) && grepl("^[0-9]+([.-][0-9]+){0,2}$", v)
}

## The parent's `version` list as a named character vector (module name -> version), read
## from its local copy. metadataInModules(metadataItem = "version") drops the names.
.moduleVersions <- function(module, modulePath) {
  module <- extractModName(module)
  f <- file.path(whichModulePath(module, modulePath), module, paste0(module, ".R"))
  if (!file.exists(f)) return(NULL)
  pp <- parse(file = f, keep.source = FALSE)
  dm <- pp[[grep("^defineModule", vapply(pp, function(x) deparse(x)[1], character(1)))[1]]]
  v <- try(eval(as.list(dm[[3]])$version, envir = baseenv()), silent = TRUE)
  if (inherits(v, "try-error") || !is.list(v) || is.null(names(v))) return(NULL)
  vapply(v, function(x) as.character(x), character(1))
}

## Children kept in the parent's own repository, in its `modules/` folder (as in
## PredictiveEcology/scfm, whose root is the parent): the parent's copy already holds them
## at the parent's ref, so each is placed beside the parent, where simInit() looks for it,
## instead of being fetched. Returns the placed children as "<parent>/modules/<child>",
## with the parent's ref, so each names where it came from.
.placeInRepoChildren <- function(entries, parent, modulePath, overwrite = FALSE) {
  parName <- extractModName(parent)
  parPath <- whichModulePath(parName, modulePath)
  src <- file.path(parPath, parName, "modules", entries)
  inRepo <- !grepl("[/@]", entries) & dir.exists(src)
  if (!any(inRepo)) return(character())
  for (i in which(inRepo)) {
    to <- file.path(parPath, entries[i])
    if (dir.exists(to) && !isTRUE(overwrite)) next
    files <- dir(src[i], recursive = TRUE, all.files = TRUE)
    toFiles <- file.path(to, files)
    lapply(unique(dirname(toFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
    unlink(toFiles)
    linkOrCopy(file.path(src[i], files), toFiles)
  }
  paste0(trimVersionNumber(parent), "/modules/", entries[inRepo])
}

## Fetch the children of `parents`, then theirs, and so on. `explicit` (the modules the
## user asked for) are never refetched from a parent's entry; `seen` stops a cycle.
.getChildModules <- function(parents, explicit, modulePath, overwrite = FALSE,
                             verbose = getOption("Require.verbose", 1L)) {
  ## a logical `overwrite` per explicit module does not apply to children
  if (is.logical(overwrite) && length(overwrite) != 1) overwrite <- FALSE
  seen <- if (length(explicit)) extractModName(explicit) else character()
  success <- failed <- character()
  while (length(parents)) {
    kids <- placed <- character()
    for (par in parents) {
      entries <- .childModuleEntries(par, modulePath)
      entries <- entries[!extractModName(entries) %in% seen]
      here <- .placeInRepoChildren(entries, par, modulePath, overwrite = overwrite)
      placed <- c(placed, here)
      seen <- c(seen, extractModName(here))
      entries <- entries[!entries %in% extractModName(here)]
      kids <- c(kids, vapply(entries, .childModuleSpec, character(1), parent = par,
                             versions = .moduleVersions(par, modulePath), USE.NAMES = FALSE))
    }
    if (length(placed))
      messageVerbose("Child modules from the parent's repository: ",
                     paste(placed, collapse = ", "), verbose = verbose)
    kidNames <- extractModName(kids)
    keep <- !kidNames %in% seen & !duplicated(kidNames)
    kids <- kids[keep]
    seen <- c(seen, kidNames[keep])
    out <- list(success = character(), failed = character())
    if (length(kids)) {
      messageVerbose("Child modules: ", paste(kids, collapse = ", "), verbose = verbose)
      out <- .getModuleNoChildren(kids, modulePath, overwrite = overwrite, verbose = verbose)
    }
    success <- c(success, placed, out$success)
    failed <- c(failed, out$failed)
    parents <- c(placed, out$success)
  }
  list(success = success, failed = failed)
}

## Names of all modules below `module` (children, grandchildren, ...), as found locally.
.descendantModules <- function(module, modulePath, seen = character()) {
  kids <- setdiff(extractModName(.childModuleEntries(module, modulePath)), c(seen, module))
  if (!length(kids)) return(character())
  seen <- c(seen, module, kids)
  unique(c(kids, unlist(lapply(kids, .descendantModules, modulePath = modulePath, seen = seen))))
}
