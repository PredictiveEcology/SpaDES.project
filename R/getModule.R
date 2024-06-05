utils::globalVariables(c(
  c("Account", "GitSubFolder", "Repo", "destFile", "filepath",
    "hasSubFolder", "repoLocation",
    "downloaded", "hasVersionSpec", "inequ", "moduleFullName",
    "needDownload", "pkg", "status", "sufficient", "versionSpec",
    "modulesNoVersion")
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


  modulePath <- normPath(modulePath)
  modulePath <- checkPath(modulePath, create = TRUE)
  modulesOrig <- modules
  modNam <- extractPkgName(modules)
  localExists <- dir.exists(file.path(modulePath, modNam))

  stateDT <- data.table(moduleFullName = modules, modNam = extractPkgName(modules),
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

  if (all(!overwrite %in% FALSE)) {
    if (any(stateDT$localExists %in% TRUE & !stateDT$sufficient %in% FALSE)) {
      messageVerbose("Local copies: ", verbose = verbose)
      stateDT <- checkModuleVersion(stateDT, modulePath, verbose = getOption("Require.verbose"))
    }

  }
  stateDT[localExists %in% FALSE | sufficient %in% FALSE, needDownload := TRUE]
  if (any(stateDT$needDownload %in% TRUE)) {
    tmpdir <- file.path(tempdir(), .rndstr(1))
    Require::checkPath(tmpdir, create = TRUE)
    od <- setwd(tmpdir)
    on.exit(setwd(od))

    stateDT[needDownload %in% TRUE, c("acct", "repo", "br") := {
      a <- splitGitRepo(modulesNoVersion)
      a[["versionSpec"]] <- NULL
      lapply(a, unlist)
    }
    ]

    stateDT[needDownload %in% TRUE, {
      downloadGHRepoOuter(modToDL = moduleFullName[[1]],
                          overwrite = needDownload[[1]],
                          modulePath = modulePath,
                          verbose = verbose)
    }
    , by = c("acct", "repo")] # if there is one large repository with many SpaDES modules, download only once

    stateDT[needDownload %in% TRUE, downloaded :=
              Require::extractPkgName(moduleFullName) %in% dir(modulePath)]

    if (any(stateDT$downloaded %in% TRUE)) {
      messageVerbose("Downloaded copies: ", verbose = verbose)
      downloadedDT <- split(stateDT, by = "downloaded")
      downloadedDT[["TRUE"]] <- checkModuleVersion(downloadedDT[["TRUE"]], modulePath, verbose = getOption("Require.verbose"))
      stateDT <- rbindlist(downloadedDT)
    }
    stateDT[sufficient %in% TRUE & downloaded %in% TRUE, status := "downloaded"]
    stateDT[sufficient %in% FALSE & downloaded %in% TRUE, status := "downloaded but incorrect version"]
    stateDT[sufficient %in% FALSE & !downloaded %in% TRUE, status := "failed"]
  }

  successes <- stateDT$moduleFullName[stateDT$sufficient %in% TRUE]
  failed <- stateDT$moduleFullName[!stateDT$sufficient %in% TRUE]
  stateDT[, modulePath := file.path(modulePath, fileRelPathFromFullGHpath(stateDT$moduleFullName))]
  df <- stateDT[, list(moduleFullName, status, modulePath)]
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
    url <- file.path("https://raw.githubusercontent.com/", ar, br, file)
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

checkModuleVersion <- function(stateDT, modulePath, verbose = getOption("Require.verbose")) {
  stateDT$moduleFullName
  set(stateDT, NULL, "hasVersionSpec", !is.na(stateDT$versionSpec))
  stateDT[!sufficient %in% TRUE, sufficient := !hasVersionSpec]

  if (any(stateDT$hasVersionSpec)) {
    stateDT[hasVersionSpec %in% TRUE,
            `:=`(inequ = extractInequality(moduleFullName),
                 pkg = extractPkgGitHub(moduleFullName))]
    stateDT[hasVersionSpec %in% TRUE,
            `:=`(version = as.character(metadataInModules(modules = pkg, metadataItem = "version", modulePath = modulePath)))]
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

