utils::globalVariables(c(
  c("Account", "GitSubFolder", "Repo", "destFile", "filepath",
    "hasSubFolder", "repoLocation")
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
#' @inheritParams Require::Require
#' @importFrom utils capture.output
#' @importFrom Require checkPath normPath trimVersionNumber extractPkgGitHub extractInequality compareVersion2
getModule <- function(modules, modulePath, overwrite = FALSE,
                      verbose = getOption("Require.verbose", 1L)) {

  modulePath <- normPath(modulePath)
  modulePath <- checkPath(modulePath, create = TRUE)
  anyfailed <- character()
  modulesOrig <- modules
  modNam <- extractPkgName(modules)
  whExist <- dir.exists(file.path(modulePath, modNam))

  modsToDL <- modules

  if (overwrite %in% FALSE) {
    if (any(whExist)) {
      modToDLnoVersion <- Require::trimVersionNumber(modsToDL[whExist])
      hasVersionSpec <- modToDLnoVersion != modsToDL[whExist]
      if (any(hasVersionSpec)) {
        versionSpec <- extractVersionNumber(modsToDL[whExist][hasVersionSpec])
        inequ <- extractInequality(modsToDL[whExist][hasVersionSpec])
        pkg <- extractPkgGitHub(modsToDL[whExist][hasVersionSpec])
        version <- mapply(module = pkg, function(module) {
          ver <- moduleMetadata(module = module, path = modulePath, defineModuleListItems = "version")
          as.character(ver$version)
        })
        sufficient <- compareVersion2(as.character(version),
                                      versionSpec = versionSpec, inequality = inequ)
        if (any(sufficient))
          messageVerbose("Local version sufficient for: ",
                         paste(modsToDL[whExist %in% TRUE][sufficient %in% TRUE],
                               collapse = ", "),
                         "\n... not downloading", verbose = verbose)
        modsToDL <- modToDLnoVersion[whExist %in% TRUE][sufficient %in% FALSE]

      }
    }

    # append modules that exist and fail test with modules don't exist
    modsToDL <- unique(c(modsToDL, modules[whExist %in% FALSE]))

  }
  if (length(modsToDL)) {
    tmpdir <- file.path(tempdir(), .rndstr(1))
    Require::checkPath(tmpdir, create = TRUE)
    od <- setwd(tmpdir)
    on.exit(setwd(od))

    out <-
      Map(modToDL = modsToDL, function(modToDL) {
        dd <- .rndstr(1)
        modNameShort <- Require::extractPkgName(modToDL)
        Require::checkPath(dd, create = TRUE)
        messageVerbose(modToDL, " ...", verbose = verbose)

        mess <- capture.output(type = "message",
                       out <- withCallingHandlers(
                         downloadRepo(modToDL, subFolder = NA,
                               destDir = dd, overwrite = overwrite,
                               verbose = verbose + 1),
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
          file.copy(file.path(dd, modNameShort, files),
                    file.path(modulePath, modNameShort, files), overwrite = TRUE)
          messageVerbose("\b Done!", verbose = verbose)

        } else {
          messageVerbose(modToDL, " could not be downloaded; does it exist? and are permissions correct?",
                         verbose = verbose)
        }

      })
    allworked <- Require::extractPkgName(modsToDL) %in% dir(modulePath)
    anyfailed <- modsToDL[!allworked]
    modules <- anyfailed
  }

  successes <- setdiff(modulesOrig, anyfailed)
  if (length(successes)) {
    df <- data.frame(modules = modulesOrig)
    df[match(successes, df$modules), "downloaded"] <- TRUE
    df[match(successes, df$modules), "modulePath"] <- normPath(modulePath)

    messageDF(df)
    if (length(anyfailed)) {
      messageVerbose("Will try using `git clone` ... ",
                     verbose = verbose)
    }
  }

  return(list(success = successes, failed = anyfailed))
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
#' @inheritParams getModule
#' @seealso [getModule]
#' @examples
#' \donttest{
#'   getGithubFile("PredictiveEcology/LandWeb@development/04-options.R")
#' }
getGithubFile <- function(gitRepoFile, overwrite = FALSE, destDir = ".",
                          verbose = getOption("Require.verbose")) {
  gitRepo <- splitGitRepo(gitRepoFile)
  gitRepo <- file.path(gitRepo$acct, paste0(gitRepo$repo, "@", gitRepo$br))
  file <- gsub(gitRepo, "", gitRepoFile)
  file <- gsub("^\\/", "", file)
  if (nchar(dirname(file)))
    checkPath(dirname(file), create = TRUE)

  out <- downloadFile(gitRepo, file, overwrite = overwrite, destDir = ".",
                           verbose = getOption("Require.verbose"))
  if (!isTRUE(out))
    out <- "Did not download"
  else {
    messageVerbose("downloaded ", file)
  }
  out <- normPath(file)
  return(out)
}


downloadFile <- function(gitRepo, file, overwrite = FALSE, destDir = ".",
                         verbose = getOption("Require.verbose")) {
  tryDownload <- TRUE
  if (file.exists(file))
    if (overwrite %in% FALSE) {
      messageVerbose(file, "
                     already exists and overwrite = FALSE")
      tryDownload <- FALSE
    }

  if (isTRUE(tryDownload)) {
    dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
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
      try(downloadFileMasterMainAuth(url, destfile = tf, need = "master"), silent = TRUE)
    )
    if (file.exists(tf)) {
      file.copy(tf, file, overwrite = TRUE)
    }

  }

}
