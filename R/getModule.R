#' Simple function to download a SpaDES module as GitHub repository
#'
#' @param ... One or more github repositories as character strings that contain
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
#' @importFrom utils download.file unzip
#' @importFrom Require getGitHubFile isWindows
getModule <- function(..., overwrite = FALSE, modulePath) {

  gitRepo = unlist(list(...))
  if (missing(modulePath)) modulePath <- getOption("spades.modulePath")
  if (is.null(modulePath)) modulePath <- "."
  if (!dir.exists(modulePath)) dir.create(modulePath, recursive = TRUE)
  out <- Map(gitRep = gitRepo, overwriteInner = overwrite, function(gitRep, overwriteInner) {

    gitRepOrig <- gitRep
    vn <- Require::extractVersionNumber(gitRep)
    inequ <- Require::extractInequality(gitRep)
    if (!is.na(vn)) {
      gitRep <- gsub(" *\\(.+", "", gitRep)
    }
    gr <- splitGitRepo(gitRep)
    ar <- file.path(gr$acct, gr$repo)
    repoFull <- file.path(modulePath, gr$repo)
    repoFullNormalized <- normalizePath(repoFull, mustWork = FALSE, winslash = "/")

    if (dir.exists(repoFull)) {
      versionOK <- FALSE
      if (!is.na(vn)) {
        fn <- getGitHubFile(paste0(gr$acct, "/", gr$repo, "@", gr$br), filename = paste0(gr$repo, ".R"))
        dircreated <- Require::checkPath(file.path(dirname(fn$destFile), gr$repo), create = TRUE)
        newTempName <- file.path(dircreated, paste0(gr$repo, ".R"))
        file.rename(fn$destFile, newTempName)
        verOnline <- metadataInModules(gr$repo, "version", modulePath = dirname(dircreated))
        verInstalled <- metadataInModules(gr$repo, "version", modulePath = modulePath)
        if (!is.null(verInstalled)) {
          compVersOnline <- compareVersion(as.character(verOnline[[gr$repo]]), vn)
          compVersInstalled <- compareVersion(as.character(verInstalled[[gr$repo]]), vn)
          versionOnlineOK <- eval(parse(text = paste0(compVersOnline, inequ, 0)))
          if (!versionOnlineOK) message("Version request cannot be satisfied at ", gitRepOrig)
          versionOK <- eval(parse(text = paste0(compVersInstalled, inequ, 0)))
        } else {
          versionOK <- FALSE
        }
      }
      if (isTRUE(overwriteInner) && !versionOK) {
        message(repoFullNormalized, " exists; overwriting")
        unlink(repoFullNormalized, recursive = TRUE)
      } else {
        if (versionOK) {
          message(
            repoFullNormalized,
            " directory already exists, overwrite = TRUE, but version number is OK. Not overwriting. ",
            "To overwrite, either delete the module manually, change the minimum version number, ",
            "or remove version number comparison"
          )
        } else {
          message(repoFullNormalized, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
        }
        return(repoFullNormalized)
      }
    }

    zipFileName <- normalizePath(paste0(repoFull, ".zip"), winslash = "/", mustWork = FALSE)
    for (i in 1:2) {
      url <- paste0("http://github.com/", ar, "/archive/", gr$br, ".zip")
      suppressWarnings(out <- try(download.file(url, destfile = zipFileName), silent = TRUE))
      if (is(out, "try-error") && identical(gr$br, "main")) {
        gr$br <- "master"
      } else {
        break
      }
    }
    out <- unzip(zipFileName, exdir = modulePath) # unzip it

    if (!is.null(out)) {
      dirnames <- dirname(out)
      badDirname <- unique(dirnames)[which.min(nchar(unique(dirnames)))]
      file.rename(badDirname, gsub(basename(badDirname), gr$repo, badDirname)) # it was downloaded with a branch suffix
      message(gitRep, " downloaded and placed in ", repoFullNormalized)
    } else {
      warning("The zipfile: ", zipFileName, " failed to unzip for unknown causes")
    }
    unlink(zipFileName)
    # possRmd <- normalizePath(winslash = "/", file.path(repoFull, paste0(gr$repo, ".Rmd")), mustWork = FALSE)
    # if (file.exists(possRmd)) {
    #   message("To run it, try: \nfile.edit('", possRmd, "')")
    # }
    return(repoFullNormalized)
  })

  return(out)
}
