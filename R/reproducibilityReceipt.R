.spatialPkgsRegex <- paste(.spatialPkgs, collapse = "|")

#' Reproducibility receipt for Rmarkdown documents
#'
#' Insert git repository and R session info into Rmarkdown documents.
#' Based on suggestions in a Twitter thread by Miles McBain
#' (<https://twitter.com/MilesMcBain/status/1263272935197782016?s=20>).
#'
#' Add the following to your Rmd files (without the backslashes):
#'
#' \verb{
#' -----
#'
#' \```{r details, echo=FALSE}
#' pemisc::reproducibilityReceipt()
#' \```
#' }
#'
#' @param prjDir path to project directory
#' @param title Header title for the inserted details section.
#' @param writeTo If provided, an markdown filename to write to (e.g., `outputs/<runName>/INFO.md`).
#'                File path is assumed to be relative to `prjDir`.
#'
#' @export
#' @importFrom tools file_ext file_path_sans_ext
#' @seealso [`projectSessionInfo`]
#' @rdname reproducibilityReceipt
reproducibilityReceipt <- function(prjDir = NULL, title = "Reproducibility receipt", writeTo = NULL) {
  if (is.null(prjDir)) {
    prjDir <- findProjectPath()
  }

  rr <- if (requireNamespace("details", quietly = TRUE)) {
    details::details({
      projectSessionInfo(prjDir)
    }, summary = title)
  } else {
    stop("Suggested package 'details' is required.")
  }

  if (!is.null(writeTo)) {
    if (!identical(tools::file_ext(writeTo), "md")) {
      writeTo <- paste0(tools::file_path_sans_ext(writeTo), ".md")
    }

    writeTo <- normPath(file.path(prjDir, writeTo))

    if (file.exists(writeTo)) {
      stop("File ", writeTo, " exists and will not be overwritten.") ## TODO: allow append
    } else {
      cat(paste0("# ", title, "\n"), rr, file = writeTo, sep = "\n")
    }
    return(invisible(rr))
  } else {
    return(rr)
  }
}

#' Project session info
#'
#' Get detailed information about the software environment being used for a project,
#' including git repository information, externally installed spatial library info,
#' R session info, and timestamp.
#'
#' Can be displayed inside an Rmarkdown document using `[reproducibilityReceipt()]`,
#' saved inside the project directory, or added to a SpaDES `simList` to improve
#' reproducibility of these workflows.
#'
#' @param prjDir path to project directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## get project session info for current project
#' projectSessionInfo()
#'
#' ## replace default session info in a simList
#' mySimOut <- SpaDES.core::simInitAndSpades()
#' mySimOut@.xData[["._sessionInfo"]] <- projectSessionInfo()
#' }
projectSessionInfo <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- findProjectPath()
  }

  list(`Git repository` = gitInfo(prjDir),
       `External spatial libraries` = spatialLibs(),
       `R session info` = sessInfo(),
       `Timestamp` = timestamp())
}

#' @export
#' @rdname reproducibilityReceipt
gitInfo <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- findProjectPath()
  }

  cwd <- setwd(prjDir)
  on.exit(setwd(cwd), add = TRUE)
  local <- gsub("[*] ", "", grep("[*]", system(paste(Sys.which("git"), "branch"), intern = TRUE), value = TRUE))

  remote <- system(paste(Sys.which("git"), "remote -v"), intern = TRUE)
  remote <- strsplit(unique(gsub(" (.*)$", "", remote)), "\t")[[1]]
  remote <- paste0(local, " @ ", remote[1], " (", remote[2], ")")

  head <- system(paste(Sys.which("git"), "log -1 --format='[%h] %as: %s'"), intern = TRUE)

  list(Local = local,
       Remote = remote,
       Head = head,
       Submodules = submoduleInfo(prjDir))
}

#' @export
#' @importFrom data.table as.data.table rbindlist
#' @rdname reproducibilityReceipt
submoduleInfo <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- findProjectPath()
  }

  cwd <- setwd(prjDir)
  on.exit(setwd(cwd), add = TRUE)
  submodules <- system(paste(Sys.which("git"), "submodule status"), intern = TRUE)
  submodules <- rbindlist(lapply(strsplit(gsub("^ ", "", submodules), " "), function(m) {
    d <- as.data.table(t(m))
    colnames(d) <- c("commit", "directory", "ref")
    d
  }))

  submodules
}

#' @export
#' @inheritParams Require::Require
#' @rdname reproducibilityReceipt
sessInfo <- function(verbose = getOption("Require.verbose", 1L)) {
  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    sessioninfo::session_info()
  } else {
    messageVerbose("Suggested package 'sessioninfo' provides more useful session info.",
                   verbose = verbose)
    utils::sessionInfo()
  }
}

#' @export
#' @rdname reproducibilityReceipt
spatialLibs <- function() {
  if (requireNamespace("sf", quietly = TRUE) && any(grepl(.spatialPkgsRegex, sessInfo()))) {
    sf::sf_extSoftVersion()
  } else {
    NULL
  }
}

#' @export
#' @rdname reproducibilityReceipt
timestamp <- function() {
  Sys.time()
}
