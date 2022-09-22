.spatialPkgs <- utils::getFromNamespace(".spatialPkgs", "Require")
.spatialPkgs <- .spatialPkgs[-which(.spatialPkgs %in% c("units"))]
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
#'
#' @export
#' @importFrom rprojroot find_root is_rstudio_project
reproducibilityReceipt <- function(prjDir = NULL, title = "Reproducibility receipt") {
  if (is.null(prjDir)) {
    prjDir <- find_root(is_rstudio_project, path = prjDir)
  }

  if (requireNamespace("details", quietly = TRUE)) {
    details::details({
      projectSessionInfo(prjDir)
    }, summary = title)
  } else {
    stop("Suggested package 'details' is required.")
  }
}

#' @export
#' @importFrom rprojroot find_root is_rstudio_project
projectSessionInfo <- function(prjDir) {
  if (is.null(prjDir)) {
    prjDir <- find_root(is_rstudio_project, path = prjDir)
  }

  list(`Git repository` = gitInfo(prjDir),
       `External spatial libraries` = spatialLibs(),
       `R session info` = sessInfo(),
       `Timestamp` = timestamp())
}

#' @export
#' @importFrom rprojroot find_root is_git_root
#' @rdname reproducibilityReceipt
gitInfo <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- find_root(is_git_root, path = prjDir)
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
#' @importFrom rprojroot find_root is_git_root
#' @rdname reproducibilityReceipt
submoduleInfo <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- find_root(is_git_root, path = prjDir)
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
#' @rdname reproducibilityReceipt
sessInfo <- function() {
  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    sessioninfo::session_info()
  } else {
    message("Suggested package 'sessioninfo' provides more useful session info.")
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
