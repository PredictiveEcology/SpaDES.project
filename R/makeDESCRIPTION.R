#' Make DESCRIPTION file(s) from SpaDES module metadata
#'
#' @param modules A character vector of module names
#' @param modulePath Character. The path with modules, usually `modulePath()` or `paths$modulePath`
#' @param projectPath Character. Only used if `singleDESCRIPTION = TRUE`
#' @param singleDESCRIPTION Logical. If `TRUE`, there be only one DESCRIPTION file written
#'   for all modules, i.e., all reqdPkgs will be trimmed for redundancies and put into the
#'   single project-level DESCRIPTION file.
#' @param package The name inserted into the "Package" entry in DESCRIPTION
#' @param title The string inserted into the "Title" entry in DESCRIPTION
#' @param description The string inserted into the "Description" entry in DESCRIPTION
#' @param version The string inserted into the "Version" entry in DESCRIPTION
#' @param authors The string inserted into the "Authors" entry in DESCRIPTION
#' @param write Logical. If `TRUE`, then it will write the DESCRIPTION file either in
#'   the `modulePath` (if `singleDESCRIPTION = FALSE`) or `projectPath`
#'   (if `singleDESCRIPTION = TRUE`)
#' @rdname makeDESCRIPTION
makeDESCRIPTIONproject <- function(modules, modulePath, projectPath = ".", singleDESCRIPTION = TRUE,
                                   package = "Project",
                                   title = "Project", description = "Project",
                                   version = "1.0.0", authors = Sys.info()["user"], write = TRUE,
                                   verbose = getOption("Require.verbose")) {

  makeDESCRIPTION(modules, modulePath, projectPath, singleDESCRIPTION, package = package, title = title,
                  description = description,
                  version = version, authors = authors, write = write, verbose = verbose)
}

#' @rdname makeDESCRIPTION
makeDESCRIPTION <- function(modules, modulePath, projectPath = ".", singleDESCRIPTION = FALSE, package, title, date, description,
                            version, authors, write = TRUE, verbose, metadataList, ...) {

  if (missing(metadataList)) {
    mainModuleFile <- file.path(modulePath, unlist(modules), paste0(unlist(modules), ".R"))
    packageFolderName <- dirname(mainModuleFile)
    metadataList <- lapply(mainModuleFile, function(file) parse(file, keep.source = TRUE))
  }
  defModule <- lapply(metadataList, function(x) grepl(pattern = "^defineModule", x[[1]]))
  whDefModule <- lapply(defModule, function(x) which(x[[1]]))
  mds <- Map(whDefMod = whDefModule, defMod = metadataList, function(whDefMod, defMod) {
    defMod[[whDefMod]][[3]]
  }
  )

  names(mds) <- modules
  mods <-if (singleDESCRIPTION)  "Project" else modules
  for (module in mods) {
    md <- mds[[module]]
    d <- list()
    d$Package <- if (!missing(package)) package else .moduleNameNoUnderscore(module)
    d$Type <- "Package"

    d$Title <- if (!missing(title)) title else md$name
    d$Description <- if (!missing(description)) description else md$description
    # d$Description <- md$description
    d$Version <- if (!missing(version)) version else md$version
    # d$Version <- as.character(eval(md$version[[2]]))
    d$Date <- if (!missing(date)) date else format(Sys.Date())
    # d$Date <- Sys.Date()
    d$Authors <- if (!missing(authors)) authors else md$authors
    # d$Authors <- md$authors
    d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
  }

  # importsAll <- character()
  pfnAll <- character()

  pfnAllList <- Map(md = mds, function(md) {
  #   md <- mds[[module]]
    pkgFullName <- unlist(eval(md$reqdPkgs))

    if (singleDESCRIPTION) {
      pfnAll <- c(pfnAll, pkgFullName)
    } else {
      pfnAll <- pkgFullName
    }
    Require:::toPkgDTFull(pfnAll)
  })

  if (singleDESCRIPTION) {
    pfnAllList <- list(rbindlist(pfnAllList, fill = TRUE, use.names = TRUE))
  }

  dFiles <- Map(pfnAll = pfnAllList, function(pfnAll) {

    pfnAll <- Require:::trimRedundancies(pfnAll)
    # if ()
    pfnAll[, hasHEAD := grepl("\\(HEAD\\)", packageFullName)]
    whHEAD <- grep("\\(HEAD\\)", pfnAll$packageFullName)
    if (length(whHEAD)) {
      pkgDT <- Require:::getVersionOnRepos(pfnAll[whHEAD], repos = getOption("repos"), purge = FALSE)
      pkgDT[which(hasHEAD), packageFullName := gsub("HEAD", paste0(">=", VersionOnRepos), packageFullName)]
      pfnAll[pkgDT[, c("Package", "packageFullName")], packageFullName := i.packageFullName, on = "Package"]
    }
    deps <- pfnAll$packageFullName
    imports <- pfnAll$Package

    # concatenate version number without GH
    versionNumb <- Require::extractVersionNumber(pfnAll$packageFullName)
    hasVersionNumb <- !is.na(versionNumb)
    inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
    missingSpace <- !grepl("[[:space:]]", inequality)
    if (any(missingSpace))
      inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])
    hasSC <- grepl("SpaDES.core", imports)
    if (all(!hasSC))
      imports <- c("SpaDES.core", imports)
    imports[hasVersionNumb] <- paste(imports[hasVersionNumb], inequality)
    d$Imports <- imports

    d$Suggests <- c('knitr', 'rmarkdown')

    if (write) {
      if (singleDESCRIPTION)
        dFile <- file.path(projectPath, "DESCRIPTION")
      else
        dFile <- filenameFromFunction(packageFolderName, "DESCRIPTION", fileExt = "")
    } else {
      dFile <- Require::tempfile2()
    }

    cat(paste("Package:", d$Package), file = dFile, sep = "\n")
    cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
    cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

    if (length(d$Imports))
      cat(c("Imports:", paste("   ", d$Imports, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

    cat(c("Suggests:", paste("   ", d$Suggests, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

    needRemotes <- grepl("/", pfnAll$packageFullName)
    if (any(needRemotes)) {
      remotes <- trimVersionNumber(pfnAll$packageFullName[needRemotes])
      d$Remotes <- remotes
      cat(c("Remotes:", paste("   ", d$Remotes, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)
    }
    cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
    cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
    cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
    cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
    cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)

    messageVerbose("DESCRIPTION file written to ", dFile, verbose = verbose)
    dFile
  })

  unlist(dFiles)
}
