utils::globalVariables(c(
  "VersionOnRepos", "i.packageFullName",
  "packageFullName", "hasHEAD"
  ))

## A module's package name: modules may contain underscores, R packages may not.
## Same definition as SpaDES.core:::.moduleNameNoUnderscore, inlined rather than
## reached for -- SpaDES.core is in Suggests, so it may not be present, and this
## is a one-liner.
.moduleNameNoUnderscore <- function(mod) gsub("_", ".", basename(mod))
#filenameFromFunction
#gitAccount init modulesNoVersion
# renv

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
#' @inheritParams Require::Require
#' @return Invisibly, the path(s) of the DESCRIPTION file(s) written.
#' @export
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
#' @param metadataList The parsed source code from a module. Must include `defineModule` metadata.
#' @param date Date to enter into DESCRIPTION file. Defaults to `Sys.Date()`
#' @param ... Currently not used.
#' @export
makeDESCRIPTION <- function(modules, modulePath, projectPath = ".", singleDESCRIPTION = FALSE,
                            package, title, date, description,
                            version, authors, write = TRUE, verbose, metadataList, ...) {

  # Require is in Imports, but these three are not exported by it, so they have
  # to be fetched from its namespace rather than imported. Bound locally so the
  # body below reads as it did when this was written against an attached Require.
  toPkgDTFull         <- getFromNamespace("toPkgDTFull", "Require")
  getVersionOnRepos   <- getFromNamespace("getVersionOnRepos", "Require")
  filenameFromFunction <- getFromNamespace("filenameFromFunction", "Require")

  if (missing(verbose)) verbose <- getOption("Require.verbose", 1L)

  if (missing(metadataList)) {
    mainModuleFile <- file.path(modulePath, unlist(modules), paste0(unlist(modules), ".R"))
    packageFolderName <- dirname(mainModuleFile)
    metadataList <- lapply(mainModuleFile, function(file) parse(file, keep.source = TRUE))
  } else {
    # packageFolderName is where a per-module DESCRIPTION gets written; it used
    # to be defined only in the branch above, so passing metadataList left it
    # undefined.
    packageFolderName <- if (!missing(modulePath)) {
      file.path(modulePath, unlist(modules))
    } else {
      rep(projectPath, length(unlist(modules)))
    }
  }
  defModule <- lapply(metadataList, function(x) grepl(pattern = "^defineModule", x[[1]]))
  whDefModule <- lapply(defModule, function(x) which(x[[1]]))
  mds <- Map(whDefMod = whDefModule, defMod = metadataList, function(whDefMod, defMod) {
    defMod[[whDefMod]][[3]]
  }
  )

  names(mds) <- modules
  mods <-if (singleDESCRIPTION)  "Project" else modules
  # missing() only answers for the frame that owns the formal, so resolve these
  # here rather than inside the per-module closure below.
  hasPackage     <- !missing(package)
  hasTitle       <- !missing(title)
  hasDescription <- !missing(description)
  hasVersion     <- !missing(version)
  hasDate        <- !missing(date)
  hasAuthors     <- !missing(authors)
  # One `d` per DESCRIPTION to be written. This used to be a for-loop assigning
  # a single `d`, so with several modules and singleDESCRIPTION = FALSE every
  # file got the LAST module's metadata.
  dList <- lapply(mods, function(module) {
    md <- mds[[module]]
    d <- list()
    d$Package <- if (hasPackage) package else .moduleNameNoUnderscore(module)
    d$Type <- "Package"

    d$Title <- if (hasTitle) title else md$name
    d$Description <- if (hasDescription) description else md$description
    # d$Description <- md$description
    # Module metadata carries `version = list(<module> = "x.y.z")` as an
    # unevaluated call. Take this module's entry; pasting the call itself
    # vectorises over it and emits two "Version:" lines ("list", then "1.2.3").
    d$Version <- if (hasVersion) {
      version
    } else {
      v <- try(eval(md$version), silent = TRUE)
      if (inherits(v, "try-error") || !length(v)) {
        NA_character_
      } else {
        as.character(if (module %in% names(v)) v[[module]] else v[[1]])
      }
    }
    d$Date <- if (hasDate) date else format(Sys.Date())
    # d$Date <- Sys.Date()
    d$Authors <- if (hasAuthors) authors else md$authors
    # d$Authors <- md$authors
    d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
    d
  })
  names(dList) <- mods

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
    toPkgDTFull(pfnAll)
  })

  if (singleDESCRIPTION) {
    pfnAllList <- list(rbindlist(pfnAllList, fill = TRUE, use.names = TRUE))
  }

  # Each output needs its own metadata and its own destination folder; with
  # singleDESCRIPTION there is exactly one of each.
  folders <- if (singleDESCRIPTION) projectPath else packageFolderName
  dFiles <- Map(pfnAll = pfnAllList, d = dList, folder = folders,
                f = function(pfnAll, d, folder) {

    pfnAll <- trimRedundancies(pfnAll)
    # if ()
    pfnAll[, hasHEAD := grepl("\\(HEAD\\)", packageFullName)]
    whHEAD <- grep("\\(HEAD\\)", pfnAll$packageFullName)
    if (length(whHEAD)) {
      pkgDT <- getVersionOnRepos(pfnAll[whHEAD], repos = getOption("repos"), purge = FALSE)
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
    imports[hasVersionNumb] <- paste(imports[hasVersionNumb], inequality)
    if (all(!hasSC))
      imports <- c("SpaDES.core", imports)
    d$Imports <- imports

    d$Suggests <- c('knitr', 'rmarkdown')

    if (write) {
      if (singleDESCRIPTION)
        dFile <- file.path(projectPath, "DESCRIPTION")
      else
        dFile <- filenameFromFunction(folder, "DESCRIPTION", fileExt = "")
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
