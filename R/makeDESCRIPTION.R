#' Make DESCRIPTION file(s) from SpaDES module metadata
#'
#' @param modules A character vector of module names
#' @param modulePath
makeDESCRIPTIONproject <- function(modules, modulePath, projectPath = ".", singleDESCRIPTION = TRUE,
                                   package = "Project",
                                   title = "Project", description = "Project",
                                   version = "1.0.0", authors = Sys.info()["user"]) {

  makeDESCRIPTION(modules, modulePath, projectPath, singleDESCRIPTION, package, title,
                  # date,
                  description,
                  version, authors)
}

makeDESCRIPTION <- function(modules, modulePath, projectPath = ".", singleDESCRIPTION = FALSE, package, title, date, description,
                            version, authors, ...) {

  mainModuleFile <- file.path(modulePath, unlist(modules), paste0(unlist(modules), ".R"))
  packageFolderName <- dirname(mainModuleFile)
  aa <- lapply(mainModuleFile, function(file) parse(file, keep.source = TRUE))
  # aa <- parse(mainModuleFile, keep.source = TRUE)
  defModule <- lapply(aa, function(x) grepl(pattern = "^defineModule", x[[1]]))
  whDefModule <- lapply(defModule, function(x) which(x[[1]]))
  mds <- Map(whDefMod = whDefModule, defMod = aa, function(whDefMod, defMod) {
    defMod[[whDefMod]][[3]]
  }
  )
  # md <- aa[[whDefModule]][[3]]

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
    d$Date <- if (!missing(date)) date else md$date
    # d$Date <- Sys.Date()
    d$Authors <- if (!missing(authors)) authors else md$authors
    # d$Authors <- md$authors
    d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
  }

  # importsAll <- character()
  pfnAll <- character()

  for (module in modules) {
    md <- mds[[module]]

    deps <- unlist(eval(md$reqdPkgs))
    imports <- Require::extractPkgName(deps)
    pkgFullName <- deps


    if (singleDESCRIPTION) {
      # importsAll <- c(dAll, imports)
      pfnAll <- c(pfnAll, pkgFullName)
    }
  }

  pfnAll <- Require:::trimRedundancies(Require:::toPkgDTFull(pfnAll))
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

  if (singleDESCRIPTION)
    dFile <- file.path(projectPath, "DESCRIPTION")
  else
    dFile <- filenameFromFunction(packageFolderName, "DESCRIPTION", fileExt = "")

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Imports))
    cat(c("Imports:", paste("   ", d$Imports, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

  Suggests <- c('knitr', 'rmarkdown')
  cat(c("Suggests:", paste("   ", Suggests, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

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
  message("DESCRIPTION file written to ", file.path(projectPath, "DESCRIPTION"))
  dFile
}
