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
  # This translation used to be implemented here as well as in SpaDES.core, and
  # the two had drifted: each had fixes and features the other lacked, and this
  # copy even inlined its own .moduleNameNoUnderscore() to avoid reaching into
  # SpaDES.core. SpaDES.core owns module metadata -- defineModule(), packages(),
  # moduleMetadata() -- so the translation lives there now and this delegates.
  # SpaDES.core is in Suggests, hence requireNamespace() rather than an import.
  # Check for the function, not just the package: SpaDES.core is in Suggests, so
  # its version floor cannot be enforced at install time, and an older copy is
  # both installed and importable. Testing the namespace alone let that through
  # and produced "'DESCRIPTIONfromModule' is not an exported object" instead of
  # something the reader can act on.
  if (!requireNamespace("SpaDES.core", quietly = TRUE) ||
      !exists("DESCRIPTIONfromModule", envir = asNamespace("SpaDES.core"), inherits = FALSE))
    stop("makeDESCRIPTION() needs SpaDES.core (>= 3.2.1.9002), which provides ",
         "DESCRIPTIONfromModule(). Install or update it:\n",
         "  Require::Install('PredictiveEcology/SpaDES.core@development')", call. = FALSE)

  if (missing(verbose)) verbose <- getOption("Require.verbose", 1L)

  # Forward only what the caller actually supplied: DESCRIPTIONfromModule() uses
  # missing() on each of these to decide whether to fall back to the metadata,
  # so passing a placeholder would override the metadata with it.
  args <- list(modules = modules, projectPath = projectPath,
               singleDESCRIPTION = singleDESCRIPTION, write = write, verbose = verbose)
  if (!missing(modulePath))   args$modulePath   <- modulePath
  if (!missing(package))      args$package      <- package
  if (!missing(title))        args$title        <- title
  if (!missing(date))         args$date         <- date
  if (!missing(description))  args$description  <- description
  if (!missing(version))      args$version      <- version
  if (!missing(authors))      args$authors      <- authors
  if (!missing(metadataList)) args$metadataList <- metadataList

  do.call(SpaDES.core::DESCRIPTIONfromModule, args)
}
