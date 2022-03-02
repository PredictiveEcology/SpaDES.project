#' Create new SpaDES project
#'
#' Initialize a project with subdirectories \file{cache/}, \file{modules/},
#' \file{inputs/}, \file{outputs/}, and \code{setPaths} accordingly.
#' If invoked from Rstudio, will also create a new Rstudio project file.
#'
#' This extends the basic SpaDES project template provided by \pkg{SpaDES.core}.
#'
#' @param name project name (name of project directory)
#' @param path path to directory in which to create the project directory
#' @param type character string giving the project type. Possible values include:
#'             \code{"basic"} (default), and \code{"advanced"}.
#' @param open  Logical. Should the new project file be opened after creation?
#'              Default \code{TRUE} in an interactive session.
#' @param ...  Additional arguments. Currently only the following are implemented:
#'             \describe{
#'               \item{\code{pkgPath}}{
#'                 Path to project's package directory.
#'                 Defaults to \file{<projectName>_packages} in the project's parent directory.
#'               }
#'             }
#' @export
#' @rdname newProject
#'
#' @examples
#' \dontrun{
#' ## use basic project template
#' myProjDir <- newProject("myProject", tempdir())
#'
#' ## use advanced project setup
#' myAdvProjDir <- newProject("myAdvProject", tempdir(), type = "advanced")
#' }
setGeneric("newProject", function(name, path, type, open, ...) {
  standardGeneric("newProject")
})

#' @export
#' @rdname newProject
#' @importFrom Require checkPath
#' @importFrom SpaDES.core setPaths
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "character", open = "logical"),
  definition = function(name, path, type, open, ...) {
    checkPath(path, create = TRUE)
    projDir <- checkPath(file.path(path, name), create = TRUE)

    cachePath <- checkPath(file.path(projDir, "cache"), create = TRUE)
    inputPath <- checkPath(file.path(projDir, "inputs"), create = TRUE)
    modulePath <- checkPath(file.path(projDir, "modules"), create = TRUE)
    outputPath <- checkPath(file.path(projDir, "outputs"), create = TRUE)

    setPaths(cachePath = cachePath,
             inputPath = inputPath,
             modulePath = modulePath,
             outputPath = outputPath)

    if (type != "basic")
      checkPath(file.path(path, paste0(name, "_packages")), create = TRUE)

    if (interactive() && Sys.getenv("RSTUDIO") == "1") {
      if (requireNamespace("rstudioapi", quietly = TRUE))
        rstudioapi::initializeProject(path = projDir)
    }

    newProjectCode(name, path, type, open, ...)

    return(projDir)
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "missing", open = "missing"),
  definition = function(name, path, type, open, ...) {
    newProject(name, path, open = interactive(), type = "basic", ...)
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "character", open = "missing"),
  definition = function(name, path, type, open, ...) {
    newProject(name, path, type = type, open = interactive(), ...)
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "missing", open = "logical"),
  definition = function(name, path, type, open, ...) {
    newProject(name, path, type = "basic", open = open, ...)
})

#' Create new module code file
#'
#' @inheritParams newProject
#'
#' @author Alex Chubaty
#' @export
#' @rdname newProjectCode
#'
setGeneric("newProjectCode", function(name, path, type, open, ...) {
  standardGeneric("newProjectCode")
})

#' @export
#' @importFrom Require checkPath
#' @importFrom whisker whisker.render
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "character", open = "logical"),
  definition = function(name, path, type, open = interactive(), ...) {
    stopifnot(type %in% c("basic", "advanced", "LandR-fireSense"))

    dots <- list(...)
    pkgPath <- if (is.null(dots$pkgPath)) {
      checkPath(file.path(path, paste0(name, "_packages")), create = TRUE)
    } else {
      checkPath(dots$pkgPath, create = TRUE)
    }

    nestedPath <- checkPath(file.path(path, name), create = TRUE)

    ## start with files needed for all project types
    fnames <- list(
      "README.md",
      ".Renviron",
      ".Rprofile"
    )
    tnames <- list(
      "README.md.template",
      "Renviron.template",
      "Rprofile.template"
    )

    ## additional files/templates for each project type
    if (type == "basic") {
      fnames <- append(fnames, "global.R")
      tnames <- append(tnames, "basic-project.R.template")
    } else if (type == "advanced") {
      fnames <- append(fnames, list(
        "config.yml",
        "00-global.R",
        "01-packages.R",
        "02-init.R",
        "03-paths.R",
        "04-options.R",
        "05-google-ids.R",
        "06-studyArea.R",
        "07-dataPrep.R",
        "08-pre-sim.R",
        "09-main-sim.R"
      ))

      tnames <- append(tnames, list(
        "config.yml.template",
        "advanced-00-global.R.template",
        "advanced-01-packages.R.template",
        "advanced-02-init.R.template",
        "advanced-03-paths.R.template",
        "advanced-04-options.R.template",
        "advanced-05-google-ids.R.template",
        "advanced-06-studyArea.R.template",
        "advanced-07-dataPrep.R.template",
        "advanced-08-pre-sim.R.template",
        "advanced-09-main-sim.R.template"
      ))
    } else if (type == "LandR-fireSense") {
      checkPath(file.path(nestedPath, "R"), create = TRUE)
      checkPath(file.path(nestedPath, "scripts"), create = TRUE)

      fnames <- append(fnames, list(
        "config.yml",
        "00a-global_fit.R",
        "00b-global_sim.R",
        "01-packages.R",
        "02-init.R",
        "03-paths.R",
        "04-options.R",
        "05-google-ids.R",
        "05-google-ids.csv",
        "06-studyArea.R",
        "07a-dataPrep_2001.R",
        "07b-dataPrep_2011.R",
        "07c-dataPrep_fS.R",
        "08a-ignitionFit.R",
        "08b-escapeFit.R",
        "08c-spreadFit.R",
        "09-main-sim.R",
        "R/upload_ignitionFit.R",
        "R/upload_spreadFit.R",
        "R/upload_fSDatPrepFit_vegCoeffs.R",
        "R/upload_sims.R",
        "R/upload_sims.R",
        "scripts/submodules.sh"
      ))

      tnames <- append(tnames, list(
        "config.yml.template",
        "LandR-fS-00a-global_fit.R.template",
        "LandR-fS-00b-global_sim.R.template",
        "LandR-fS-01-packages.R.template",
        "LandR-fS-02-init.R.template",
        "LandR-fS-03-paths.R.template",
        "LandR-fS-04-options.R.template",
        "LandR-fS-05-google-ids.R.template",
        "LandR-fS-05-google-ids.csv.template",
        "LandR-fS-06-studyArea.R.template",
        "LandR-fS-07a-dataPrep_2001.R.template",
        "LandR-fS-07b-dataPrep_2011.R.template",
        "LandR-fS-07c-dataPrep_fS.R.template",
        "LandR-fS-08a-ignitionFit.R.template",
        "LandR-fS-08b-escapeFit.R.template",
        "LandR-fS-08c-spreadFit.R.template",
        "LandR-fS-09-main-sim.R.template",
        "LandR-fS-R-upload_ignitionFit.R.template",
        "LandR-fS-R-upload_spreadFit.R.template",
        "LandR-fS-R-upload_fSDatPrepFit_vegCoeffs.R.template",
        "LandR-fS-R-upload_sims.R.template",
        "LandR-fS-R-upload_sims.R.template",
        "LandR-fS-scripts-submodules.sh.template"
      ))
    }

    stopifnot(identical(length(fnames), length(tnames)))

    projectTemplates <- lapply(tnames, function(t) {
      readLines(file.path(.pkgEnv[["templatePath"]], t))
    })

    projectData <- list(projName = name, pkgPath = pkgPath)

    lapply(seq_along(fnames), function(i) {
      writeLines(whisker.render(projectTemplates[[i]], projectData), file.path(nestedPath, fnames[[i]]))
    })

    if (open) {
      lapply(grep("^00.-global.*[.]R$", file.path(nestedPath, fnames)), .fileEdit)
    }

    return(nestedPath)
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "missing", open = "missing"),
  definition = function(name, path, type, open, ...) {
    newProjectCode(name = name, path = path, type = "basic", open = interactive(), ...)
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "missing", open = "logical"),
  definition = function(name, path, type, open, ...) {
    newProjectCode(name = name, path = path, type = "basic", open = open, ...)
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "character", open = "missing"),
  definition = function(name, path, type, open, ...) {
    newProjectCode(name = name, path = path, type = type, open = interactive(), ...)
})
