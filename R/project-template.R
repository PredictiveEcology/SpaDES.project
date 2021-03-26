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
#'
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
setGeneric("newProject", function(name, path, type, open) {
  standardGeneric("newProject")
})

#' @export
#' @rdname newProject
#' @importFrom Require checkPath
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "character", open = "logical"),
  definition = function(name, path, type, open) {
    checkPath(path, create = TRUE)
    projDir <- checkPath(file.path(path, name), create = TRUE)

    checkPath(file.path(projDir, "cache"), create = TRUE)
    checkPath(file.path(projDir, "inputs"), create = TRUE)
    checkPath(file.path(projDir, "modules"), create = TRUE)
    checkPath(file.path(projDir, "outputs"), create = TRUE)

    if (type != "basic")
      checkPath(file.path(projDir, "packages"), create = TRUE)

    if (interactive() && Sys.getenv("RSTUDIO") == "1") {
      if (requireNamespace("rstudioapi", quietly = TRUE))
        rstudioapi::initializeProject(path = projDir)
    }

    newProjectCode(name, path, type, open)

    return(projDir)
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "missing", open = "missing"),
  definition = function(name, path, type, open) {
    newProject(name, path, open = interactive(), type = "basic")
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "character", open = "missing"),
  definition = function(name, path, type, open) {
    newProject(name, path, type = type, open = interactive())
})

#' @export
#' @rdname newProject
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", type = "missing", open = "logical"),
  definition = function(name, path, type, open) {
    newProject(name, path, type = "basic", open = open)
})

#' Create new module code file
#'
#' @inheritParams newProject
#'
#' @author Alex Chubaty
#' @export
#' @rdname newProjectCode
#'
setGeneric("newProjectCode", function(name, path, type, open) {
  standardGeneric("newProjectCode")
})

#' @export
#' @importFrom Require checkPath
#' @importFrom whisker whisker.render
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "character", open = "logical"),
  definition = function(name, path, type, open = interactive()) {
    stopifnot(type %in% c("basic", "advanced"))

    nestedPath <- checkPath(file.path(path, name), create = TRUE)
    fnames <- list()

    projectData <- list(projName = name)
    projectTemplates <- list()

    fnames[[1]] <- file.path(nestedPath, "README.md")
    projectTemplates[[1]] <- readLines(file.path(.pkgEnv[["templatePath"]], "README.md.template"))

    if (type == "basic") {
      fnames[[2]] <- file.path(nestedPath, "global.R")

      projectTemplates[[2]] <- readLines(file.path(.pkgEnv[["templatePath"]], "basic-project.R.template"))
    } else if (type == "advanced") {
      fnames <- append(fnames, list(
        file.path(nestedPath, "00-global.R"),
        file.path(nestedPath, "01-init.R"),
        file.path(nestedPath, "02-paths.R"),
        file.path(nestedPath, "03-packages.R"),
        file.path(nestedPath, "04-options.R"),
        file.path(nestedPath, "05-google-ids.R"),
        file.path(nestedPath, "06-studyArea.R"),
        file.path(nestedPath, "07-dataPrep.R"),
        file.path(nestedPath, "08-pre-sim.R"),
        file.path(nestedPath, "09-main-sim.R"),
        file.path(nestedPath, "config.yml"),
        file.path(nestedPath, ".Rprofile")
      ))

      projectTemplates[[2]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-00-global.R.template"))
      projectTemplates[[3]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-01-init.R.template"))
      projectTemplates[[4]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-02-paths.R.template"))
      projectTemplates[[5]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-03-packages.R.template"))
      projectTemplates[[6]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-04-options.R.template"))
      projectTemplates[[7]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-05-google-ids.R.template"))
      projectTemplates[[8]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-06-studyArea.R.template"))
      projectTemplates[[9]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-07-dataPrep.R.template"))
      projectTemplates[[10]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-08-pre-sim.R.template"))
      projectTemplates[[11]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-09-main-sim.R.template"))

      projectTemplates[[12]] <- readLines(file.path(.pkgEnv[["templatePath"]], "config.yml.template"))
      projectTemplates[[13]] <- readLines(file.path(.pkgEnv[["templatePath"]], "Rprofile.template"))
    } else if (type == "LandR-fireSense") {
      fnames <- append(fnames, list(
        file.path(nestedPath, "00-global.R"),
        file.path(nestedPath, "01-init.R"),
        file.path(nestedPath, "02-paths.R"),
        file.path(nestedPath, "03-packages.R"),
        file.path(nestedPath, "04-options.R"),
        file.path(nestedPath, "05-google-ids.R"),
        file.path(nestedPath, "06-studyArea.R"),
        file.path(nestedPath, "07a-dataPrep_2001.R"),
        file.path(nestedPath, "07b-dataPrep_2011.R"),
        file.path(nestedPath, "07c-dataPrep_fS.R"),
        file.path(nestedPath, "08a-ignitionFit.R"),
        file.path(nestedPath, "08b-escapeFit.R"),
        file.path(nestedPath, "08c-spreadFit.R"),
        file.path(nestedPath, "09-main-sim.R"),
        file.path(nestedPath, "config.yml")
      ))

      projectTemplates[[2]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-00-global.R.template"))
      projectTemplates[[3]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-01-init.R.template"))
      projectTemplates[[4]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-02-paths.R.template"))
      projectTemplates[[5]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-03-packages.R.template"))
      projectTemplates[[6]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-04-options.R.template"))
      projectTemplates[[7]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-05-google-ids.R.template"))
      projectTemplates[[8]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-06-studyArea.R.template"))
      projectTemplates[[9]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-07a-dataPrep_2001.R.template"))
      projectTemplates[[10]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-07b-dataPrep_2011.R.template"))
      projectTemplates[[11]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-07c-dataPrep_fS.R.template"))
      projectTemplates[[12]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-08a-ignitionFit.R.template"))
      projectTemplates[[13]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-08b-escapeFit.R.template"))
      projectTemplates[[14]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-08c-spreadFit.R.template"))
      projectTemplates[[15]] <- readLines(file.path(.pkgEnv[["templatePath"]], "advanced-09-main-sim.R.template"))

      projectTemplates[[16]] <- readLines(file.path(.pkgEnv[["templatePath"]], "config.yml.template"))
      projectTemplates[[17]] <- readLines(file.path(.pkgEnv[["templatePath"]], "Rprofile.template"))
    }

    lapply(seq_along(fnames), function(i) {
      writeLines(whisker.render(projectTemplates[[i]], projectData), fnames[[i]])
    })

    if (open) .fileEdit(fnames[[2]]) ## global.R

    return(nestedPath)
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "missing", open = "missing"),
  definition = function(name, path, type, open) {
    newProjectCode(name = name, path = path, type = "basic", open = interactive())
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "missing", open = "logical"),
  definition = function(name, path, type, open) {
    newProjectCode(name = name, path = path, type = "basic", open = open)
})

#' @export
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", type = "character", open = "missing"),
  definition = function(name, path, type, open) {
    newProjectCode(name = name, path = path, type = type, open = interactive())
})
