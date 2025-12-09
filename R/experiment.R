#' Run Parallel Experiments for projects that use `setupProject`
#'
#' Executes a set of experiments in parallel using `furrr::future_pmap()`.
#' Optionally, it can run a pre-setup routine by sourcing and evaluating
#' part of a script (e.g., `global.R`) before starting the experiments.
#' One important reason to do this is to parse the components of the `setupProject`
#' that define the `paths`, so that the `paths` can be used within the
#' experiment, e.g., to define log paths.
#'
#' @param file Character string specifying the path to the R script
#'   (default: `"global.R"`). This script is expected to contain a `setupProject`
#'   call. It is also expected to have very little done before the `setupProject`
#'   call, including no loading of libraries (other than `setupProject` and its
#'   dependencies).
#' @param expt A data.frame/data.table/list of parameters for the experiments.
#'   Each column name must be an object that the `setupProject` is expecting
#'   in the `.GlobalEnv`. Each row in the `expt` should have a single value (e.g.,
#'   `character` or `numeric`) that will be assigned to an object named with
#'   the column name, in the `.GlobalEnv`, so that the `setupProject` will
#'   find it. If there is not a column named `.iter`, then this function will add
#'   it, filling the column with `seq(NROW(expt))`. If there is not a column
#'   named `.runName`, then this function will add it, filling the column with
#'   `apply(expt, 1, paste(..., collapse = "_"))`, i.e. the concatenate of the
#'   values of the `expt` row. If there is not a column named `.logFile`, then
#'   this function will create it, filling the values with
#'   `paste0(.runName, "_", format(Sys.time()))`.
#' @param preRunSetupProject Logical or `character`. Passed to `upTo`
#' in `preRunSetupProject`. Default is `paths`, which means that it will run the `file`
#' up to and including the `paths` argument.
#'
#' @param clearSimEnv Logical. Default `TRUE`. This has the potential to be VERY
#'   large. Returning it from all the threads or cores may be very inefficient. In
#'   general, the user should define the `outputs` that they want so that individual
#'   objects are saved. It will not delete any `dot` object, i.e., objects that start
#'   with a `.`.
#'
#' @param saveSimToDisk Logical or character. Default `FALSE`. If `TRUE`, then this will
#'   save the entire `simList` (before any clearing via `clearSimEnv`, i.e, with
#'   all objects intact) using `saveSimList`. The object will be saved with the name:
#'   `file.path(paths$outputPath, paste0("sim_", .runName))` unless this parameter is a character
#'   string, in which case it will be `file.path(paths$outputPath, paste0(saveSimToDisk, .runName))`.
#'   In these cases, the `paths$` is coming from the `preRunSetupProject` that returns
#'   the paths created with `setupProject`. Alternatively the user can pass a
#'   character vector with the full path of the individual simList objects to save.
#'   If supplying a character vector of names, it must be same length as the `NROW(expt)`,
#'   and it must be explicit about the file extension, either `.rds` or `.qs2` for each
#'   filename.
#'
#' @export
#' @details
#' The function uses `furrr::future_pmap()` to run experiments in parallel.
#' Each experiment:
#' \itemize{
#'   \item Creates a unique log file named using `.ELFind` and the start time.
#'   \item Sources the `global.R` script in a local environment.
#'   \item Captures messages, warnings, and errors, writing them to the log file.
#' }
#'
#' Logging is handled via `withCallingHandlers()`, ensuring that all
#' non-empty messages, warnings, and errors are appended to the log file.
#'
#' @return Invisibly returns the result of `furrr::future_pmap()`.
#' Currently, the inner function returns `NULL` for each iteration.
#'
#' @examples
#' \dontrun{
#' # Example experiment list
#' expt_list <- list(
#'   list(iter = 1, .rep = 1, .ELFind = "A"),
#'   list(iter = 2, .rep = 1, .ELFind = "B")
#' )
#'
#' # Run experiments without pre-run setup
#' experiment3(expt = expt_list)
#'
#' # Run experiments with pre-run setup
#' experiment3(file = "global.R", expt = expt_list)
#' }
#'
experiment3 <- function(expt, file = "global.R", preRunSetupProject = "paths",
                        logFiles = list(expt, "time"),
                        clearSimEnv = TRUE, saveSimToDisk = FALSE) {
  if (isTRUE(preRunSetupProject) || nzchar(preRunSetupProject)) {
    outs <- preRunSetupProject(file = file, upTo = preRunSetupProject)
    # eval(pp[1:whSetupProject], envir = environment())
  }

  if (is.null(expt$.iter)) {
    expt$.iter <- seq_len(NROW(expt))
  }

  if (is.null(expt$.runName)) {
    exptChar <- lapply(expt, function(x) if (is.numeric(x)) reproducible:::paddedFloatToChar(x, padL = max(nchar(x))) else x) |>
      as.data.frame()
    expt$.runName <- apply(exptChar, 1, function(...) paste0(names(exptChar), ..., collapse = "_"))
  }
  if (is.null(expt$.logFile)) {
    logFiles2 <- paste0(expt$.runName)
    if (any(grepl("^time$", unlist(logFiles)))) {
      starttime <- format(Sys.time())
      logFiles2 <- paste0(logFiles2, "_", starttime)
    }
    logFiles2 <- paste0(logFiles2, ".log")
    startsWithDot <- startsWith(logFiles2, ".")
    if (any(startsWithDot)) {
      message("logFiles start with a dot, which will produce hidden files; ",
              " removing initial dot to make visible log files")
      logFiles2[startsWithDot] <- gsub("^\\.", "", logFiles2[startsWithDot])
    }
    logDir <- if (exists("outs", inherits = FALSE)) attr(outs$paths, "extraPaths")$projectPath else normalizePath(".")
    logDir <- file.path(logDir, "logs")
    dir.create(logDir, recursive = TRUE, showWarnings = FALSE)
    expt$.logFile <- file.path(logDir, logFiles2)
  }
  message("First logfiles are:\n",
          paste(head(expt$.logFile), collapse = "\n"))

  mess <- head(expt$.logFile)
  mess <- gsub(" ", "\\\\ ", mess)
  mess <- gsub(":", "\\\\:", mess)
  message("To see log files, run in a separate command prompt e.g., \n",
          paste("tail -f", mess, collapse = "\n"))
  message("or in a separate R session on the same machine:\n",
          "system(paste0(\"tail -f '", expt$.logFile[1], "'\"))")

  frr <- requireNamespace("furrr")
  wthr <- requireNamespace("withr")
  if (!all(c(frr, wthr))) {
    toInstall <- c("furrr", "withr")[!c(frr, wthr)]
    stop("Please install packages", toInstall)
  }
  if (length(saveSimToDisk) > 1)
    if (length(saveSimToDisk) != NROW(expt))
      stop("saveSimToDisk must be either length 1 or length NROW(expt)")


  if (!requireNamespace("furrr")) stop("Please install furrr")
  # exptOrig <- expt
  # expt <- exptOrig[1:4, ]
  rr <- furrr::future_pmap(
    .options = furrr::furrr_options(seed = TRUE,
                                    scheduling = Inf),
    .progress = TRUE,
    .l = expt,
    file = file,
    sstd = saveSimToDisk,
    cse = clearSimEnv,
    .f = function(..., file, sstd, cse) {
      dots <- list(...)
      list2env(dots, environment())

      withCallingHandlers({
        Sys.sleep(dots$.iter)
        withr::local_options(crayon.enabled = TRUE)

        # browser()

        sim <- try(source(file, local = TRUE))
        if (is(sim, "try-error")) {
          warning(sim)
        }
        # sim <- SpaDES.core::simInit(paths = list(outputPath = tempdir()))
        # sim$a <- 1
        # sim$.b <- 2
        filenameEnd <- paste0(dots$.runName, ".rds")
        op <- SpaDES.core::outputPath(sim)
        if (isTRUE(sstd)) {
          sstd <- file.path(op, paste0("sim_", filenameEnd))
        } else if (is.character(sstd)) {
          if (length(sstd) == 1) {
            sstd <- file.path(op, paste0(sstd, filenameEnd))
          } else {
            sstd <- sstd[dots$.iter]
            if (requireNamespace("fs"))
              if (!fs::is_absolute_path(sstd))
                sstd <- file.path(op, sstd)

          }
        }

      }, message = function(mess) {
        if (!identical("\n", mess$message) && nchar(mess$message) > 0)
          cat(mess$message, file = dots$.logFile, append = TRUE, sep = "\n")
      }, warning = function(warn) {
        if (!identical("\n", warn$message) && nchar(warn$message) > 0)
          cat(paste("WARNING: ", warn$message), file = dots$.logFile, append = TRUE, sep = "\n")
      } , error = function(err) {
        if (!identical("\n", err$message) && nchar(err$message) > 0)
          cat(paste("ERROR:", err$message), file = dots$.logFile, append = TRUE, sep = "\n")
      }
      )

      # return(sstd)
      # The final result of the future
      # message("Saving sim to disk at: ", sstd)
      if (isTRUE(!sstd %in% FALSE)) {
        SpaDES.core::saveSimList(sim, filename = sstd)
      }
      if (isTRUE(cse))
        rm(list = ls(sim, all.names = FALSE), envir = SpaDES.core::envir(sim))

      return(invisible(sim))
    }
  )
  return(rr)
}




#' Partially or Fully Run `setupProject`
#'
#' \code{preRunSetupProject} parses an R script (default: \code{"global.R"}) and
#' evaluates its contents up to the \code{setupProject()} call, either fully or
#' partially based on the \code{upTo} argument. This is useful for initializing
#' only certain parts of a project without executing the entire setup.
#'
#' @param file Character string. Path to the R script containing the setup code.
#'   Defaults to \code{"global.R"}.
#' @param upTo Character or logical. If \code{TRUE}, evaluates all code up to and
#'   including the first \code{setupProject()} call within `file`.
#'   If a character string, only evaluates the code up to the `setupProject` plus
#'   the arguments up to the `upTo` named argument. Defaults to \code{"paths"} so
#'   that `paths` will be evaluated and availble to use.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Parses the specified file using \code{parse()}.
#'   \item Identifies the line where \code{setupProject()} is called.
#'   \item Evaluates all code before the \code{setupProject()} call.
#'   \item Depending on \code{upTo}, evaluates either the full call or a subset
#'         of its arguments.
#' }
#'
#' This allows selective initialization of project components for debugging or
#' partial setup in large projects.
#'
#' @return The evaluated result of the executed portion of \code{setupProject()}.
#'   i.e., a `list` returned by \code{setupProject()}.
#'
#' @examples
#' \dontrun{
#' # Run file up to and including the setupProject, but only to the 'paths' argument
#' result <- preRunSetupProject(file = "global.R", upTo = "paths")
#'
#' # Run file up to and including full setupProject()
#' result <- preRunSetupProject(file = "global.R", upTo = TRUE)
#' }
#'
#' @seealso \code{\link{setupProject}}
#'
#' @export
preRunSetupProject <- function(file = "global.R", upTo = "paths") {
  pp <- parse(file)
  whSetupProject <- grep("setupProject", pp)
  # eval(pp[1:whSetupProject], envir = environment())

  eval(pp[1:c(whSetupProject - 1)], envir = environment())
  if (isTRUE(upTo)) {
    outs <-   eval(pp[whSetupProject], envir = environment())
  } else {
    upToNum <- grep(upTo, names(pp[[whSetupProject]][[3]]))
    outs <- eval(pp[[whSetupProject]][[3]][1:upToNum], envir = environment())
  }
  outs
}
