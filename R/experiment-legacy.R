## experiment(), factorialDesign() and simInitAndExperiment(), ported from the
## now-unmaintained SpaDES.experiment package. experiment() is now a light
## wrapper: it builds the factorial set of simLists and delegates execution to
## experiment2() (future backend). The bespoke parallel-cluster machinery
## (`.setupCl()` etc.) that the old experiment() carried has been dropped.
##
## SpaDES.core / reproducible are in Suggests; they are required at call time and
## accessed via locally-bound functions so the ported bodies read unchanged.

if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expLevel"))
}

#' Build a factorial experiment design
#'
#' Extracts the "all meaningful combinations" factorial-design logic that used to
#' live inside `experiment()`. Given a `simList` plus lists of alternative
#' `params` / `modules` / `inputs` / `objects`, it returns one row per run.
#' Values are stored as *indices* into the supplied alternatives (because an
#' alternative may itself be a vector and so cannot live in a single data.frame
#' cell); column names are `module.parameter`, plus a `modules` index, an
#' `expLevel`, and (when relevant) `input`, `object` and `replicate` columns.
#'
#' This is the engine behind [experiment()]. It is exported so the same design
#' can also seed the file-queue [experiment_family] (`experimentFuture()` etc.):
#' map each row's indices back to values to build their `df`.
#'
#' @inheritParams experiment
#'
#' @return A `data.frame`, one row per run.
#'
#' @export
#' @importFrom data.table rbindlist
#' @importFrom stats na.omit
#' @seealso [experiment()], [experiment_family]
factorialDesign <- function(sim, params, modules, objects = list(), inputs,
                            replicates = 1) {
  if (!requireNamespace("SpaDES.core", quietly = TRUE))
    stop("Package 'SpaDES.core' is required for factorialDesign(). ",
         "Install it with: install.packages('SpaDES.core')", call. = FALSE)

  if (missing(params)) params <- list()
  if (missing(modules)) modules <- list(unlist(SpaDES.core::modules(sim)))
  if (missing(inputs)) inputs <- list()
  if (missing(objects)) {
    objects <- list()
  } else if (length(objects) == 1) {
    objects <- unlist(objects, recursive = FALSE)
  }

  factorialExpList <- lapply(seq_along(modules), function(x) {
    paramsTmp <- pmatch(modules[[x]], names(params)) |> na.omit()
    factorsTmp <- if (NROW(paramsTmp) > 0) {
      lapply(params[paramsTmp], function(z) {
        lapply(z, function(y) seq_along(y))
      }) |> unlist(recursive = FALSE)
    } else {
      params
    }

    if (length(inputs) > 0) {
      inputsList <- list(input = seq_len(NROW(inputs)))
      factorsTmp <- append(factorsTmp, inputsList)
    }
    if (length(objects) > 0) {
      objectsList <- list(object = seq_along(objects))
      factorsTmp <- append(factorsTmp, objectsList)
    }

    factorialExpInner <- expand.grid(factorsTmp, stringsAsFactors = FALSE)

    if (NROW(factorialExpInner) > 0) {
      if (any(!(names(factorialExpInner) %in% c("object", "input")))) {
        factorialExpInner[["modules"]] <- x
      }
    } else {
      factorialExpInner <- data.frame(modules = x, stringsAsFactors = FALSE)
    }
    factorialExpInner
  })
  factorialExp <- rbindlist(factorialExpList, fill = TRUE) |>
    data.frame(stringsAsFactors = FALSE)
  numExpLevels <- NROW(factorialExp)
  factorialExp$expLevel <- seq_len(numExpLevels)

  # Add replicates to experiment
  if (length(replicates) == 1 && replicates > 1) {
    replicates <- seq_len(replicates)
    factorialExp <- do.call(rbind, replicate(length(replicates), factorialExp,
                                             simplify = FALSE))
    factorialExp$replicate <- rep(replicates, each = numExpLevels)
  } else if (length(replicates) > 1) {
    factorialExp <- do.call(rbind, replicate(length(replicates), factorialExp,
                                             simplify = FALSE))
    factorialExp$replicate <- rep(replicates, each = numExpLevels)
  }
  factorialExp
}

################################################################################
#' Run an experiment using [SpaDES.core::spades()]
#'
#' A wrapper around [experiment2()] that builds a fully-factorial set of
#' `simList`s from a single base `simList` plus alternative `params` / `modules`
#' / `inputs` / `objects`, then runs them via [experiment2()]'s `future`
#' backend. The factorial design is built with [factorialDesign()].
#'
#' This function (and the `simLists` class it produces) was moved here from the
#' now-unmaintained `SpaDES.experiment` package. Two behavioural notes versus the
#' historical version: parallelism is now controlled by `future::plan()` rather
#' than a `cl` cluster object (the `cl` argument is accepted but ignored, with a
#' message), and the return value is a `simLists` object (as from
#' [experiment2()]) rather than a plain list. The experimental design table is
#' still saved to `experimentFile` and is attached to the result (see Value).
#'
#' @param sim A `simList`, acting as the basis for the experiment.
#' @param inputs Like for [SpaDES.core::simInit()], but a list of `inputs` data.frames.
#' @param objects Like for [SpaDES.core::simInit()], but a list of named lists of named objects.
#' @param params Like for [SpaDES.core::simInit()], but for each parameter, provide a list of
#'               alternative values.
#' @param modules Like for [SpaDES.core::simInit()], but a list of `module` names (as strings).
#' @param replicates The number of replicates to run of the same `simList`.
#' @param substrLength Numeric. While making `outputPath` for each spades call, this
#'                     is the number of characters kept from each factor level.
#' @param dirPrefix String vector. This will be concatenated as a prefix on the
#'                  directory names.
#' @param saveExperiment Logical. Should the resulting experimental design be saved
#'   to a file. Default TRUE.
#' @param experimentFile String. Filename if `saveExperiment` is TRUE; saved to
#'   `outputPath(sim)` in `.RData` format.
#' @param clearSimEnv Logical. If TRUE, then the `envir(sim)` of each simList in the
#'   return is emptied, to reduce RAM load. Default FALSE.
#' @param notOlderThan Currently unused (kept for back-compatibility).
#' @param cl Deprecated and ignored; control parallelism with `future::plan()`.
#' @param ... Passed to [experiment2()] and onward to [SpaDES.core::spades()]
#'   (e.g. `debug`, `.plotInitialTime`, `cache`, and `events` -- see
#'   `Controlling events` in [experiment2()]).
#'
#' @return Invisibly, a `simLists` object. The experimental design list
#'   (`expDesign` + `expVals`) is attached as an attribute named `"experiment"`
#'   on the object's data environment, i.e. `attr(out@.xData, "experiment")`,
#'   and is also written to `experimentFile`.
#'
#' @author Eliot McIntire
#' @export
#' @include experiment2.R simLists-class.R
#' @importFrom data.table rbindlist
#' @rdname experiment
#' @seealso [experiment2()], [factorialDesign()], [as.data.table.simLists()],
#'   [experiment_family]
## Plain function (not S4) because it dispatched only on `simList`, a class from
## the Suggests-only SpaDES.core; see the note in experiment2.R.
experiment <- function(sim, replicates = 1, params, modules, objects = list(), inputs,
                       dirPrefix = "simNum", substrLength = 3, saveExperiment = TRUE,
                       experimentFile = "experiment.RData", clearSimEnv = FALSE,
                       notOlderThan, cl, ...) {
    .experiment2RequireDeps()
    if (!is(sim, "simList"))
      stop("`sim` must be a `simList` object", call. = FALSE)
    if (!missing(cl) && !is.null(cl))
      message("`cl` is deprecated in experiment(); parallelism is now controlled ",
              "via future::plan(). Ignoring `cl`.")

    if (missing(params)) params <- list()
    if (missing(modules)) modules <- list(unlist(SpaDES.core::modules(sim)))
    if (missing(inputs)) inputs <- list()
    if (missing(objects)) {
      objects <- list()
    } else if (length(objects) == 1) {
      objects <- unlist(objects, recursive = FALSE)
    }

    factorialExp <- factorialDesign(sim = sim, params = params, modules = modules,
                                    objects = objects, inputs = inputs,
                                    replicates = replicates)
    numExpLevels <- max(factorialExp$expLevel)
    replicatesSeq <- if (length(replicates) == 1 && replicates > 1) {
      seq_len(replicates)
    } else {
      replicates
    }

    numToDo <- seq_len(NROW(factorialExp))
    built <- lapply(numToDo, .buildExperimentSim, sim = sim, factorialExp = factorialExp,
                    modules = modules, paramsAlt = params, dirPrefix = dirPrefix,
                    numExpLevels = numExpLevels, substrLength = substrLength,
                    replicates = replicatesSeq, inputs = inputs, objects = objects)
    sims <- lapply(built, `[[`, 1)
    expDFs <- lapply(built, `[[`, 2)
    experimentDF <- rbindlist(expDFs, fill = TRUE, use.names = TRUE) |>
      data.frame(stringsAsFactors = FALSE)

    keepCols <- names(experimentDF) %in% c(names(factorialExp),
                                           "param"[length(params) > 1],
                                           "module"[length(params) > 1],
                                           "modules"[length(modules) > 1],
                                           "val"[length(params) > 1])
    experimentDF <- experimentDF[, keepCols]

    experiment <- list(expDesign = factorialExp, expVals = experimentDF)

    # Factorial Levels are determined at this point. Save file.
    if (saveExperiment) {
      save(experiment, file = file.path(SpaDES.core::outputPath(sim), experimentFile))
    }

    # Each built simList already has its outputPath set to the factor-derived
    # directory, so tell experiment2() not to nest the path again.
    out <- do.call(experiment2,
                   c(unname(sims),
                     list(replicates = 1L,
                          createUniquePaths = character(0),
                          clearSimEnv = clearSimEnv, ...)))

    # attach the design table for downstream use (environments hold attributes)
    attr(out@.xData, "experiment") <- experiment
    return(invisible(out))
}

#' Build a single experiment simList (one design row), without running it
#'
#' Internal: applies one row of the factorial design (params / modules / inputs /
#' objects overrides) to a copy of `sim`, sets its `outputPath` to the
#' factor-derived directory, and returns the prepared `simList` plus the
#' corresponding design-value row. Lifted from the old `experiment()` worker
#' (`FunDef`), minus the `spades()` call (now done by [experiment2()]).
#'
#' @keywords internal
#' @noRd
#' @importFrom data.table rbindlist
.buildExperimentSim <- function(ind, sim, factorialExp, modules, paramsAlt,
                                dirPrefix, numExpLevels, substrLength,
                                replicates, inputs, objects) { # nolint
  # local bindings to SpaDES.core (Suggests) functions so the ported body reads
  # unchanged. The list of alternative parameter values is `paramsAlt` (it used
  # to be `params`, which collided with the SpaDES.core `params()` accessor once
  # SpaDES.core moved from Imports to Suggests).
  Copy <- reproducible::Copy
  paddedFloatToChar <- SpaDES.core::paddedFloatToChar
  simInit <- SpaDES.core::simInit
  times <- SpaDES.core::times
  paths <- SpaDES.core::paths
  outputs <- SpaDES.core::outputs
  params <- SpaDES.core::params
  `params<-` <- get("params<-", envir = asNamespace("SpaDES.core"))
  `paths<-` <- get("paths<-", envir = asNamespace("SpaDES.core"))
  `outputs<-` <- get("outputs<-", envir = asNamespace("SpaDES.core"))
  `inputs<-` <- get("inputs<-", envir = asNamespace("SpaDES.core"))

  dtOrig <- data.table::setDTthreads(2)
  on.exit(data.table::setDTthreads(dtOrig), add = TRUE)
  mod <- strsplit(names(factorialExp), split = "\\.") |>
    sapply(function(x) x[1])
  param <- strsplit(names(factorialExp), split = "\\.") |>
    sapply(function(x) x[2])
  param[is.na(param)] <- ""

  paramValues <- factorialExp[ind, ]

  whNotExpLevel <- which(colnames(paramValues) != "expLevel")
  if (length(whNotExpLevel) < length(paramValues)) {
    mod <- mod[whNotExpLevel]
    param <- param[whNotExpLevel]
    paramValues <- paramValues[whNotExpLevel]
  }

  whNotRepl <- which(colnames(paramValues) != "replicate")
  if (length(whNotRepl) < length(paramValues)) {
    repl <- paramValues$replicate
    mod <- mod[whNotRepl]
    param <- param[whNotRepl]
    paramValues <- paramValues[whNotRepl]
  }

  notNA <- which(!is.na(paramValues))

  if (length(notNA) < length(mod)) {
    mod <- mod[notNA]
    param <- param[notNA]
    paramValues <- paramValues[notNA]
  }

  sim_ <- Copy(sim) # nolint
  experimentDF <- data.frame(module = character(0),
                             param = character(0),
                             val = I(list()),
                             modules = character(0),
                             input = data.frame(),
                             object = character(0),
                             expLevel = numeric(0),
                             stringsAsFactors = FALSE)

  for (x in seq_along(mod)) {
    if (any(mod != "modules")) {
      y <- factorialExp[ind, names(paramValues)[x]]

      if (!is.na(y) & (mod[x] != "modules")) {
        val <- paramsAlt[[mod[x]]][[param[[x]]]][[y]]
        params(sim_)[[mod[x]]][[param[[x]]]] <- val #factorialExp[ind,x]
        experimentDF <- rbindlist(
          l = list(
            experimentDF,
            data.frame(
              module = if (!(mod[x] %in% c("input", "object"))) mod[x] else NA,
              param = if (!(mod[x] %in% c("input", "object"))) param[x] else NA,
              val = if (!(mod[x] %in% c("input", "object"))) I(list(val)) else list(NA),
              modules = paste0(unlist(modules[factorialExp[ind, "modules"]]), collapse = ","),
              input = if (mod[x] %in% c("input")) inputs[[factorialExp[ind, "input"]]] else NA,
              object = if (mod[x] %in% c("object")) names(objects)[[factorialExp[ind, "object"]]] else NA, # nolint
              expLevel = factorialExp[ind, "expLevel"],
              stringsAsFactors = FALSE
            )),
          use.names = TRUE,
          fill = TRUE)
      }
    } else {
      experimentDF <- rbindlist(
        l = list(
          experimentDF,
          data.frame(modules = paste0(unlist(modules[factorialExp[ind, "modules"]]),
                                      collapse = ","),
                     expLevel = factorialExp[ind, "expLevel"],
                     stringsAsFactors = FALSE
          )),
        use.names = TRUE,
        fill = TRUE)
    }

    if (!any(unlist(lapply(modules, is.null)))) {
      if ("modules" %in% names(factorialExp)) {
        if (!identical(sort(unlist(modules[factorialExp[ind, "modules"]])),
                       sort(unlist(SpaDES.core::modules(sim))))) {
          # test if modules are different from sim; if yes, rerun simInit
          sim_ <- simInit(params = params(sim_), # nolint
                          modules = as.list(unlist(modules[factorialExp[ind, "modules"]])),
                          times = append(lapply(times(sim_)[2:3], as.numeric), times(sim_)[4]),
                          paths = paths(sim_),
                          outputs = outputs(sim_))
        }
      }
    } else {
      sim_ <- sim
    }
  }

  # Deal with directory structures
  if (any(dirPrefix == "simNum")) {
    exptNum <- paddedFloatToChar(factorialExp$expLevel[ind],
                                 ceiling(log10(numExpLevels + 1)))
  }
  dirPrefixTmp <- paste0(dirPrefix, collapse = "")

  if ((numExpLevels > 1) & (substrLength > 0)) {
    dirName <- paste(collapse = "-", substr(mod, 1, substrLength),
                     substr(param, 1, substrLength),
                     paramValues, sep = "_")
    dirName <- gsub(dirName, pattern = "__", replacement = "_")
    if (any(dirPrefix == "simNum")) {
      dirPrefix <- gsub(dirPrefixTmp, pattern = "simNum", replacement = exptNum)
    }
    if (any(dirPrefix != "")) {
      dirName <- paste(paste(dirPrefix, collapse = ""), dirName, sep = "_")
    }
  } else if (substrLength == 0) {
    if (any(dirPrefix != "")) {
      simplePrefix <- if (any(dirPrefix == "simNum")) exptNum else ""
      dirName <- gsub(dirPrefixTmp, pattern = "simNum", replacement = simplePrefix)
    }
  } else {
    if (any(dirPrefix != "")) {
      dirName <- gsub(dirPrefixTmp, pattern = "simNum", replacement = "")
    }
  }

  if (exists("repl", inherits = FALSE)) {
    nn <- paste0("rep", paddedFloatToChar(repl, ceiling(log10(length(replicates) + 1))))
    dirName <- if (!is.null(dirName)) {
      file.path(dirName, nn)
    } else {
      file.path(nn)
    }
  }
  newOutputPath <- file.path(paths(sim_)$outputPath, dirName) |>
    gsub(pattern = "/$", replacement = "") |>  # nolint
    gsub(pattern = "//", replacement = "/")
  if (!dir.exists(newOutputPath)) dir.create(newOutputPath, recursive = TRUE)
  paths(sim_)$outputPath <- newOutputPath
  if (NROW(outputs(sim_))) {
    outputs(sim_)$file <- file.path(newOutputPath, basename(outputs(sim_)$file))
  }
  # Actually put inputs into simList
  if (length(inputs) > 0) {
    inputs(sim_) <- inputs[[factorialExp[ind, "input"]]]
  }
  # Actually put objects into simList
  if (length(objects) > 0) {
    replaceObjName <- strsplit(names(objects)[[factorialExp[ind, "object"]]],
                               split = "\\.")[[1]][1]
    sim_[[replaceObjName]] <- objects[[factorialExp[ind, "object"]]]
  }

  return(list(sim_, experimentDF))
}

################################################################################
#' Run `simInit` and `experiment` in one step
#'
#' @export
#' @aliases simInitAndExperiment
#' @rdname simInitAnd
#' @inheritParams experiment
#' @importFrom utils getFromNamespace
#' @details
#' `simInitAndExperiment` cannot pass modules or params to `experiment` because
#' these are also in `simInit`. If the `experiment` is being used
#' to vary these arguments, it must be done separately (i.e., `simInit` then
#' `experiment`).
#'
#' Moved here from the now-unmaintained `SpaDES.experiment` package.
simInitAndExperiment <- function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
                                 notOlderThan, replicates,
                                 dirPrefix, substrLength, saveExperiment,
                                 experimentFile, clearSimEnv, cl, ...)  {
  if (!requireNamespace("SpaDES.core", quietly = TRUE))
    stop("Package 'SpaDES.core' is required for simInitAndExperiment(). ",
         "Install it with: install.packages('SpaDES.core')", call. = FALSE)
  simInit <- SpaDES.core::simInit
  spades <- SpaDES.core::spades

  list2env(list(...), envir = environment())
  lsAllNames <- ls(all.names = TRUE)
  lsAllNames <- lsAllNames[lsAllNames != "..."]

  objsAll <- mget(lsAllNames, envir = environment())

  objsSimInit <- objsAll[formalArgs(simInit)]

  namesMatchCall <- names(match.call())
  objsSimInit <- getFromNamespace(".fillInSimInit", ns = "SpaDES.core")(objsSimInit, namesMatchCall)

  sim <- simInit(times = objsSimInit$times, params = objsSimInit$params,
                 modules = objsSimInit$modules, objects = objsSimInit$objects,
                 paths = objsSimInit$paths, inputs = objsSimInit$inputs,
                 outputs = objsSimInit$outputs, loadOrder = objsSimInit$loadOrder,
                 notOlderThan = objsSimInit$notOlderThan)

  experimentFormals <- formalArgs(experiment)[formalArgs(experiment) %in% names(objsAll)]
  objsExperiment <- append(list(sim = sim), objsAll[experimentFormals])
  spadesFormals <- formalArgs(spades)[formalArgs(spades) %in% names(objsAll)]
  objsSpades <- append(list(sim = quote(sim)), objsAll[spadesFormals]) # quote so entire simList is not serialized

  # Because there are some arguments in BOTH simInit and Experiment, can't pass them
  #  through, because they have different meaning
  objsExperiment <- objsExperiment[!names(objsExperiment) %in% names(objsSimInit)]
  onlyInSpades <- setdiff(names(objsSpades), names(objsExperiment))
  if (length(onlyInSpades))
    objsExperiment[onlyInSpades] <- objsSpades[onlyInSpades]
  sims <- do.call("experiment", objsExperiment)

  return(sims)
}
