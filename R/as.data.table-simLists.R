## as.data.table method for simLists, ported from SpaDES.experiment.
## SpaDES.core / purrr are in Suggests, so they are required at runtime
## and accessed via `::` / getFromNamespace rather than @importFrom.

utils::globalVariables(c("..onlyInteger", "objectName", "saveTime", "file"))

#' Coerce elements of a `simLists` object to a `data.table`
#'
#' This is particularly useful to build plots using the \pkg{tidyverse}, e.g., \pkg{ggplot2}.
#' Ported here from the now-unmaintained `SpaDES.experiment` package.
#'
#' @inheritParams data.table::as.data.table
#'
#' @param vals A (named) list of object names to extract from each
#'   `simList`, or a named list of quoted expressions to calculate for each `simList`,
#'   or a mix of character and quoted expressions.
#'
#' @param objectsFromSim Character vector of objects to extract from the simLists. If
#'   omitted, it will extract all objects from each simList in order to calculate the
#'   `vals`. This may have a computational cost. If `NA`, then no objects will be
#'   accessed from the `simList`. Objects identified here will only be as they are in
#'   the `simList`, i.e., at `end(sim)`.
#'
#' @param objectsFromOutputs List of (named) character vectors of objects to load from the
#'   `outputs(sim)` prior to evaluating `vals`. If there already is an object
#'   with that same name in the `simList`, then it will be overwritten with
#'   the object loaded from `outputs(sim)`. If there are many objects with the
#'   same name, specifically from several `saveTime` values in the `outputs(sim)`,
#'   these will all be loaded, one at a time, `vals` evaluated one at a time, and
#'   each of the values will be returned from each `saveTime`.
#'   A column, `saveTime`, will be part of the returned `data.table`.
#'   For cases where more than one object is required at a given
#'   `saveTime`, all should be identified here, without time specified.
#'   This function will take all identified objects from the same time period.
#'
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' See examples.
#'
#' @return
#' This returns a `data.table` class object.
#'
#' @exportS3Method data.table::as.data.table
#' @importFrom data.table := as.data.table is.data.table set setDT setkeyv setnames
#' @importFrom data.table data.table melt rbindlist
#' @importFrom stats na.omit
#' @importFrom tools file_ext
#' @importFrom utils getFromNamespace
#' @include simLists-class.R
as.data.table.simLists <- function(x, vals,
                                   objectsFromSim = NULL,
                                   objectsFromOutputs = NULL,  ...) {
  if (!requireNamespace("SpaDES.core", quietly = TRUE))
    stop("Package 'SpaDES.core' is required for as.data.table.simLists. ",
         "Install it with: install.packages('SpaDES.core')", call. = FALSE)
  if (!requireNamespace("purrr", quietly = TRUE))
    stop("Package 'purrr' is required for as.data.table.simLists. ",
         "Install it with: install.packages('purrr')", call. = FALSE)

  # local bindings to SpaDES.core (Suggests) functions
  envir <- SpaDES.core::envir
  outputs <- SpaDES.core::outputs
  end <- SpaDES.core::end
  .fileExtensions <- getFromNamespace(".fileExtensions", "SpaDES.core")

  objs <- ls(x)
  names(objs) <- objs
  simLists <- gsub("_.*", "", objs)
  if (!is.list(vals)) {
    vals <- if (is.character(vals)) {
      as.list(vals)
    } else {
      list(vals)
    }
  }
  vals <- updateNames(vals)
  if (!is.null(objectsFromOutputs)) {
    if (!is(objectsFromOutputs, "list")) {
      stop("objectsFromOutputs must be a list of same length as vals")
    }
    if (length(objectsFromOutputs) < length(vals)) { # recycling
      message("objectsFromOutputs is shorter than vals. Recycling values to create same length")
      if (!is.null(names(objectsFromOutputs))) {
        namesMatches <- match(names(objectsFromOutputs), names(vals))
        namesMisMatches <- which(!seq_along(vals) %in% namesMatches)
        if (length(namesMisMatches))
          stop("objectsFromOutputs is shorter than vals, and the name order also does not match")
      }
      objectsFromOutputs <- rep(objectsFromOutputs, length.out = length(vals))
      names(objectsFromOutputs) <- names(vals)
    }
    if (!all(names(objectsFromOutputs) == names(vals))) {
      stop("objectsFromOutputs must be a named list with same length and names as vals")
    }
  }

  # Evaluate the expression
  reps <- gsub(".*_", "", objs)

  ll <- lapply(objs, vals = vals, ofos = objectsFromOutputs,
               function(sName, vals, ofos) {
                 valsNoTime <- vals # default is all
                 out <- NULL
                 n <- new.env(parent = .GlobalEnv) # need this to be .GlobalEnv so eval will
                                                   # find functions in search() (including base)
                 if (is.null(objectsFromSim)) {
                   # get ALL objects from simList -- could be slow --
                   objectsFromSim <- ls(x[[sName]])
                 }
                 if (!all(is.na(objectsFromSim))) {
                   list2env(mget(na.omit(objectsFromSim),
                                 envir = envir(x[[sName]])), envir = n)
                 }

                 if (length(ofos)) {
                   needTimes <- unlist(lapply(ofos, function(o) !all(is.na(o))))
                   ofosNoTime <- ofos[!needTimes]
                   valsNoTime <- vals[!needTimes]
                   vals <- vals[needTimes]
                   ofos <- ofos[needTimes]
                   ofos <- unique(unlist(ofos))
                   out <- list()
                   if (!is.null(ofos)) {
                     outpts <- if (is(ofos, "list")) {
                       setDT(outputs(x[[sName]]))[objectName %in% unlist(ofos)]
                     } else {
                       setDT(outputs(x[[sName]]))[objectName %in% ofos]
                     }

                     innerTimes <- outpts$saveTime
                   } else {
                     innerTimes <- SpaDES.core::end(x[[sName]])
                   }
                   innerTimes <- unique(innerTimes)
                   names(innerTimes) <- as.character(innerTimes)
                   Times <- innerTimes

                   out <- lapply(innerTimes, function(t) {
                     if (!is.null(ofos)) {
                       theLine <- outpts[objectName %in% ofos & saveTime == t, ]
                       theFile <- theLine[, file]
                       ext <- file_ext(theFile)
                       dt1 <- data.table(exts = ext)
                       fun <- setDT(.fileExtensions())[dt1, on = "exts"]$fun
                       tmp <- lapply(seq_along(fun), function(i) {
                         tmpObj <- get(fun[i])(theFile[i])
                         assign(theLine$objectName[i], tmpObj, envir = n)
                       })
                     }

                     # get only some of the objects from x if don't need all
                     out2 <- lapply(vals, function(val) {
                       if (is.call(val)) {
                         eval(val, envir = n)
                       } else {
                         eval(parse(text = val), envir = n)
                       }
                     })
                   })
                   if (length(Times) == 1) {
                     out <- out[[1]]
                   } else {
                     ll2 <- purrr::transpose(out)
                     labels <- seq_along(ll2)
                     names(labels) <- names(ll2)
                     ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
                     dt <- as.data.table(ll3)
                     out <- data.table(saveTime = Times, dt, stringsAsFactors = FALSE)
                   }

                   # deal with mismatching classes
                   cla <- lapply(out[, !"saveTime"], is)
                   claSame <- all(sapply(cla, identical, cla[[1]]))
                   if (isFALSE(claSame)) {
                     onlyNumerics <- sapply(out[, !"saveTime"], is.numeric)
                     if (!all(onlyNumerics)) {
                       stop("vals produce different class objects;",
                            " they must all produce same class")
                     } else {
                       message("vals produce columns of classes integer and numeric;",
                               " converting all to numerics")
                       onlyInteger <- sapply(out[, !"saveTime"], is.integer)
                       namesInteger <- names(out[, !"saveTime"][, ..onlyInteger])
                     }
                     tmp <- lapply(namesInteger, function(col) {
                       set(out, NULL, col, as.numeric(out[[col]]))
                     })
                   }
                   out <- data.table::melt(out, id.vars = "saveTime", variable.name = "vals",
                                           variable.factor = FALSE)
                 }

                 if (length(valsNoTime)) {
                   out2 <- lapply(valsNoTime, function(val) {
                     out3 <- if (is.call(val)) {
                       eval(val, envir = n)
                     } else {
                       eval(parse(text = val), envir = n)
                     }
                     dt <- as.data.table(out3)
                     out3 <- data.table(saveTime = end(x[[sName]]), dt, stringsAsFactors = FALSE)
                   })
                   out2 <- rbindlist(out2, idcol = "vals")
                   setnames(out2, old = "out3", new = "value")
                   out <- rbindlist(list(out, out2), use.names = TRUE)
                 }
                 out
  })

  if (!all(unlist(lapply(ll, is.data.table)))) {
    ll2 <- purrr::transpose(ll)
    labels <- seq_along(ll2)
    names(labels) <- names(ll2)
    ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
    dt <- data.table(simName = rownames(ll3[[1]]), as.data.table(ll3),
                     stringsAsFactors = FALSE)
  } else {
    dt <- rbindlist(ll, use.names = TRUE, idcol = "simName", fill = TRUE)
  }
  dt[, `:=`(simList = gsub("_.*", "", simName), reps = gsub(".*_", "", simName))]
  varNameOnly <- gsub(".V[[:digit:]]+", "", names(dt))
  changed <- which(varNameOnly != names(dt))
  counts <- table(varNameOnly)
  whichSingleton <- which(counts == 1)

  if (any(changed %in% whichSingleton)) {
    out <- lapply(names(whichSingleton), dt = dt, function(n, dt) {
      setnames(dt, old = grep(n, names(dt), value = TRUE), new = n)
    })
  }
  set(dt, NULL, "order", match(dt$vals, names(vals)))
  setkeyv(dt, c("simName", "order"))
  set(dt, NULL, "order", NULL)
  dt[]
}
