## simLists class + small helpers, ported from SpaDES.experiment (now unmaintained).
## SpaDES.core is in Suggests here, so it is accessed via requireNamespace / `::`
## rather than @importFrom (which would require it in Imports/Depends).

#' @keywords internal
#' @noRd
updateNames <- function(lst, newNames) {
  namesVals <- names(lst)
  emptyChar <- nchar(namesVals) == 0
  if (is.null(namesVals) || any(emptyChar)) {
    if (missing(newNames)) {
      newNames <- unlist(lapply(seq_along(lst), function(x) {
        if (is.name(lst[[x]])) {
          paste(collapse = "_", format(as.character(lst[[x]])))
        } else {
          paste(collapse = "_", format(lst[[x]]))
        }
      }))
    }
    if (any(emptyChar)) {
      namesVals[emptyChar] <- newNames[emptyChar]
      newNames <- namesVals
    }
    names(lst) <- newNames
  }
  lst
}

#' @keywords internal
#' @noRd
.objNamesBySimList <- function(simLists) {
  objs <- ls(simLists)
  simLists <- gsub("_.*", "", objs)
  simListsBySimList <- split(objs, f = simLists)
  simListsBySimList <- lapply(simListsBySimList, sort)
}

#' The `simLists` class
#'
#' This is a grouping of `simList` objects. Normally this class will be
#' made using [experiment2()], but can be made manually if there are
#' existing `simList` objects.
#'
#' This class (and the [experiment()] / [experiment2()] functions that
#' produce it) was moved here from the now-unmaintained `SpaDES.experiment`
#' package.
#'
#' @slot paths      Named list of `modulePath`, `inputPath`,
#'                  and `outputPath` paths. Partial matching is performed. These
#'                  will be prepended to the relative paths of each `simList`
#' @slot .xData  Environment holding the `simLists`.
#'
#' @aliases simLists
#' @author Eliot McIntire
#' @exportClass simLists
#' @importFrom data.table as.data.table data.table
#' @rdname simLists-class
setClass(
  "simLists",
  contains = "environment",
  slots = list(
    .xData = "environment", paths = "list"
  ),
  validity = function(object) {
    return(object)
  }
)

#' Generate a `simLists` object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, `new` returns an object from that class.
#'
#' @param .Object  A `simList` object.
#' @param ... Optional Values passed to any or all slot
#'
#' @export
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "simLists"),
          definition = function(.Object, ...) {

            .Object@paths <- if (requireNamespace("SpaDES.core", quietly = TRUE)) {
              getFromNamespace(".paths", "SpaDES.core")()
            } else {
              list()
            }

            .Object@.xData <- new.env(parent = emptyenv())

            attr(.Object@.xData, "name") <- "simLists"
            return(.Object)
})

#' Show method for `simLists`
#' @param object  `simLists`
#'
#' @author Eliot McIntire
#' @importFrom utils capture.output ls.str tail getFromNamespace
#' @include simLists-class.R
#' @export
setMethod(
  "show",
  signature = "simLists",
  definition = function(object) {
    out <- list()
    out[[1]] <- capture.output(
      cat(rep("=", getOption("width"), sep = ""), "\n", sep = "")
    )

    simListsBySimList <- .objNamesBySimList(object)
    simLists <- unlist(simListsBySimList)
    simLists <- gsub("_.*", "", simLists)

    lengths <- lapply(simListsBySimList, length)
    uniqueLengths <- unique(unlist(lengths))
    out2 <- paste(">> ", length(unique(simLists)), "simLists;")
    out3 <- if (length(uniqueLengths) == 1) {
      paste("with", uniqueLengths, "replicates each")
    } else if (isTRUE(uniqueLengths) == 1) {
      paste0("with only 1 replicate each")
    }
    out[[2]] <- capture.output(cat(out2, out3))
    ll <- lapply(simListsBySimList, function(s) {
      paste0(s[1], ", ..., ", tail(s, 1))
    })
    simListChStr <- paste0(names(ll), ": ", ll)
    simListEntries <- (seq_along(unique(simLists)) - 1) * 2 + length(out) + 1
    out[simListEntries] <- lapply(simListChStr, function(x) x)
    out[simListEntries + 1] <- lapply(simListsBySimList, function(x) {
      paste("  ", capture.output(ls.str(object[[x[1]]])))
    })

    out[[length(out) + 1]] <- capture.output(cat("\n"))
    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
})
