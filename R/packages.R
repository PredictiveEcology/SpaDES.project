#' Extract element from SpaDES module metadata
#'
#' Parses module code, looking for the `metadataItem` (default = `"reqdPkgs"`)
#' element in the `defineModule` function.
#'
#' @param modules character vector of module names
#'
#' @param modulePath path to directory containing the module(s) named in `modules`
#'
#' @return A character vector of sorted, unique packages that are identified in all named
#' modules, or if `modules` is omitted, then all modules in `modulePath`.
#'
#' @export
#' @rdname metadata
packagesInModules <- function(modules, modulePath = getOption("spades.modulePath")) {
  metadataInModules(modulePath = modulePath, modules = modules, metadataItem = "reqdPkgs")
}

#' @param metadataItem character identifying the metadata field to extract
#'
#' @param needUnlist logical indicating whether to `unlist` the resulting metadata look up
#'
#' @inheritParams Require::Require
#' @export
#' @rdname metadata
metadataInModules <- function(modules, metadataItem = "reqdPkgs",
                              modulePath = getOption("spades.modulePath"), needUnlist,
                              verbose = getOption("Require.verbose", 1L)) {
  if (missing(modules))
    modules <- dir(modulePath)
  names(modules) <- modules

  if (missing(needUnlist)) {
    needUnlistInner <- switch(metadataItem, reqdPkgs = TRUE, version = FALSE, authors = FALSE, FALSE)
    needUnlistOuter <- switch(metadataItem, reqdPkgs = FALSE, version = TRUE, authors = FALSE, FALSE)
  } else {
    needUnlistInner <- needUnlistOuter <- needUnlist
  }

  vals <- lapply(modules, function(mod) {
    for (i in 1:2) {
      modPath <- file.path(modulePath, mod, paste0(mod, ".R"))
      feMP <- file.exists(modPath)
      if (!feMP)
        modulePath <- "."
      else
        break
    }
    if (feMP) {
      pp <- parse(file = modPath)
      wh <- unlist(lapply(pp, grep, pattern = "defineModule"))
      wh2 <- which(unlist(lapply(pp[[1]], function(x)
        any(grepl(pattern = metadataItem, format(x))))))
      if (length(wh2)) {
        val <- eval(pp[[wh]][[wh2]][[metadataItem]])
        if (identical(metadataItem, "version")) {
          val <- lapply(val, as.character)
          hasSpaDES.core <- names(val) == "SpaDES.core"
          val <- unname(val)
          if (any(hasSpaDES.core))
            val <- val[!hasSpaDES.core]
        }
        if (needUnlistInner)
          val <- unlist(val)
      } else {
        messageVerbose("Skipping ", metadataItem, " in ", modules, "; it is empty",
                       verbose = verbose)
        val <- NULL
      }
      val
    }
  })
  vals <- vals[!unlist(lapply(vals, is.null))]

  if (needUnlistOuter) {
    vals2 <- unlist(vals, recursive = FALSE)
    dups <- duplicated(vals2)
    vals <- try(sort(vals2[!dups]), silent = TRUE)
    if (is(vals, "try-error"))
      vals <- vals2[!dups]
  }
  vals
}

isRstudio <- function() {
  Sys.getenv("RSTUDIO") == 1 || .Platform$GUI == "RStudio" ||
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::isAvailable()
    }
  else {
    FALSE
  }
}


bindrows <- function(...) {
  # Deal with things like "trailing commas"
  rws <- try(list(...), silent = TRUE)
  if (any(grepl("argument is missing|bind_rows", rws))) {
    ll <- as.list(match.call(expand.dots = TRUE))
    nonEmpties <- unlist(lapply(ll, function(x) any(nchar(x) > 0)))
    eval(as.call(ll[nonEmpties]))
  } else if (is(rws, "try-error")) {
    stop(rws)
  } else {
    rbindlist(rws, fill = TRUE, use.names = TRUE)
  }
}
