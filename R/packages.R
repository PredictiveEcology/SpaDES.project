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
        modPath <- dir(modulePath, recursive = TRUE, pattern = paste0(mod, ".R"))
      else
        break
    }
    if (feMP) {
      pp <- parse(file = modPath)
      wh <- grep("^defineModule", pp)
      wh2 <- which(unlist(lapply(pp[[wh]], function(x)
        any(grepl(pattern = metadataItem, format(x))))))
      if (length(wh2)) {
        val <- try(eval(pp[[wh]][[wh2]][[metadataItem]]), silent = TRUE)
        for (ii in 1:2)
          if (is(val, "try-error")) {
            if (identical(metadataItem, "reqdPkgs") && ii == 1) {
              val <- substitutePackages(pp[[wh]][[wh2]][[metadataItem]])
              next
            }
            val <- pp[[wh]][[wh2]][[metadataItem]]
          }
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

## Seam for interactive(): a function of our own can be mocked in tests, whereas
## base::interactive() reports the session and cannot. Behaviour is identical.
isInteractive <- function() interactive()

# Positron's R kernel (ark) attaches a `tools:positron` environment and, on
# `rstudioapi` load, rewrites the body of `rstudioapi::isAvailable()` to
# `TRUE` so that rstudioapi-using packages keep working.  That means
# `hasRstudioApi()` below is TRUE in Positron as well as in RStudio, and any
# code that needs *RStudio specifically* (an .Rproj project, the
# `rstudio.sessionInit` hook) must use `isRstudio()`, not `hasRstudioApi()`.
#
# Detection uses the POSITRON environment variable, which the supervisor sets
# to "1" for every R session it starts.  `.Platform$GUI == "Positron"` and the
# `positron.session_init` hook are only available in Positron >= 2026.04, so
# they are not safe to detect with.
isPositron <- function() {
  Sys.getenv("POSITRON") == "1" || "tools:positron" %in% search()
}

# TRUE when the `rstudioapi` shims are usable, whichever front-end provides
# them (RStudio itself, or Positron's emulation).
hasRstudioApi <- function() {
  isTRUE(Sys.getenv("RSTUDIO") == 1) || isTRUE(.Platform$GUI == "RStudio") ||
    isTRUE(tryCatch(requireNamespace("rstudioapi", quietly = TRUE) &&
                      rstudioapi::isAvailable(),
                    error = function(e) FALSE))
}

isRstudio <- function() {
  hasRstudioApi() && !isPositron()
}


