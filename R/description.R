#' Create or modify a project's `DESCRIPTION` file
#'
#' A simple wrapper around `usethis::use_description()` to write
#' a `DESCRIPTION` file in the root project directory.
#' This file can be used to record project metadata, including package version
#' information from the current project library.
#'
#' @note fields Imports and Remotes will be automatically populated based on `snapshot`
#' (or, if `snapshot = NULL`, the currently installed packages).
#'
#' @param fields named list of `DESCRIPTION` fields and their values.
#'
#' @param snapshot character. path to a [Require::pkgSnapshot()] file.
#'
#' @return `NULL` invisibly.
#' Invoked for side effect of writing a DESCRIPTION file to the project directory,
#' and printing the resulting DESCCRIPTION file to screen.
#'
#' @export
#' @importFrom Require modifyList3 pkgSnapshot
#' @importFrom utils read.csv
#'
#' @examples
#' tmpdir <- file.path(tempdir(), "example_project")
#' cwd <- setwd(tmpdir)
#'
#' description(fields = list(
#'   Title = "My Project: It Does Cool Stuff",
#'   Description = paste(
#'     "My project does so many cool things.",
#'     "It's so useful for all the things."
#'   ),
#'   `Authors@R` = "c(
#'     person('First', 'Last', , 'email@email.com', role = 'aut')
#'   )",
#'   Version = "0.0.1",
#'   Language = "en-CA",
#'   License = "MIT",
#'   Depends = paste0("R (== 4.2)", collapse = ",\n    ")
#' ))
#'
#' setwd(cwd)
#' unlink(tmpdir, recursive = TRUE)
description <- function(fields = list(), snapshot = NULL) {
  stopifnot(requireNamespace("usethis", quietly = TRUE))

  if (is.null(snapshot)) {
    snapshot <- tempfile("pkgsnapshot_", fileext = ".csv")
    pkgSnapshot(snapshot) ## TODO: need 93-snapshot branch of Require
    on.exit(unlink(snapshot), add = TRUE)
  }

  pkgs <- read.csv(snapshot)
  cranPkgs <- pkgs[is.na(pkgs$GithubUsername), ]
  ghPkgs <- pkgs[!is.na(pkgs$GithubUsername), ]

  ## voerride user-specified field values
  fields <- modifyList3(fields, list(
    Type = "project",
    Package = NULL,
    Imports = paste0(pkgs$Package, " (== ", pkgs$Version, ")", collapse = ",\n    "),
    Remotes = paste0(ghPkgs$GithubUsername, "/", ghPkgs$GithubRepo, "@",
                     ghPkgs$GithubSHA1, collapse = ",\n    ")
  ))

  usethis::proj_set(findProjectPath(), force = TRUE)
  usethis::use_description(fields = fields, check_name = FALSE, roxygen = FALSE)

  return(invisible(NULL))
}
