% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imports.R
\docType{data}
\name{messageDF}
\alias{messageDF}
\alias{yellow}
\alias{blue}
\alias{green}
\alias{whereInStack}
\alias{messageVerbose}
\alias{compareVersion2}
\alias{downloadRepo}
\alias{splitGitRepo}
\alias{isGitHub}
\alias{fileRenameOrMove}
\alias{linkOrCopy}
\alias{.basePkgs}
\alias{DESCRIPTIONFileDeps}
\alias{getVersionOnRepos}
\alias{substitutePackages}
\alias{toPkgDTFull}
\alias{trimRedundancies}
\alias{DESCRIPTIONFileVersionV}
\alias{.rndstr}
\alias{.spatialPkgs}
\title{non-exported objects and functions from other packages}
\format{
An object of class \code{character} of length 30.

An object of class \code{character} of length 9.
}
\usage{
messageDF(df, round, verbose = getOption("Require.verbose"), verboseLevel = 1)

yellow(...)

blue(...)

green(...)

whereInStack(obj)

messageVerbose(..., verbose = getOption("Require.verbose"), verboseLevel = 1)

compareVersion2(version, versionSpec, inequality)

downloadRepo(
  gitRepo,
  subFolder,
  overwrite = FALSE,
  destDir = ".",
  verbose = getOption("Require.verbose")
)

splitGitRepo(gitRepo, default = "PredictiveEcology", masterOrMain = NULL)

isGitHub(pkg, filenames)

fileRenameOrMove(from, to)

linkOrCopy(from, to, allowSymlink = FALSE)

.basePkgs

DESCRIPTIONFileDeps(
  desc_path,
  which = c("Depends", "Imports", "LinkingTo"),
  keepVersionNumber = TRUE,
  purge = getOption("Require.purge", FALSE),
  keepSeparate = FALSE
)

getVersionOnRepos(
  pkgInstall,
  repos,
  purge,
  libPaths,
  type = getOption("pkgType")
)

substitutePackages(packagesSubstituted, envir = parent.frame())

toPkgDTFull(pkgDT)

trimRedundancies(
  pkgInstall,
  repos,
  purge,
  libPaths,
  verbose = getOption("Require.verbose"),
  type = getOption("pkgType")
)

DESCRIPTIONFileVersionV(file, purge = getOption("Require.purge", FALSE))

.rndstr(n = 1, len = 8)

.spatialPkgs
}
\description{
non-exported objects and functions from other packages
}
\keyword{datasets}
\keyword{internal}
