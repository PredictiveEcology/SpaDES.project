.rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <-
      paste0(sample(
        c(0:9, letters, LETTERS),
        size = len,
        replace = TRUE
      ), collapse = "")
  }))
}


setupTest <- function(pkgs, envir = parent.frame(), name = .rndstr(1), first = FALSE) {

  options(Map(o = names(spadesProjectOptions()), function(o) NULL))

  origLibPaths <- get0("origLibPaths", envir = envir)
  if (is.null(origLibPaths))
    origLibPaths <- .libPaths()
  # Also include covr's instrumented temp lib paths so coverage tracking is not lost
  covrPaths <- get0("covrLibPaths", .GlobalEnv, inherits = TRUE)
  origLibPaths <- unique(c(covrPaths, origLibPaths))

  # Packages below are *loaded* from the session's real libraries, so widen
  # .libPaths() only for that. This deliberately uses with_libpaths(), not
  # local_libpaths(): the latter is scoped to setupTest()'s own frame, so its
  # restore ran when setupTest() RETURNED -- i.e. after the caller-scoped
  # local_libpaths(lib) below -- handing every caller the real library back.
  # setupProject(paths = list(packagePath = .libPaths()[1L])) then installed
  # into the developer's real library instead of a temp one.
  withr::with_libpaths(origLibPaths, {
  warns <- capture_warnings({
    # withr::local_package("googledrive", .local_envir = envir)
    # withr::local_package("curl", .local_envir = envir)
    # if (!isNamespaceLoaded("crayon"))
    #   withr::local_package("crayon", .local_envir = envir)
    # if (!isNamespaceLoaded("httr"))
    #   withr::local_package("httr", .local_envir = envir)
    # if (!isNamespaceLoaded("waldo"))
    #   withr::local_package("waldo", .local_envir = envir)
    # if (!isNamespaceLoaded("rematch2"))
    #   withr::local_package("rematch2", .local_envir = envir)
    # if (!isNamespaceLoaded("diffobj"))
    #   withr::local_package("diffobj", .local_envir = envir)
    # if (!isNamespaceLoaded("terra"))
    #   withr::local_package("terra", .local_envir = envir)
  })

  if (isNamespaceLoaded("googledrive"))
    if ((!googledrive::drive_has_token())) {
      # CI stages a serialized user OAuth token and exports its path as
      # GDRIVE_OAUTH_TOKEN; it needs drive_auth(token=), not drive_auth(path=).
      # Service accounts are not supported: they have no Drive quota on
      # user-owned folders, so they authenticate but cannot complete an upload
      # round-trip, which silently leaves the cloud paths uncovered.
      tokenPath <- Sys.getenv("GDRIVE_OAUTH_TOKEN")
      if (nzchar(tokenPath) && file.exists(tokenPath)) {
        tok <- tryCatch(readRDS(tokenPath), error = function(e) NULL)
        if (!is.null(tok)) {
          ## Drop the token's own cache_path before using it, as
          ## reproducible's tests/testthat/setup.R does. drive_auth() writes
          ## the refreshed token back to that path, which is wherever the
          ## token was MINTED (e.g. ~/.secret on a dev machine). On a runner
          ## that directory does not exist, the write fails, and gargle
          ## reports "Can't get Google credentials" -- indistinguishable from
          ## having no credential at all. A runner should not persist a
          ## credential to disk anyway.
          tok$cache_path <- NULL
          # Report rather than swallow: a failure here used to surface far
          # downstream as an unrelated git error.
          tryCatch(googledrive::drive_auth(token = tok),
                   error = function(e)
                     warning("Drive auth from GDRIVE_OAUTH_TOKEN failed; ",
                             "Drive-backed tests will not be exercised: ",
                             conditionMessage(e), call. = FALSE))
        }
      }
    }

  if (!missing(pkgs)) {
    lapply(pkgs, function(pk) {
      skip_if_not_installed(pk)
      withr::local_package(pk, .local_envir = envir)
    })
  }
  }) # end with_libpaths(origLibPaths)

  # The suite-wide temp library created in setup.R. Every test shares it, so a
  # package installed by one test is available to the rest instead of being
  # rebuilt per test. `first` is retained for call-site compatibility but no
  # longer selects a different (persistent) library.
  lib <- get0("testLib", .GlobalEnv, inherits = TRUE)
  if (is.null(lib)) {
    # test_file()/manual use, where setup.R has not run
    lib <- file.path(tempdir(), "SpaDES.project-test-lib")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  }

  # Set the caller's path explicitly to `lib` followed by the libraries this
  # session started with:
  #   * `lib` first, so anything a test installs lands in the temp library;
  #   * origLibPaths after, so tests can still LOAD packages they did not
  #     install (s2, curl, terra ...).
  #
  # Built from origLibPaths rather than with action = "prefix", which prepends
  # to .libPaths() *as it currently stands*. That is not safe here: setupProject()
  # narrows .libPaths() to the project library as part of what it does, so by the
  # time a later test calls setupTest() the real libraries may already be gone,
  # and prefixing would preserve their absence. The old scoping bug happened to
  # paper over this by resetting the path on every setupTest() call; this does
  # the reset deliberately instead of relying on ambient state.
  withr::local_libpaths(unique(c(lib, origLibPaths)), .local_envir = envir)

  withr::local_dir(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org"),
      Require.verbose = 5,
      Require.cloneFrom = lib),
    .local_envir = envir)
  # withr::local_options(.local_envir = envir,
  #   list(repos = c(CRAN = "https://cloud.r-project.org"))
  # )

}

# setupTest() clears the spades.* path options -- projectPath, packagePath,
# inputPath, modulePath, outputPath, scratchPath -- with a bare options() call,
# so they are gone for the rest of the run. SpaDES.core::simInit() reads them,
# and without them fails with "The modules argument is specified incorrectly"
# or "Invalid path: cannot be NA". Any test that builds a simList after
# test-setupProject.R has run therefore has to put SpaDES.core's defaults back.
# Call this rather than relying on test file ordering.
localSpadesOptions <- function(envir = parent.frame()) {
  if (!requireNamespace("SpaDES.core", quietly = TRUE)) return(invisible(NULL))
  suppressMessages(
    withr::local_options(SpaDES.core::spadesOptions(), .local_envir = envir)
  )
  invisible(NULL)
}
