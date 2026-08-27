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
      if (nzchar(Sys.getenv("GOOGLEDRIVE_AUTH"))) {
        # Failure here would block tests that don't actually need Drive creds,
        # so swallow it; tests that genuinely need a token will fail clearly.
        try(googledrive::drive_auth(path = Sys.getenv("GOOGLEDRIVE_AUTH")),
            silent = TRUE)
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

  withr::local_libpaths(lib, .local_envir = envir)

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
