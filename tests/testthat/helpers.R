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

  warns <- capture_warnings({
    # withr::local_package("googledrive", .local_envir = envir)
    # withr::local_package("curl", .local_envir = envir)
    withr::local_package("crayon", .local_envir = envir)
    withr::local_package("httr", .local_envir = envir)
    withr::local_package("waldo", .local_envir = envir)
    withr::local_package("rematch2", .local_envir = envir)
    withr::local_package("diffobj", .local_envir = envir)
    withr::local_package("terra", .local_envir = envir)
  })

  # if (user("emcintir")) {
  #   if (!googledrive::drive_has_token()) {
  #     options(gargle_oauth_cache = "/home/emcintir/.secret",
  #             gargle_oauth_email = "eliotmcintire@gmail.com")
  #     googledrive::drive_auth()
  #   }
  # }

  if (!missing(pkgs)) {
    lapply(pkgs, function(pk) {
      skip_if_not_installed(pk)
      withr::local_package(pk, .local_envir = envir)
    })
  }

  # withr::local_libpaths(Require::tempdir2(.rndstr(1)), .local_envir = envir)

  libStable <- .libPathDefault("test_SpaDES_project")
  if (isTRUE(first)) {
    lib <- checkPath(libStable, create = TRUE)
  } else {
    lib <- Require::tempdir2(.rndstr(1))
  }


  withr::local_libpaths(lib, .local_envir = envir)

  withr::local_dir(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org"),
      Require.verbose = 5,
      Require.cloneFrom = libStable),
    .local_envir = envir)
  # withr::local_options(.local_envir = envir,
  #   list(repos = c(CRAN = "https://cloud.r-project.org"))
  # )

}
