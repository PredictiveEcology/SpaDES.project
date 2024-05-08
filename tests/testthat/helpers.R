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


setupTest <- function(pkgs, envir = parent.frame()) {

  warns <- capture_warnings({
  withr::local_package("googledrive", .local_envir = envir)
  withr::local_package("curl", .local_envir = envir)
  withr::local_package("crayon", .local_envir = envir)
  withr::local_package("httr", .local_envir = envir)
  withr::local_package("waldo", .local_envir = envir)
  withr::local_package("rematch2", .local_envir = envir)
  withr::local_package("diffobj", .local_envir = envir)
  withr::local_package("terra", .local_envir = envir)
  })

  if (user("emcintir")) {
    if (!googledrive::drive_has_token())
      googledrive::drive_auth(email = "eliotmcintire@gmail.com")
  }

  if (!missing(pkgs))
    lapply(pkgs, withr::local_package, .local_envir = envir)
  withr::local_libpaths(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_dir(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_options(
    list(
      repos = c(CRAN = "https://cloud.r-project.org"),
      "Require.verbose" = 5),
    .local_envir = envir)
  # withr::local_options(.local_envir = envir,
  #   list(repos = c(CRAN = "https://cloud.r-project.org"))
  # )

}
