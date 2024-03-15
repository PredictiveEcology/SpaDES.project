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


setupTest <- function(envir = parent.frame()) {
  withr::local_libpaths(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_dir(Require::tempdir2(.rndstr(1)), .local_envir = envir)
  withr::local_options(list(repos = c(CRAN = "https://cloud.r-project.org")),
                       .local_envir = envir)
  withr::local_options(.local_envir = envir,
    list(repos = c(CRAN = "https://cloud.r-project.org"))
  )

}
