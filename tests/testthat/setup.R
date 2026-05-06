# On R 4.6 + ubuntu-noble + R CMD check, .libPaths() during the testthat run
# sometimes does not include the user/site lib where setup-r-dependencies
# installed Suggests deps (terra, reproducible, geodata, gert, ...). The result
# is `requireNamespace("terra")` returning FALSE inside tests even though pak
# logged a successful install. Belt-and-suspenders: prepend any candidate lib
# that exists before we capture origLibPaths for setupTest().
for (lp in c(Sys.getenv("R_LIBS_USER", unset = ""),
             Sys.getenv("R_LIBS_SITE", unset = ""),
             Sys.getenv("R_LIB_FOR_PAK", unset = ""))) {
  if (nzchar(lp) && dir.exists(lp) && !lp %in% .libPaths()) {
    .libPaths(c(.libPaths(), lp))
  }
}

origLibPaths <- .libPaths()
covrLibPaths <- .libPaths()  # capture early, before any modification (includes covr's temp path if running under covr)
if (tryCatch(packageVersion("crayon") > "0", error = function(e) FALSE))
  withr::local_package("crayon", .local_envir = teardown_env())
if (tryCatch(packageVersion("waldo") > "0", error = function(e) FALSE))
  withr::local_package("waldo", .local_envir = teardown_env())
if (tryCatch(packageVersion("rematch2") > "0", error = function(e) FALSE))
  withr::local_package("rematch2", .local_envir = teardown_env())
if (tryCatch(packageVersion("diffobj") > "0", error = function(e) FALSE))
  withr::local_package("diffobj", .local_envir = teardown_env())
if (tryCatch(packageVersion("terra") > "0", error = function(e) FALSE))
  requireNamespace("terra", quietly = TRUE)
