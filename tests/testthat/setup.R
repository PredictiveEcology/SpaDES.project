# withr::local_options(list(repos = c(CRAN = "https://cloud.r-project.org")),
#                      .local_envir = teardown_env())
#
# # origLibPaths <- .libPaths()
# withr::local_libpaths(Require::tempdir2(.rndstr(1)))
# # withr::defer(.libPaths(origLibPaths), .local_envir = teardown_env())
# # withr::defer(.teardownProject(out$paths, origLibPaths))
#
# browser()
# tmpdir <- Require::tempdir2(sub = .rndstr(1))
# origWD <- getwd()
# setwd(tmpdir)
# withr::defer(setwd(origWD))
# withr::local_libpaths(Require::tempdir2(.rndstr(1)))
# withr::local_dir(Require::tempdir2(.rndstr(1)))
# browser()
#
#
# # withr::local_options(Require.RPackageCache = RequirePkgCacheDir(), .local_envir = teardown_env())
origLibPaths <- .libPaths()
withr::local_package("crayon", .local_envir = teardown_env())
withr::local_package("waldo", .local_envir = teardown_env())
withr::local_package("rematch2", .local_envir = teardown_env())
withr::local_package("diffobj", .local_envir = teardown_env())
requireNamespace("terra", quietly = TRUE)
