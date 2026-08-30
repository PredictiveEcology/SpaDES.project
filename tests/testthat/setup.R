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

# One temp library, shared by the whole test suite.
#
# setupTest() previously handed each test its own Require::tempdir2(.rndstr(1)),
# so nothing a test installed could be reused by the next one -- every
# setupProject() test that installs packages paid for the full dependency tree
# again, from source. A single shared library means each package is installed at
# most once per run, and being under tempdir() it never touches the developer's
# real library.
testLib <- file.path(tempdir(), "SpaDES.project-test-lib")
dir.create(testLib, recursive = TRUE, showWarnings = FALSE)
withr::defer(unlink(testLib, recursive = TRUE), teardown_env())
