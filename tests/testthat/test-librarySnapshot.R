## Shared-library safety for experiment workers (#163): sync once per launch,
## snapshot per job. No tmux, no network; a tiny fake library in tempdir().

mkFakeLib <- function(root = withr::local_tempdir(.local_envir = parent.frame())) {
  lib <- file.path(root, "lib")
  for (p in c("pkgA", "pkgB")) {
    dir.create(file.path(lib, p, "R"), recursive = TRUE)
    writeLines(c(paste0("Package: ", p), "Version: 0.1.0"), file.path(lib, p, "DESCRIPTION"))
    writeBin(as.raw(1:64), file.path(lib, p, "R", paste0(p, ".rdb")))
  }
  writeLines("hidden", file.path(lib, ".hidden"))
  lib
}

inode <- function(p) {
  fi <- tryCatch(fs::file_info(p), error = function(e) NULL)
  if (is.null(fi) || is.null(fi$inode)) NA_real_ else as.numeric(fi$inode)
}

test_that("snapshotLibrary replicates the tree by hardlink and records its origin", {
  lib  <- mkFakeLib()
  where <- withr::local_tempdir()
  snap <- snapshotLibrary(lib, where = where, id = "host_123", setLibPaths = FALSE)
  expect_identical(basename(snap), "lib_host_123")
  expect_setequal(list.files(snap, recursive = TRUE, all.files = TRUE, no.. = TRUE),
                  c(list.files(lib, recursive = TRUE, all.files = TRUE, no.. = TRUE), ".snapshot_of"))
  expect_identical(readLines(file.path(snap, ".snapshot_of"))[1], normalizePath(lib))
  skip_on_os("windows")
  skip_if_not_installed("fs")
  i1 <- inode(file.path(lib, "pkgA", "R", "pkgA.rdb")); i2 <- inode(file.path(snap, "pkgA", "R", "pkgA.rdb"))
  if (!is.na(i1) && !is.na(i2)) expect_identical(i1, i2)   # same filesystem -> hardlink
})

test_that("a replaced file in the shared library does not change the snapshot", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  snap  <- snapshotLibrary(lib, where = where, id = "host_124", setLibPaths = FALSE)
  # what an install does: write a new file and move it over the old one
  tmp <- file.path(lib, "pkgA", "R", "new.rdb"); writeBin(as.raw(101:164), tmp)
  file.rename(tmp, file.path(lib, "pkgA", "R", "pkgA.rdb"))
  expect_identical(readBin(file.path(snap, "pkgA", "R", "pkgA.rdb"), "raw", 64), as.raw(1:64))
  expect_identical(readBin(file.path(lib,  "pkgA", "R", "pkgA.rdb"), "raw", 64), as.raw(101:164))
})

test_that("snapshotLibrary(setLibPaths = TRUE) puts the snapshot first and drops the original", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  old <- .libPaths(); withr::defer(.libPaths(old))
  withr::local_envvar(SPADES_PROJECT_LIB_SNAPSHOT = NA)
  snap <- snapshotLibrary(lib, where = where, id = "host_125")
  expect_identical(normalizePath(.libPaths()[1]), normalizePath(snap))
  expect_false(normalizePath(lib) %in% normalizePath(.libPaths()))
  expect_identical(normalizePath(Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT")), normalizePath(snap))
  expect_true(releaseLibrarySnapshot(snap))
  expect_false(dir.exists(snap))
  expect_identical(Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT"), "")
})

test_that("releaseLibrarySnapshot refuses a directory that is not a snapshot", {
  d <- withr::local_tempdir()
  expect_error(releaseLibrarySnapshot(d), "Not a library snapshot")
  expect_true(dir.exists(d))
})

test_that("sweepLibrarySnapshots removes dead owners on this host and keeps live and foreign ones", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  me    <- Sys.info()[["nodename"]]
  live  <- snapshotLibrary(lib, where = where, id = paste0(me, "_", Sys.getpid()), setLibPaths = FALSE)
  dead  <- snapshotLibrary(lib, where = where, id = paste0(me, "_", 999999L), setLibPaths = FALSE)
  other <- snapshotLibrary(lib, where = where, id = "somewhere-else_1", setLibPaths = FALSE)
  removed <- sweepLibrarySnapshots(where)
  expect_identical(normalizePath(removed, mustWork = FALSE), normalizePath(dead, mustWork = FALSE))
  expect_true(dir.exists(live)); expect_true(dir.exists(other)); expect_false(dir.exists(dead))
})

test_that("libraryInUse sees a Running_ sentinel with a live pid and ignores a dead one", {
  arp <- withr::local_tempdir()
  saveRDS("x", file.path(arp, paste0("Running_job1_", Sys.getpid(), "_.rds")))
  saveRDS("x", file.path(arp, "Running_job2_999999_.rds"))
  busy <- libraryInUse(arp)
  expect_true(any(grepl(paste0("Running_job1_", Sys.getpid()), busy)))
  expect_false(any(grepl("Running_job2_999999", busy)))
})

test_that("syncProjectLibrary refuses while a worker is alive, and runs installFun in a fresh process", {
  skip_if_not_installed("callr")
  arp <- withr::local_tempdir()
  lib <- mkFakeLib()
  saveRDS("x", file.path(arp, paste0("Running_job1_", Sys.getpid(), "_.rds")))
  expect_error(syncProjectLibrary("pkgX", libPath = lib, activeRunningPath = arp,
                                  installFun = function(p, l) NULL, verbose = 0),
               "Refusing to install")
  unlink(file.path(arp, paste0("Running_job1_", Sys.getpid(), "_.rds")))
  marker <- file.path(arp, "installed.txt")
  ## requireIdle = FALSE: this machine may legitimately be running other experiments'
  ## workers (it does, on the FireSense cluster); the guard itself is tested above.
  ok <- syncProjectLibrary(c("pkgX", "pkgY"), libPath = lib, activeRunningPath = arp, verbose = 0,
                           requireIdle = FALSE,
                           installFun = function(p, l) writeLines(c(p, l, as.character(Sys.getpid())), marker))
  expect_true(ok)
  got <- readLines(marker)
  expect_identical(got[1:2], c("pkgX", "pkgY"))
  expect_identical(normalizePath(got[3]), normalizePath(lib))
  expect_false(identical(got[4], as.character(Sys.getpid())))   # a different process
  expect_false(syncProjectLibrary(character(), libPath = lib, activeRunningPath = arp, requireIdle = FALSE))
})

test_that(".librarySnapshotCode runs in a bare session, before any package, and switches .libPaths()", {
  skip_if_not_installed("callr")
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  code  <- SpaDES.project:::.librarySnapshotCode(lib, where)
  out <- callr::r(function(code) {
    eval(parse(text = code))
    list(lib1 = .libPaths()[1], env = Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT"), loaded = loadedNamespaces())
  }, args = list(code = code))
  expect_true(startsWith(basename(out$lib1), "lib_"))
  expect_identical(normalizePath(dirname(out$lib1)), normalizePath(where))
  expect_identical(normalizePath(out$env), normalizePath(out$lib1))
  expect_false("SpaDES.project" %in% out$loaded)
  expect_true(file.exists(file.path(out$lib1, "pkgA", "DESCRIPTION")))
})

test_that(".build_worker_r_expr snapshots before tmuxRunWorkerLoop, and can be told not to", {
  mk <- function(snapshot) SpaDES.project:::.build_worker_r_expr(
    queue_path = "q.rds", global_path = "global.R", on_interrupt = "requeue",
    runNameLabel = quote(colnames(q)[1]), activeRunningPath = "logs/q",
    ss_id = NULL, pane_mode = "killAndNewPane", email = NULL, cache_path = NULL,
    dots_path = NULL, lib_path = "/lib", snapshot_library = snapshot)
  e <- mk(TRUE)
  expect_true(startsWith(e, "Sys.unsetenv('R_PROFILE_USER'); "))
  expect_lt(regexpr(".snapBase", e, fixed = TRUE), regexpr("tmuxRunWorkerLoop", e, fixed = TRUE))
  expect_match(e, "snapshot_library=TRUE", fixed = TRUE)
  e0 <- mk(FALSE)
  expect_false(grepl(".snapBase", e0, fixed = TRUE))
  expect_match(e0, "snapshot_library=FALSE", fixed = TRUE)
})

test_that(".isWorkerCmdline: an R process calling the loop, not any line mentioning it", {
  f <- SpaDES.project:::.isWorkerCmdline
  expect_true(f("1234 /opt/R/4.6.1/lib/R/bin/exec/R --no-echo -e SpaDES.project::tmuxRunWorkerLoop(queue_path=\"q.rds\")"))
  expect_true(f("1234 Rscript -e .libPaths(\"/lib\"); SpaDES.project::tmuxRunWorkerLoop(queue_path = \"q\")"))
  expect_true(f("99 /usr/bin/Rscript -e runWorkerLoopFuture(queue_path=q)"))
  # this package's own test harness, an editor, a grep: not workers
  expect_false(f("5 /bin/bash -c Rscript -e testthat::test_file(\"tests/testthat/test-tmuxRunWorkerLoop.R\")"))
  expect_false(f("6 grep -rn tmuxRunWorkerLoop( R/"))
  expect_false(f("7 vim R/tmux.R tmuxRunWorkerLoop("))
  # a tmux worker keeps the call in its profile: no match on the command line
  expect_false(f("8 /opt/R/4.6.1/lib/R/bin/exec/R --quiet --no-save --no-restore --interactive"))
})

test_that("libraryInUse does not report the calling process or a harness that merely names the loop", {
  withr::local_envvar(SPADES_PROJECT_LIB_SNAPSHOT = NA)
  busy <- libraryInUse(withr::local_tempdir())
  expect_false(any(grepl(paste0("^", Sys.getpid(), " "), busy)))
})

test_that(".pidsAlive knows this process is alive and a wild pid is not", {
  f <- SpaDES.project:::.pidsAlive
  expect_identical(f(c(Sys.getpid(), 999999L, NA_integer_)), c(TRUE, FALSE, FALSE))
})
