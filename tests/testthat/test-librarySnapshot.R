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
  expect_identical(normalizePath(dirname(out$lib1)), normalizePath(SpaDES.project:::.snapshotDir(lib)))
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

test_that("two jobs with the same nodename and pid get separate snapshots, and the second leaves the first alone", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  withr::local_envvar(SLURM_JOB_ID = "1001", SLURM_PROCID = NA)
  s1 <- SpaDES.project:::.snapshotLibraryBase(lib, where)
  withr::local_envvar(SLURM_JOB_ID = "1002")
  s2 <- SpaDES.project:::.snapshotLibraryBase(lib, where)
  expect_false(identical(s1, s2))
  expect_match(basename(s1), paste0("^lib_", Sys.info()[["nodename"]], "_", Sys.getpid(), "-j1001$"))
  expect_true(file.exists(file.path(s1, "pkgA", "DESCRIPTION")))
  expect_true(file.exists(file.path(s2, "pkgA", "DESCRIPTION")))
  # without Slurm the token is random
  withr::local_envvar(SLURM_JOB_ID = NA)
  r1 <- SpaDES.project:::.snapshotLibraryBase(lib, where); r2 <- SpaDES.project:::.snapshotLibraryBase(lib, where)
  expect_false(identical(r1, r2))
})

test_that("a snapshot directory that already exists is never removed or reused", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  s1 <- snapshotLibrary(lib, where = where, id = "host_5", setLibPaths = FALSE)
  writeLines("live", file.path(s1, "pkgA", "extra"))
  s2 <- snapshotLibrary(lib, where = where, id = "host_5", setLibPaths = FALSE)
  expect_false(identical(s1, s2))
  expect_true(file.exists(file.path(s1, "pkgA", "extra")))
  expect_true(file.exists(file.path(s2, "pkgA", "DESCRIPTION")))
})

test_that("sweepLibrarySnapshots leaves a snapshot from another PID namespace, even with a dead pid", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  me    <- Sys.info()[["nodename"]]
  foreign <- snapshotLibrary(lib, where = where, id = paste0(me, "_", 999999L, "-j7"), setLibPaths = FALSE)
  marker <- file.path(foreign, ".snapshot_of")
  writeLines(c(readLines(marker)[1:3], "pidns=pid:[1]"), marker)   # not this sweeper's namespace
  mine <- snapshotLibrary(lib, where = where, id = paste0(me, "_", 999998L, "-j8"), setLibPaths = FALSE)
  writeLines(c(readLines(file.path(mine, ".snapshot_of"))[1:3],
               paste0("pidns=", if (is.na(SpaDES.project:::.pidNamespace())) "NA" else SpaDES.project:::.pidNamespace())),
             file.path(mine, ".snapshot_of"))
  removed <- sweepLibrarySnapshots(where)
  expect_true(dir.exists(foreign))
  expect_identical(normalizePath(removed, mustWork = FALSE), normalizePath(mine, mustWork = FALSE))
})

test_that("old-format snapshot names and markers are swept as before", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  me    <- Sys.info()[["nodename"]]
  old <- snapshotLibrary(lib, where = where, id = paste0(me, "_", 999997L), setLibPaths = FALSE)
  writeLines(readLines(file.path(old, ".snapshot_of"))[1:3], file.path(old, ".snapshot_of"))   # no pidns line
  live <- snapshotLibrary(lib, where = where, id = paste0(me, "_", Sys.getpid()), setLibPaths = FALSE)
  writeLines(readLines(file.path(live, ".snapshot_of"))[1:3], file.path(live, ".snapshot_of"))
  removed <- sweepLibrarySnapshots(where)
  expect_identical(normalizePath(removed, mustWork = FALSE), normalizePath(old, mustWork = FALSE))
  expect_true(dir.exists(live))
})

test_that("the default snapshot directory is beside the library, and its files are hardlinks", {
  lib  <- mkFakeLib()
  snap <- snapshotLibrary(lib, setLibPaths = FALSE)
  expect_identical(normalizePath(dirname(snap)),
                   normalizePath(file.path(dirname(lib), paste0(basename(lib), "_snapshots"))))
  expect_false(startsWith(normalizePath(snap), paste0(normalizePath(lib), "/")))   # not inside the library
  skip_on_os("windows")
  skip_if_not_installed("fs")
  i1 <- inode(file.path(lib, "pkgA", "R", "pkgA.rdb")); i2 <- inode(file.path(snap, "pkgA", "R", "pkgA.rdb"))
  skip_if(is.na(i1) || is.na(i2), "no inodes on this platform")
  expect_identical(i1, i2)
  expect_gt(as.numeric(fs::file_info(file.path(lib, "pkgA", "R", "pkgA.rdb"))$hard_links), 1)
})

test_that("`where` overrides the default location", {
  lib   <- mkFakeLib()
  where <- withr::local_tempdir()
  snap  <- snapshotLibrary(lib, where = where, setLibPaths = FALSE)
  expect_identical(normalizePath(dirname(snap)), normalizePath(where))
  expect_false(dir.exists(SpaDES.project:::.snapshotDir(lib)))
})

test_that("an unwritable directory beside the library falls back to activeRunningPath, with a message", {
  skip_on_os("windows")
  skip_if(Sys.info()[["user"]] == "root", "root can write anywhere")
  root <- withr::local_tempdir(); lib <- mkFakeLib(root)
  Sys.chmod(root, "555"); withr::defer(Sys.chmod(root, "755"))
  arp <- withr::local_tempdir()
  withr::local_options(spades.activeRunningPath = arp)
  expect_message(snap <- snapshotLibrary(lib, setLibPaths = FALSE), "cannot write")
  expect_identical(normalizePath(dirname(snap)), normalizePath(arp))
  expect_true(file.exists(file.path(snap, "pkgA", "DESCRIPTION")))
})

test_that("sweepLibrarySnapshots finds dead snapshots in the new and in the old location", {
  lib <- mkFakeLib()
  arp <- withr::local_tempdir()
  withr::local_options(spades.activeRunningPath = arp)
  me   <- Sys.info()[["nodename"]]
  newd <- snapshotLibrary(lib, id = paste0(me, "_999996"), setLibPaths = FALSE)
  oldd <- snapshotLibrary(lib, where = arp, id = paste0(me, "_999995"), setLibPaths = FALSE)
  withr::local_envvar(SPADES_PROJECT_LIB_SNAPSHOT = file.path(dirname(newd), "lib_unrelated"))   # this session's snapshot dir
  removed <- sweepLibrarySnapshots()
  # other dead snapshots beside the session's library (made by earlier tests' workers) may go too
  expect_true(all(normalizePath(c(newd, oldd), mustWork = FALSE) %in% normalizePath(removed, mustWork = FALSE)))
  expect_false(dir.exists(newd)); expect_false(dir.exists(oldd))
})
