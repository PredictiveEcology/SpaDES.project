test_that(".pidIsAlive treats a zombie as dead, and parses comm names containing ') '", {
  skip_on_cran()
  skip_if_not(dir.exists("/proc"), "no /proc filesystem")

  ## A zombie -- exited but not yet reaped -- still has a /proc entry, so the
  ## file.exists("/proc/<pid>") test this replaced reported it alive. A worker killed
  ## while its parent never waits for it (a tmux server, in the case this fixes) can stay
  ## a zombie indefinitely, and its queue row would never be reclaimed: that scenario is
  ## silently skipped for the rest of the run.
  ##
  ## R reaps its own forked children, so a zombie cannot be manufactured reliably in a
  ## test. The states are therefore driven through procRoot, which is what makes the
  ## parsing itself checkable -- including the case the naive "field 3" split gets wrong.
  root <- withr::local_tempdir()
  mk <- function(pid, comm, state) {
    d <- file.path(root, pid)
    dir.create(d, recursive = TRUE)
    writeLines(paste(pid, paste0("(", comm, ")"), state,
                     paste(rep(0L, 20), collapse = " ")),
               file.path(d, "stat"))
    pid
  }

  expect_false(.pidIsAlive(mk("101", "R", "Z"), procRoot = root))        # zombie
  expect_true(.pidIsAlive(mk("102", "R", "S"), procRoot = root))         # sleeping
  expect_true(.pidIsAlive(mk("103", "R", "R"), procRoot = root))         # running
  expect_true(.pidIsAlive(mk("104", "R", "D"), procRoot = root))         # uninterruptible

  ## comm can contain spaces and parentheses, so the state is the field after the LAST
  ## ")", not the third whitespace-separated field.
  expect_false(.pidIsAlive(mk("105", "my (odd) name", "Z"), procRoot = root))
  expect_true(.pidIsAlive(mk("106", "my (odd) name", "S"), procRoot = root))

  ## The /proc entry exists in every case above -- which is precisely why bare existence
  ## was not liveness.
  expect_true(file.exists(file.path(root, "101")))

  expect_false(.pidIsAlive("999999", procRoot = root))                   # no entry
  expect_true(.pidIsAlive(Sys.getpid()))                                 # the real thing
})
