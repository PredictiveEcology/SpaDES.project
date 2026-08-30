## File-descriptor diagnostics. .classifyFds() is pure string classification;
## openFds()/openFdsReport() read /proc/self/fd and degrade to empty/"" off Linux.

test_that("fdSelectLimit is the select() ceiling", {
  expect_identical(fdSelectLimit(), 1024L)
})

test_that(".classifyFds buckets every holder type", {
  cf <- SpaDES.project:::.classifyFds

  expect_identical(cf("socket:[12345]"), "socket")
  expect_identical(cf("pipe:[678]"), "pipe")
  expect_identical(cf("anon_inode:[eventpoll]"), "anon_inode")
  expect_identical(cf("/tmp/RtmpXX/terra/spat_abc123.tif"), "terra scratch")
  expect_identical(cf("/data/elevation.TIF"), "tif (other)")
  expect_identical(cf("/data/mosaic.vrt"), "vrt")
  expect_identical(cf("/var/cache/cache.sqlite"), "sqlite")
  expect_identical(cf("/var/cache/cache.sqlite-wal"), "sqlite")
  expect_identical(cf("/var/cache/store.db-shm"), "sqlite")
  expect_identical(cf("/tmp/obj.qs"), "qs/qs2")
  expect_identical(cf("/tmp/obj.qs2"), "qs/qs2")
  expect_identical(cf("/etc/hosts"), "other file")
  expect_identical(cf("something-relative"), "unknown")
})

test_that(".classifyFds distinguishes deleted terra scratch files", {
  cf <- SpaDES.project:::.classifyFds

  expect_identical(cf("/tmp/RtmpXX/terra/spat_abc.tif (deleted)"),
                   "terra scratch (deleted)")
  # the suffix is stripped before matching, so other buckets still resolve
  expect_identical(cf("/data/x.tif (deleted)"), "tif (other)")
})

test_that(".classifyFds is vectorised and order-preserving", {
  cf <- SpaDES.project:::.classifyFds

  expect_identical(
    cf(c("socket:[1]", "/tmp/a.vrt", "pipe:[2]")),
    c("socket", "vrt", "pipe")
  )
  expect_identical(cf(character(0)), character(0))
})

test_that("openFds reports this process's descriptors on Linux", {
  skip_if_not(dir.exists("/proc/self/fd"), "no /proc/self/fd")

  df <- openFds()

  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("fd", "target", "bucket"))
  expect_type(df$fd, "integer")
  # a live R process always holds at least stdin/stdout/stderr
  expect_gt(nrow(df), 0L)
  expect_false(is.unsorted(df$fd))
})

test_that("openFdsReport summarises at threshold 0 and stays quiet above the limit", {
  skip_if_not(dir.exists("/proc/self/fd"), "no /proc/self/fd")

  full <- openFdsReport(threshold = 0L)
  expect_match(full, "^Open fds: [0-9]+ total")
  expect_match(full, "High-fd holders")

  # nothing is open above an absurd threshold -> summary only, no holder table
  quiet <- openFdsReport(threshold = .Machine$integer.max)
  expect_match(quiet, "0 at or above threshold")
  expect_false(grepl("High-fd holders", quiet))
})
