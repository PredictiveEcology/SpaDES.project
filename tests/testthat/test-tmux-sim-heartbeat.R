## get_sim_year_heartbeat() from R/tmux.R: infer run progress from the
## `*_year<NNNN>.*` files an experiment leaves in its output directory.
## Filesystem only -- no tmux, no simulation.

mkYearFiles <- function(years, prefix = "cohortData", ext = "rds",
                        mtimes = NULL, envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  paths <- file.path(d, sprintf("%s_year%04d.%s", prefix, years, ext))
  for (p in paths) saveRDS(1, p)
  if (!is.null(mtimes)) Map(function(p, m) Sys.setFileTime(p, m), paths, mtimes)
  d
}

test_that("get_sim_year_heartbeat returns all-NA for a missing directory", {
  res <- get_sim_year_heartbeat(file.path(withr::local_tempdir(), "nope"))

  expect_named(res, c("ts", "iter", "started", "elapsed", "pct_complete"))
  expect_true(is.na(res$ts))
  expect_true(is.na(res$iter))
})

test_that("get_sim_year_heartbeat returns all-NA when nothing matches", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "unrelated.txt"))

  res <- get_sim_year_heartbeat(d)

  expect_true(is.na(res$ts))
  expect_true(is.na(res$iter))
})

test_that("get_sim_year_heartbeat reports the highest year reached", {
  d <- mkYearFiles(c(2011, 2013, 2012))

  res <- get_sim_year_heartbeat(d)

  # files are ordered by year, not by name, so 2013 is the latest
  expect_identical(res$iter, 2013L)
})

test_that("get_sim_year_heartbeat falls back to any _year file", {
  d <- mkYearFiles(c(2001, 2002), prefix = "somethingElse")

  # nothing matches the default cohortData prefix, so the generic pattern is used
  res <- get_sim_year_heartbeat(d)

  expect_identical(res$iter, 2002L)
})

test_that("get_sim_year_heartbeat prefers the named prefix over others", {
  d <- mkYearFiles(c(2001, 2002), prefix = "cohortData")
  saveRDS(1, file.path(d, "other_year2050.rds"))

  res <- get_sim_year_heartbeat(d, file_prefix = "cohortData")

  # the decoy year is higher, but belongs to a different prefix
  expect_identical(res$iter, 2002L)
})

test_that("get_sim_year_heartbeat computes percent complete from the year range", {
  d <- mkYearFiles(c(2000, 2005))

  res <- get_sim_year_heartbeat(d, start_year = 2000, end_year = 2010)

  # reached 2005 of 2000..2010
  expect_equal(res$pct_complete, 50)
})

test_that("get_sim_year_heartbeat gives NA percent when the range is degenerate", {
  d <- mkYearFiles(2000)

  res <- get_sim_year_heartbeat(d, start_year = 2000, end_year = 2000)

  expect_true(is.na(res$pct_complete))
})

test_that("get_sim_year_heartbeat takes started from the earliest mtime", {
  now <- Sys.time()
  # Restart scenario the source comments describe: the LOWEST year is rewritten
  # most recently, so its mtime is not the run start. Here 2001 is the genuinely
  # oldest file, and 2002 (the highest year) is the latest state.
  d <- mkYearFiles(c(2000, 2001, 2002),
                   mtimes = list(now, now - 3600, now - 1800))

  res <- get_sim_year_heartbeat(d)

  expect_identical(res$iter, 2002L)
  # started follows mtime order, so it is 2001's time, not 2000's
  expect_identical(res$started, format(now - 3600, "%Y-%m-%d %H:%M:%S"))
  expect_identical(res$ts, format(now - 1800, "%Y-%m-%d %H:%M:%S"))
  # 1800s between the earliest file and the highest-year file
  expect_equal(as.numeric(res$elapsed, units = "secs"), 1800, tolerance = 2)
})
