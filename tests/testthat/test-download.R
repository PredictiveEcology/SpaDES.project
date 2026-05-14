## Tests for the re** family in R/download.R
## Covers: reUntar (incl. pathRemap), reLoad. reGet / reGetUntarLoad
## require Google Drive auth and are exercised only via integration use.

# Helper: build a fake outTar-style archive — single .rds at an absolute
# path inside the archive, so reUntar's `entries[[1L]]` logic applies.
.makeFakeTar <- function(payload, srcDir) {
  rds <- file.path(srcDir, "fake_sim.rds")
  saveRDS(payload, rds)
  tarball <- file.path(srcDir, "fake_sim.tar.gz")
  withr::with_dir(srcDir, {
    utils::tar(tarball, files = rds, compression = "gzip")
  })
  tarball
}

# reUntar uses GNU tar's --absolute-names / --transform; BSD tar (macOS)
# rejects those flags, so the tarball-extraction tests are GNU-tar only.
.skip_if_no_gnu_tar <- function() {
  tar_bin <- Sys.getenv("TAR", unset = "tar")
  v <- tryCatch(suppressWarnings(system2(tar_bin, "--version", stdout = TRUE,
                                         stderr = FALSE)),
                error = function(e) character())
  if (!any(grepl("GNU tar", v, fixed = TRUE)))
    testthat::skip("GNU tar not available (reUntar uses GNU-only flags)")
}

# ---------------------------------------------------------------------------
# reUntar
# ---------------------------------------------------------------------------

test_that("reUntar restores absolute paths and returns the sim file path", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(answer = 42L, name = "fake_sim")
  tarball <- .makeFakeTar(payload, src)
  rdsAbs  <- file.path(src, "fake_sim.rds")

  unlink(rdsAbs)  # force reUntar to recreate it
  expect_false(file.exists(rdsAbs))

  expect_message(out <- reUntar(tarball), "untarred fake_sim.tar.gz")

  expect_equal(out, rdsAbs)
  expect_true(file.exists(rdsAbs))
  expect_equal(readRDS(rdsAbs), payload)
})

test_that("reUntar with pathRemap rewrites the prefix", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(x = 1L)
  tarball <- .makeFakeTar(payload, src)

  newPrefix <- withr::local_tempdir()
  remap <- c(old = src, new = newPrefix)

  out <- reUntar(tarball, pathRemap = remap)

  expected <- file.path(newPrefix, "fake_sim.rds")
  expect_equal(out, expected)
  expect_true(file.exists(expected))
  expect_equal(readRDS(expected), payload)
})

test_that("reUntar with pathRemap expands ~ in new prefix", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  payload <- list(x = 2L)
  tarball <- .makeFakeTar(payload, src)

  # Stage a fake home so ~ expands somewhere we control and clean up
  fakeHome <- withr::local_tempdir()
  withr::local_envvar(HOME = fakeHome)
  remap <- c(old = src, new = "~/sub/dir")

  out <- reUntar(tarball, pathRemap = remap)

  expected <- file.path(fakeHome, "sub/dir", "fake_sim.rds")
  expect_equal(out, expected)
  expect_true(file.exists(expected))
  expect_equal(readRDS(expected), payload)
})

test_that("reUntar rejects malformed pathRemap", {
  src <- withr::local_tempdir()
  tarball <- .makeFakeTar(list(), src)

  expect_error(reUntar(tarball, pathRemap = c("a", "b")),
               "named character vector")
  expect_error(reUntar(tarball, pathRemap = c(foo = "a", bar = "b")),
               "named character vector")
})

test_that("reUntar is vectorised over tarballs", {
  .skip_if_no_gnu_tar()
  src <- withr::local_tempdir()
  t1 <- .makeFakeTar(list(i = 1L), src)
  src2 <- withr::local_tempdir()
  t2 <- .makeFakeTar(list(i = 2L), src2)

  out <- reUntar(c(t1, t2))
  expect_length(out, 2L)
  expect_true(all(file.exists(out)))
})

# ---------------------------------------------------------------------------
# reLoad
# ---------------------------------------------------------------------------

test_that("reLoad with method='readRDS' returns a named list", {
  src <- withr::local_tempdir()
  f1 <- file.path(src, "a.rds"); saveRDS(list(v = 1L), f1)
  f2 <- file.path(src, "b.rds"); saveRDS(list(v = 2L), f2)

  expect_message(
    out <- reLoad(c(f1, f2), method = "readRDS"),
    "loaded a.rds via readRDS"
  )

  expect_named(out, c("a.rds", "b.rds"))
  expect_equal(out$a.rds$v, 1L)
  expect_equal(out$b.rds$v, 2L)
})

test_that("reLoad errors clearly on missing files", {
  expect_error(reLoad("/no/such/file.rds", method = "readRDS"))
})

test_that("reLoad rejects unknown method", {
  src <- withr::local_tempdir()
  f <- file.path(src, "x.rds"); saveRDS(1L, f)
  expect_error(reLoad(f, method = "bogus"))
})
