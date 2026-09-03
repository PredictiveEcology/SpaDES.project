## reGetMember() pulls one member out of a remote tar by HTTP Range. The
## download needs Drive, so these mock the request and assert the parts that
## can silently go wrong: the byte range computed, and the two responses that
## would otherwise corrupt the result quietly.

.idx <- data.frame(
  name   = c("a/mod_lazy/_manifest.rds", "a/mod_lazy/0001-alpha.rds", "a/mod_lazy/0002-beta.rds"),
  offset = c(512, 4096, 20480),
  size   = c(100, 2048, 4096),
  stringsAsFactors = FALSE
)
.payloadFor <- function(obj) { f <- tempfile(); saveRDS(obj, f); readBin(f, "raw", file.size(f)) }

test_that("reGetMember requests exactly the member's byte range", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("httr")
  obj <- list(x = 1:3); payload <- .payloadFor(obj)
  idx <- .idx; idx$size[[2]] <- length(payload)
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(drive_get = function(...) data.frame(id = "FAKEID", name = "run_lazy.tar.gz"),
                        drive_token = function(...) NULL, .package = "googledrive")
  ## force `...`: add_headers() is an ARGUMENT to GET(), so a mock that ignores
  ## `...` never evaluates it and the Range is never recorded
  local_mocked_bindings(GET = function(url, ...) { seen$url <- url; force(list(...)); "resp" },
                        add_headers = function(...) { seen$range <- list(...)$Range; NULL },
                        status_code = function(...) 206L,
                        content = function(...) payload, .package = "httr")

  got <- reGetMember("FAKEID", "0001-alpha", index = idx)
  ## inclusive range: offset .. offset + size - 1
  expect_identical(seen$range, sprintf("bytes=%s-%s", 4096, 4096 + length(payload) - 1))
  expect_true(grepl("FAKEID", seen$url, fixed = TRUE))
  expect_identical(got, obj)                     # round-trips through readRDS
})

test_that("reGetMember rejects HTTP 200 -- the server ignoring the Range", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("httr")
  ## 200 means the whole file is coming back; accepting it would be silently wrong
  local_mocked_bindings(drive_get = function(...) data.frame(id = "FAKEID", name = "r.tar.gz"),
                        drive_token = function(...) NULL, .package = "googledrive")
  local_mocked_bindings(GET = function(...) "resp", add_headers = function(...) NULL,
                        status_code = function(...) 200L, content = function(...) raw(10),
                        .package = "httr")
  expect_error(reGetMember("FAKEID", "0001-alpha", index = .idx), "ignored the Range")
})

test_that("reGetMember rejects a short read", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("httr")
  local_mocked_bindings(drive_get = function(...) data.frame(id = "FAKEID", name = "r.tar.gz"),
                        drive_token = function(...) NULL, .package = "googledrive")
  local_mocked_bindings(GET = function(...) "resp", add_headers = function(...) NULL,
                        status_code = function(...) 206L, content = function(...) raw(5),
                        .package = "httr")
  expect_error(reGetMember("FAKEID", "0001-alpha", index = .idx), "Short read")
})

test_that("reGetMember matches exactly first, and refuses to guess", {
  skip_if_not_installed("googledrive"); skip_if_not_installed("httr")
  obj <- 1:5; payload <- .payloadFor(obj)
  idx <- .idx; idx$size[[3]] <- length(payload)
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(drive_get = function(...) data.frame(id = "FAKEID", name = "r.tar.gz"),
                        drive_token = function(...) NULL, .package = "googledrive")
  local_mocked_bindings(GET = function(url, ...) { force(list(...)); "resp" },
                        add_headers = function(...) { seen$range <- list(...)$Range; NULL },
                        status_code = function(...) 206L, content = function(...) payload,
                        .package = "httr")

  expect_identical(reGetMember("FAKEID", "a/mod_lazy/0002-beta.rds", index = idx), obj)
  expect_identical(seen$range, sprintf("bytes=%s-%s", 20480, 20480 + length(payload) - 1))
  expect_error(reGetMember("FAKEID", "nope", index = .idx), "No archive member matching")
  expect_error(reGetMember("FAKEID", "\\.rds$", index = .idx), "matches 3 members")
})

## reGetUntarLoad(remote = TRUE): objects stay on the remote, output files come
## down. The archive itself is never downloaded.

test_that("remote = TRUE requires loadSimList", {
  expect_error(reGetUntarLoad("id", destDir = tempdir(), remote = TRUE, method = "readRDS"),
               "requires method")
})

test_that("the sidecar dir is derived from the shell, not matched loosely", {
  ## The shell's PARENT directory is often itself named `<rep>_lazy`, so a
  ## pattern like "[^/]+_lazy/" misclassifies the shell as its own sidecar --
  ## which showed up as `paths()` being handed a data.table.
  nm <- c("out/rep3_lazy/run_3020.rds",             # the shell
          "out/rep3_lazy/run_3020_lazy/_manifest.rds",
          "out/rep3_lazy/run_3020_lazy/0001-a.rds",
          "out/rep3_lazy/cohortData_year2020.rds")  # an output that is also .rds
  lazyDirName <- paste0(tools::file_path_sans_ext(basename(nm[1])), "_lazy")
  isSide <- grepl(paste0("(^|/)", lazyDirName, "/"), nm)
  expect_identical(isSide, c(FALSE, TRUE, TRUE, FALSE))
  expect_false(isSide[1])                       # shell is not a sidecar
  expect_false(isSide[4])                       # nor is an output .rds
  ## the loose pattern gets it wrong, which is why it is not used
  expect_true(all(grepl("(^|/)[^/]+_lazy/", nm)))
})

test_that(".localPathFor applies the same prefix remap as reUntar", {
  expect_identical(SpaDES.project:::.localPathFor("home/a/b/x.rds", NULL), "/home/a/b/x.rds")
  expect_identical(
    SpaDES.project:::.localPathFor("home/a/b/x.rds", c(old = "/home/a", new = "/tmp/z")),
    "/tmp/z/b/x.rds")
})
