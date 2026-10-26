## Queue .rds files are rewritten by many workers while others read them. A reader that
## saw a half-written file used to get NULL as the type template, and the sheet's text
## ("2016") reached the job instead of the integer.

mkBigQueue <- function(n = 2000L) {
  data.frame(.hindcastYear = seq_len(n), .ELFind = as.character(seq_len(n)),
             status = "PENDING", stringsAsFactors = FALSE)
}

test_that(".gs_restore_types errors, rather than passing text through, with no template", {
  s <- data.frame(dothindcastYear = "2016", stringsAsFactors = FALSE)
  expect_error(SpaDES.project:::.gs_restore_types(s, NULL), "template")
  expect_error(SpaDES.project:::.gs_restore_types(s), "template|missing")
})

test_that(".queue_write leaves no partial file and no temp file behind", {
  d <- withr::local_tempdir()
  qp <- file.path(d, "queue.rds")
  q <- mkBigQueue()
  SpaDES.project:::.queue_write(q, qp)
  expect_identical(readRDS(qp), q)
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "queue.rds")
  ## a concurrent reader never sees a truncated file
  skip_if_not_installed("callr")
  go <- file.path(d, "go"); stop_f <- file.path(d, "stop")
  bg <- callr::r_bg(function(qp, q, stop_f) {
    while (!file.exists(stop_f)) SpaDES.project:::.queue_write(q, qp)
  }, args = list(qp, q, stop_f), package = FALSE)
  on.exit(bg$kill(), add = TRUE)
  bad <- 0L
  for (i in 1:300) if (inherits(try(readRDS(qp), silent = TRUE), "try-error")) bad <- bad + 1L
  file.create(stop_f)
  expect_identical(bad, 0L)
})

test_that("a locked, retrying reader never yields character types while the queue is rewritten", {
  skip_if_not_installed("callr")
  skip_if_not_installed("filelock")
  d <- withr::local_tempdir()
  qp <- file.path(d, "queue.rds")
  q <- mkBigQueue()
  saveRDS(q, qp)
  stop_f <- file.path(d, "stop")
  bg <- callr::r_bg(function(qp, q, stop_f) {
    while (!file.exists(stop_f)) SpaDES.project:::.mirror_local_queue(qp, 1L, list(status = "RUNNING"))
  }, args = list(qp, q, stop_f), package = FALSE)
  on.exit(bg$kill(), add = TRUE)
  sheet <- as.data.frame(lapply(q, as.character), stringsAsFactors = FALSE)
  names(sheet) <- gsub("^\\.", "dot", names(sheet))
  for (i in 1:100) {
    out <- SpaDES.project:::.gs_restore_types(sheet, SpaDES.project:::.queue_read(qp))
    expect_true(is.integer(out$dothindcastYear))
  }
  file.create(stop_f)
})

test_that(".queue_read stops with an informative error when the file cannot be read", {
  skip_if_not_installed("filelock")
  d <- withr::local_tempdir()
  qp <- file.path(d, "queue.rds")
  writeBin(as.raw(1:10), qp)
  expect_error(SpaDES.project:::.queue_read(qp, tries = 2L, wait = 0), "queue")
})
