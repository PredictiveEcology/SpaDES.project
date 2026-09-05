## experimentTmux() uses an existing queue in preference to `df`. That rule is
## correct -- it is what lets a resume keep its DONE/RUNNING rows -- but it used
## to be applied silently, so a rebuilt `df` could be discarded with nothing to
## distinguish that from success.

qFile <- function() file.path(tempdir(), paste0("q", sample(1e6, 1), ".rds"))

test_that("no existing queue: the queue is built from df", {
  f <- qFile(); on.exit(unlink(f))
  df <- data.frame(.ELFind = c("3.1.2", "7.1"), .rep = 1L)

  expect_silent(tmuxReconcileQueueWithDF(df, f))
  q <- readRDS(f)
  expect_equal(NROW(q), 2L)
  expect_true(all(q$status == "PENDING"))
})

test_that("resume with df fully covered: no warning, existing state preserved", {
  f <- qFile(); on.exit(unlink(f))
  df <- data.frame(.ELFind = c("3.1.2", "7.1"), .rep = 1L)
  tmuxReconcileQueueWithDF(df, f)
  q <- readRDS(f); q$status[1] <- "DONE"; saveRDS(q, f)

  expect_no_warning(expect_message(tmuxReconcileQueueWithDF(df, f), "already covers"))
  expect_equal(readRDS(f)$status, c("DONE", "PENDING"))
})

test_that("resume with new rows in df WARNS and names the options", {
  f <- qFile(); on.exit(unlink(f))
  tmuxReconcileQueueWithDF(data.frame(.ELFind = "3.1.2", .rep = 1L), f)
  df <- data.frame(.ELFind = c("3.1.2", "7.1", "8.1"), .rep = 1L)

  w <- capture_warnings(tmuxReconcileQueueWithDF(df, f))
  expect_length(w, 1L)
  expect_match(w, "2 are not in the queue")
  expect_match(w, "7.1")
  expect_match(w, "onExistingQueue = 'append'")
  expect_match(w, "onExistingQueue = 'rebuild'")

  ## and it really did leave the queue alone
  expect_equal(NROW(readRDS(f)), 1L)
})

test_that("append adds only the new rows and keeps existing status", {
  f <- qFile(); on.exit(unlink(f))
  tmuxReconcileQueueWithDF(data.frame(.ELFind = "3.1.2", .rep = 1L), f)
  q <- readRDS(f); q$status <- "DONE"; saveRDS(q, f)

  df <- data.frame(.ELFind = c("3.1.2", "7.1", "8.1"), .rep = 1L)
  expect_message(tmuxReconcileQueueWithDF(df, f, onExistingQueue = "append"),
                 "Appended 2 row")
  q2 <- readRDS(f)
  expect_equal(NROW(q2), 3L)
  expect_equal(q2$status[q2$.ELFind == "3.1.2"], "DONE")      # untouched
  expect_true(all(q2$status[q2$.ELFind %in% c("7.1", "8.1")] == "PENDING"))
})

test_that("rebuild discards the existing queue", {
  f <- qFile(); on.exit(unlink(f))
  tmuxReconcileQueueWithDF(data.frame(.ELFind = c("3.1.2", "9.9"), .rep = 1L), f)
  q <- readRDS(f); q$status <- "DONE"; saveRDS(q, f)

  df <- data.frame(.ELFind = "7.1", .rep = 1L)
  expect_message(tmuxReconcileQueueWithDF(df, f, onExistingQueue = "rebuild"), "rebuild")
  q2 <- readRDS(f)
  expect_equal(q2$.ELFind, "7.1")
  expect_equal(q2$status, "PENDING")
})

test_that("list columns are payload, not identity", {
  ## .modules/.times ride along as list columns; two rows differing only in
  ## those are the same scenario and must not be treated as new.
  f <- qFile(); on.exit(unlink(f))
  df1 <- data.frame(.ELFind = "3.1.2", .rep = 1L)
  df1$.modules <- I(list(c("a", "b")))
  tmuxReconcileQueueWithDF(df1, f)

  df2 <- data.frame(.ELFind = "3.1.2", .rep = 1L)
  df2$.modules <- I(list(c("a", "b", "c")))     # different payload, same scenario
  expect_no_warning(expect_message(tmuxReconcileQueueWithDF(df2, f), "already covers"))
  expect_equal(NROW(readRDS(f)), 1L)
})

test_that("a queue sharing no scenario columns with df warns rather than guessing", {
  f <- qFile(); on.exit(unlink(f))
  tmuxReconcileQueueWithDF(data.frame(.scenario = "A"), f)
  expect_warning(tmuxReconcileQueueWithDF(data.frame(.ELFind = "3.1.2"), f),
                 "shares no scenario columns")
})
