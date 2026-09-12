test_that(".errText makes a one-line, bounded summary of a condition", {
  e <- tryCatch(stop("lazy-load database '/x/LandR.rdb' is corrupt"), error = identity)
  txt <- .errText(e)
  expect_length(txt, 1L)
  expect_false(grepl("\n", txt, fixed = TRUE))
  expect_match(txt, "lazy-load database", fixed = TRUE)

  ## multi-line messages collapse rather than spilling across spreadsheet rows
  e2 <- simpleError("first line\nsecond line\n\n  third")
  expect_false(grepl("\n", .errText(e2), fixed = TRUE))
  expect_match(.errText(e2), "first line second line third", fixed = TRUE)

  ## and a runaway message is truncated, because this lands in one cell
  e3 <- simpleError(strrep("x", 5000))
  expect_lte(nchar(.errText(e3)), 300L)

  expect_true(is.na(.errText(simpleError(""))))
})

test_that(".mirror_local_queue records a failure reason, adding the column if the queue predates it", {
  skip_if_not_installed("filelock")
  qp <- withr::local_tempfile(fileext = ".rds")

  ## a queue from before last_error existed
  q <- data.frame(.ELFind = c("4.1", "4.2"), status = c("RUNNING", "PENDING"),
                  claimed_by = c("host-1", NA), stringsAsFactors = FALSE)
  saveRDS(q, qp)

  ## the requeue path: status goes back to PENDING, and the reason travels with it.
  ## Before this, a failed job was indistinguishable from one never started -- which is
  ## how a fleet of workers killed by a corrupted library looked like a slow run.
  .mirror_local_queue(qp, 1L, list(status = "PENDING", claimed_by = NA_character_,
                                   last_error = "lazy-load database is corrupt"))
  got <- readRDS(qp)
  expect_true("last_error" %in% names(got))
  expect_equal(got$status[1L], "PENDING")
  expect_equal(got$last_error[1L], "lazy-load database is corrupt")
  expect_true(is.na(got$last_error[2L]))

  ## a data column that does not exist is still refused: inventing one would change
  ## what the run means.
  .mirror_local_queue(qp, 1L, list(.someScenarioColumn = "invented"))
  expect_false(".someScenarioColumn" %in% names(readRDS(qp)))
})
