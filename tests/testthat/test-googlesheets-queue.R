## Google Sheets queue helpers. The Sheets API calls are mocked
## (local_mocked_bindings on googlesheets4), so these run offline and without a
## Drive credential -- what is under test is the range/column arithmetic and the
## cell-protection logic, not the transport.

test_that(".gs_read_queue returns an all-character data.frame", {
  skip_if_not_installed("googlesheets4")
  skip_if_not_installed("reproducible")

  # a tibble-shaped return, built without depending on tibble itself
  fakeTibble <- structure(
    list(job = c("a", "b"), status = c("PENDING", "RUNNING")),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = 1:2
  )
  testthat::local_mocked_bindings(
    read_sheet = function(ss, sheet, col_types, ...) fakeTibble,
    .package = "googlesheets4"
  )

  q <- SpaDES.project:::.gs_read_queue("fake-id")

  expect_s3_class(q, "data.frame")
  expect_false(inherits(q, "tbl_df"))
  expect_identical(q$status, c("PENDING", "RUNNING"))
})

# Capture what .gs_write_cells would send to the Sheets API.
withCapturedWrite <- function(code) {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    range_write = function(ss, data, range, sheet, col_names, ...) {
      captured$data  <- data
      captured$range <- range
      invisible(NULL)
    },
    range_read = function(...) stop("range_read should not be called"),
    cell_limits = function(ul, lr, ...) list(ul = ul, lr = lr),
    .package = "googlesheets4",
    .env = parent.frame()
  )
  force(code)
  captured
}

test_that(".gs_write_cells writes only the bounding range of the updated columns", {
  skip_if_not_installed("googlesheets4")

  cols <- c(job = 1L, status = 2L, host = 3L, pid = 4L, note = 5L)

  cap <- withCapturedWrite(
    SpaDES.project:::.gs_write_cells(
      ss_id       = "fake-id",
      sheet_row   = 7L,
      updates     = list(status = "RUNNING", pid = "123"),
      col_positions = cols,
      current_row = c(job = "j1", status = "PENDING", host = "mega",
                      pid = "", note = "n")
    )
  )

  # status(2) .. pid(4) -> three cells, starting at row 7 col 2
  expect_identical(cap$range$ul, c(7L, 2L))
  expect_identical(cap$range$lr, c(7L, 4L))
  expect_identical(ncol(cap$data), 3L)
})

test_that(".gs_write_cells preserves untouched cells inside the range", {
  skip_if_not_installed("googlesheets4")

  cols <- c(job = 1L, status = 2L, host = 3L, pid = 4L)

  cap <- withCapturedWrite(
    SpaDES.project:::.gs_write_cells(
      ss_id       = "fake-id",
      sheet_row   = 2L,
      updates     = list(status = "DONE", pid = "9"),
      col_positions = cols,
      current_row = c(job = "j1", status = "RUNNING", host = "keepme", pid = "1")
    )
  )

  vals <- unlist(cap$data, use.names = FALSE)
  # host sits between the two updated columns and must survive untouched
  expect_identical(vals, c("DONE", "keepme", "9"))
})

test_that(".gs_write_cells ignores updates for unknown columns", {
  skip_if_not_installed("googlesheets4")

  cols <- c(job = 1L, status = 2L)

  cap <- withCapturedWrite(
    SpaDES.project:::.gs_write_cells(
      ss_id       = "fake-id",
      sheet_row   = 3L,
      updates     = list(status = "DONE", nonexistent = "x"),
      col_positions = cols,
      current_row = c(job = "j1", status = "PENDING")
    )
  )

  # only `status` is known -> a single-cell range at column 2
  expect_identical(cap$range$ul, c(3L, 2L))
  expect_identical(cap$range$lr, c(3L, 2L))
  expect_identical(unlist(cap$data, use.names = FALSE), "DONE")
})

test_that(".gs_write_cells is a no-op when no update names are known", {
  skip_if_not_installed("googlesheets4")

  testthat::local_mocked_bindings(
    range_write = function(...) stop("must not write"),
    .package = "googlesheets4"
  )

  expect_null(
    SpaDES.project:::.gs_write_cells(
      ss_id = "fake-id", sheet_row = 1L,
      updates = list(nope = "x"),
      col_positions = c(job = 1L, status = 2L),
      current_row = c(job = "a", status = "b")
    )
  )
})

test_that(".gs_write_cells reads the bounding range when no current_row is given", {
  skip_if_not_installed("googlesheets4")

  read_calls <- 0L
  testthat::local_mocked_bindings(
    cell_limits = function(ul, lr, ...) list(ul = ul, lr = lr),
    range_read = function(ss, sheet, range, col_names, col_types, ...) {
      read_calls <<- read_calls + 1L
      data.frame(a = "old1", b = "old2", stringsAsFactors = FALSE)
    },
    range_write = function(ss, data, range, sheet, col_names, ...) invisible(NULL),
    .package = "googlesheets4"
  )

  SpaDES.project:::.gs_write_cells(
    ss_id = "fake-id", sheet_row = 4L,
    updates = list(status = "DONE"),
    col_positions = c(job = 1L, status = 2L)
  )

  expect_identical(read_calls, 1L)
})
