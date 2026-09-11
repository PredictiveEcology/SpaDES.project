## The Google Sheet stores every queue value as text. What comes back must have the
## queue's own types: an ELF named "14.3" or "4.10" is a name, not the number 14.3 or 4.1.

# What .gs_push_queue() writes: every column as character, `.col` as `dotcol`.
sheetText <- function(q) {
  s <- as.data.frame(lapply(q, as.character), stringsAsFactors = FALSE)
  names(s) <- gsub("^\\.", "dot", names(s))
  s
}

test_that(".gs_restore_types gives the sheet's text back the queue's column types", {
  q <- data.frame(.ELFind = c("14.3", "4.10", "5.3.1"), .rep = c(1L, 2L, 3L),
                  .useX = c(TRUE, FALSE, NA), status = "PENDING", stringsAsFactors = FALSE)
  q$.modules <- list(c("a", "b"), "c", c("d", "e", "f"))
  q$.times <- list(list(start = 2011, end = 2020), list(start = 1, end = 2),
                   list(start = 3, end = 4))

  out <- SpaDES.project:::.gs_restore_types(sheetText(q), template = q)

  expect_identical(out$dotELFind, c("14.3", "4.10", "5.3.1"))
  expect_identical(out$dotrep, c(1L, 2L, 3L))
  expect_identical(out$dotuseX, c(TRUE, FALSE, NA))
  expect_identical(out$dotmodules, q$.modules)
  expect_identical(out$dottimes, q$.times)
  expect_identical(out$status, q$status)
})

test_that(".gs_restore_types without a template keeps text and rebuilds only constructors", {
  s <- data.frame(dotELFind = c("14.3", "4.10"), dotrange = c("1991:2020", "2001:2010"),
                  dotexpr = c("10-1", "Sys.time()"), stringsAsFactors = FALSE)

  out <- SpaDES.project:::.gs_restore_types(s)

  expect_identical(out$dotELFind, c("14.3", "4.10"))
  expect_identical(out$dotrange, list(1991:2020, 2001:2010))
  expect_identical(out$dotexpr, c("10-1", "Sys.time()"))
})
