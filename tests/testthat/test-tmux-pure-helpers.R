## Pure / filesystem-only helpers from R/tmux.R. No tmux server, no ssh:
## get_latest_heartbeat() reads a directory, and .tmux_attach_ps_stats() parses
## pane titles before any remote dispatch.

# --- get_latest_heartbeat -----------------------------------------------------

mkHistDir <- function(files) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  for (f in files) writeLines("x", file.path(d, f))
  d
}

test_that("get_latest_heartbeat returns all-NA when no folder is configured", {
  res <- get_latest_heartbeat("run1", folderWithIterInFilename = NULL)

  expect_named(res, c("ts", "iter", "started", "elapsed"))
  expect_true(all(vapply(res, function(x) all(is.na(x)), logical(1))))
})

test_that("get_latest_heartbeat returns early for a zero-length folder", {
  res <- get_latest_heartbeat("run1", folderWithIterInFilename = character(0))

  expect_named(res, c("ts", "iter"))
  expect_true(is.na(res$ts))
  expect_true(is.na(res$iter))
})

test_that("get_latest_heartbeat returns early when the folder does not exist", {
  gone <- file.path(withr::local_tempdir(), "nope")

  res <- get_latest_heartbeat("run1", folderWithIterInFilename = gone)

  expect_true(is.na(res$ts))
  expect_true(is.na(res$iter))
})

test_that("get_latest_heartbeat returns early when no file mentions iter", {
  d <- mkHistDir(c("summary.png", "other.txt"))

  res <- get_latest_heartbeat("run1", folderWithIterInFilename = d)

  expect_true(is.na(res$ts))
  expect_true(is.na(res$iter))
})

test_that("get_latest_heartbeat reports the newest iteration and timestamp", {
  d <- mkHistDir(c("hist_iter001_2026-01-01 10:00:00.png",
                   "hist_iter002_2026-01-01 10:05:00.png",
                   "hist_iter003_2026-01-01 10:09:00.png"))

  res <- get_latest_heartbeat("run1", folderWithIterInFilename = d)

  expect_named(res, c("ts", "iter", "started", "elapsed"))
  expect_identical(res$ts, "2026-01-01 10:09:00")
  expect_identical(res$iter, 3L)
  # `started` is the earliest timestamp seen
  expect_identical(res$started, "2026-01-01 10:00:00")
})

test_that("get_latest_heartbeat accepts a quoted folder expression", {
  d <- mkHistDir("hist_iter007_2026-02-02 08:00:00.png")

  # the default is a call, so a call must be evaluated rather than used as-is
  res <- get_latest_heartbeat("run1", folderWithIterInFilename = bquote(.(d)))

  expect_identical(res$iter, 7L)
})

# --- .tmux_attach_ps_stats ----------------------------------------------------

test_that(".tmux_attach_ps_stats adds its columns to an empty pane table", {
  panes <- data.frame(title = character(0), stringsAsFactors = FALSE)

  res <- SpaDES.project:::.tmux_attach_ps_stats(panes)

  expect_identical(nrow(res), 0L)
  expect_true(all(c("state", "cpuAvg", "RAM (GB)", "availableCores",
                    "total RAM (GB)") %in% names(res)))
})

test_that(".tmux_attach_ps_stats leaves unparseable titles as NA", {
  panes <- data.frame(title = c("not-a-pane-title", "alsoNope"),
                      stringsAsFactors = FALSE)

  res <- SpaDES.project:::.tmux_attach_ps_stats(panes)

  expect_identical(nrow(res), 2L)
  expect_true(all(is.na(res$state)))
  expect_true(all(is.na(res$cpuAvg)))
})

test_that(".tmux_attach_ps_stats requires a 6+ digit pid to parse a title", {
  # the pid anchor is [0-9]{6,}; a short number must not be taken as a pid
  panes <- data.frame(title = c("host-node-123", "host-node-123456"),
                      stringsAsFactors = FALSE)

  res <- SpaDES.project:::.tmux_attach_ps_stats(panes)

  expect_identical(nrow(res), 2L)
  # the short one cannot be dispatched, so its stats stay NA
  expect_true(is.na(res$state[[1]]))
})
