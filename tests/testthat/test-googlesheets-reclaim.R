## .gs_reclaim_dead_jobs() must not overwrite a claim that landed after it read the queue.
##
## It reads the queue, checks whether each RUNNING row's process is alive, and marks dead
## ones INTERRUPTED. When a whole fleet starts at once, every worker runs this before
## claiming, many from the same early read. One worker reclaims a dead row, a second
## worker claims it and starts the job, and a third -- still acting on its early read,
## where the row showed the old dead process -- then marks it INTERRUPTED. The row goes
## back into the queue while the second worker is still running it, so the next worker
## claims it too: two workers on one job. Seen on a 15-worker run (issue #169).
##
## Sheets calls are mocked, so this runs offline.

reclaimFixture <- function(pid, claimed_by = paste0("host-", pid), status = "RUNNING") {
  data.frame(
    .ELFind      = "6.3.1",
    status       = status,
    claimed_by   = claimed_by,
    machine_name = Sys.info()[["nodename"]],
    process_id   = as.character(pid),
    stringsAsFactors = FALSE
  )
}

## First read: what the reclaimer judged. Later reads: the queue as it is now.
mockReclaim <- function(firstRead, laterRead, env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  state$writes <- list()
  testthat::local_mocked_bindings(
    .gs_read_queue = function(ss_id, sheet = "Status") {
      state$reads <- state$reads + 1L
      if (state$reads == 1L) firstRead else laterRead
    },
    ## same formals as the real function: the caller passes `sheet =` by name, which
    ## would otherwise partially match `sheet_row`
    .gs_write_cells = function(ss_id, sheet_row, updates, col_positions,
                               sheet = "Status", current_row = NULL) {
      state$writes[[length(state$writes) + 1L]] <- list(row = sheet_row, updates = updates)
      invisible(NULL)
    },
    .tmux_all_pane_titles = function(...) character(0),
    .pidIsAlive = function(pid, ...) FALSE,  # the process on the first read is gone
    .env = env
  )
  state
}

test_that("a row claimed again after the check is not reclaimed", {
  ## judged: RUNNING under dead PID 111; by write time a live claim (PID 222) has landed
  state <- mockReclaim(reclaimFixture(111), reclaimFixture(222))

  suppressMessages(SpaDES.project:::.gs_reclaim_dead_jobs("fake-id"))

  expect_length(state$writes, 0L)
})

test_that("a row already reclaimed by another worker is left alone", {
  state <- mockReclaim(reclaimFixture(111),
                       reclaimFixture(111, claimed_by = NA_character_, status = "INTERRUPTED"))

  suppressMessages(SpaDES.project:::.gs_reclaim_dead_jobs("fake-id"))

  expect_length(state$writes, 0L)
})

test_that("a row still held by the dead process is reclaimed", {
  state <- mockReclaim(reclaimFixture(111), reclaimFixture(111))

  suppressMessages(SpaDES.project:::.gs_reclaim_dead_jobs("fake-id"))

  expect_length(state$writes, 1L)
  expect_identical(state$writes[[1]]$row, 2L)  # row 1 + header
  expect_identical(state$writes[[1]]$updates$status, "INTERRUPTED")
})
