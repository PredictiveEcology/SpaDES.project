# Regression tests for OSC2 pane title updates in tmuxRunNextWorker / tmuxRunWorkerLoop.
#
# Bug: commit 31e0555 changed the OSC2 guard from unconditional to
# nzchar(Sys.getenv("TMUX")).  TMUX is not set inside SSH sessions, so remote
# workers silently stopped updating their pane title after claiming a job.
# Fix (6248bd1): guard is now nzchar(Sys.getenv("TMUX")) || isatty(stdout()).

test_that("OSC2 guard includes isatty(stdout()) for SSH-session compatibility", {
  # Source-level assertion: catches anyone reverting the fix.  Reads from
  # the loaded function body so it works under R CMD check (where the R/
  # source tree is not exposed).  Both worker entry points must guard the
  # OSC2 escape with isatty(stdout()) for ssh -t pane-title updates.
  body_str <- function(fn)
    paste(deparse(body(fn)), collapse = "\n")

  next_body <- body_str(SpaDES.project::tmuxRunNextWorker)
  expect_true(
    grepl("isatty(stdout())", next_body, fixed = TRUE),
    label = "isatty(stdout()) present in tmuxRunNextWorker",
    info  = paste(
      "The OSC2 pane-title guard must include isatty(stdout()) so that",
      "remote workers running via ssh -t can update their pane title.",
      "TMUX env var alone is not set in SSH sessions."
    )
  )
})

test_that("tmuxRunNextWorker emits OSC2 pane title escape when TMUX is set", {
  skip_if_not_installed("filelock")

  td <- tempfile("pane_title")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  queue_path <- file.path(td, "queue.rds")
  q_init <- data.frame(
    outputPath     = "run1",
    status         = "PENDING",
    claimed_by     = NA_character_,
    started_at     = NA_character_,
    finished_at    = NA_character_,
    machine_name   = NA_character_,
    process_id     = NA_integer_,
    interrupted_at = NA_character_,
    stringsAsFactors = FALSE
  )
  saveRDS(q_init, queue_path)
  global_path <- file.path(td, "global.R")
  writeLines("invisible(NULL)", global_path)

  withr::with_envvar(c(TMUX = "mock,0,0", TMUX_PANE = ""), {
    out <- capture.output(suppressWarnings(
      tmuxRunNextWorker(
        queue_path        = queue_path,
        global_path       = global_path,
        activeRunningPath = td
      )
    ))
  })

  expect_true(
    any(grepl("\033]2;", out, fixed = TRUE)),
    info = "OSC2 title escape must be present in stdout when TMUX is set"
  )
})

test_that("tmuxRunNextWorker does NOT emit OSC2 when TMUX is unset and stdout is not a tty", {
  # This protects experimentFuture log files from escape-byte pollution.
  skip_if_not_installed("filelock")

  td <- tempfile("pane_title_notty")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  queue_path <- file.path(td, "queue.rds")
  q_init <- data.frame(
    outputPath     = "run1",
    status         = "PENDING",
    claimed_by     = NA_character_,
    started_at     = NA_character_,
    finished_at    = NA_character_,
    machine_name   = NA_character_,
    process_id     = NA_integer_,
    interrupted_at = NA_character_,
    stringsAsFactors = FALSE
  )
  saveRDS(q_init, queue_path)
  global_path <- file.path(td, "global.R")
  writeLines("invisible(NULL)", global_path)

  # In the test runner stdout is not a tty, so isatty(stdout()) == FALSE.
  # With TMUX unset the guard should be FALSE and no OSC2 emitted.
  withr::with_envvar(c(TMUX = "", TMUX_PANE = ""), {
    out <- capture.output(suppressWarnings(
      tmuxRunNextWorker(
        queue_path        = queue_path,
        global_path       = global_path,
        activeRunningPath = td
      )
    ))
  })

  expect_false(
    any(grepl("\033]2;", out, fixed = TRUE)),
    info = "OSC2 escape must not appear in captured (non-tty) output when TMUX is unset"
  )
})
