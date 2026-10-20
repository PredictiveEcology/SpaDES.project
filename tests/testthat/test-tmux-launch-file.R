## experimentTmux() used to TYPE long commands into each pane with send-keys. Under load the keystrokes
## were garbled or lost (2026-10-19, 80-core machine): panes sat idle at the R prompt or got a corrupted
## command. Each pane now gets ONE short `bash <launcher>` command that runs a file, which an interactive
## user can also recall with up-arrow. Runs on a private tmux server (-L), never the default one.

test_that("experimentTmux types one short command per pane, which starts the same worker when re-sent", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_no_tmux() # as the other tmux tests
  skip_if_not_installed("processx")
  skip_if_not_installed("filelock")

  sock <- paste0("splaunch", Sys.getpid())
  tm <- function(...) processx::run("tmux", c("-L", sock, ...), error_on_status = FALSE)
  withr::defer(tm("kill-server"))
  expect_equal(tm("-f", "/dev/null", "new-session", "-d", "-x", "200", "-y", "60", "-s", "t", "sleep 600")$status, 0L)
  pane0 <- trimws(tm("display", "-p", "-t", "t", "#{pane_id}")$stdout)
  ## $TMUX / $TMUX_PANE make experimentTmux()'s tmux calls reach the private server
  withr::local_envvar(c(TMUX = trimws(tm("display", "-p", "-t", "t", "#{socket_path},#{pid},0")$stdout),
                        TMUX_PANE = pane0))

  td <- withr::local_tempdir()
  outdir <- file.path(td, "out"); dir.create(outdir)
  global <- file.path(td, "global.R")
  ## every job leaves a file named by the pid of the R session that ran it
  writeLines(sprintf("file.create(file.path(%s, paste0('pid_', Sys.getpid())))", deparse(outdir)), global)
  queue <- file.path(td, "queue.rds")
  logs <- file.path(td, "logs")

  ## record what is typed into panes, and still do it
  sent <- list()
  realRun <- SpaDES.project:::.tmux_run
  testthat::local_mocked_bindings(
    .tmux_run = function(...) {
      a <- c(...)
      if (a[1] == "send-keys") sent[[length(sent) + 1L]] <<- a
      realRun(...)
    }, .package = "SpaDES.project")

  workers <- experimentTmux(df = data.frame(.ELFind = "6.1.1", .rep = 1), global_path = global,
                            n_workers = 1, queue_path = queue, activeRunningPath = logs,
                            delay_after_split = 0.1, delay_before_source = 0, stagger_by = 0,
                            snapshot_library = FALSE, continue = FALSE)
  keys <- vapply(sent, function(a) a[4], character(1))   # the typed text: send-keys -t <pane> <text> C-m
  expect_gt(length(keys), 0L)

  ## short, and each names a file that exists
  files <- sub("^.*'([^']+)'$", "\\1", keys)
  expect_true(all(nchar(keys) < 300), info = paste(nchar(keys), collapse = " "))
  worker <- !grepl("Rscript", keys, fixed = TRUE)   # the other panes run an Rscript file
  expect_true(all(nchar(keys[worker]) <= nchar(files[worker]) + 10L), info = paste(keys, collapse = "\n")) # `bash '<file>'`, nothing more
  expect_true(all(file.exists(files)), info = paste(files, collapse = "\n"))

  ## the worker's launcher runs its R profile, which holds the worker loop call for this queue and global
  launcher <- files[grepl("worker_launch_01\\.sh$", files)]
  expect_length(launcher, 1L)
  cmd <- readLines(launcher)
  profile <- sub("^.*R_PROFILE_USER='([^']+)'.*$", "\\1", cmd)
  expect_true(file.exists(profile))
  prof <- paste(readLines(profile), collapse = "\n")
  expect_match(prof, "tmuxRunWorkerLoop(", fixed = TRUE)
  expect_match(prof, sprintf("queue_path=%s", deparse(normalizePath(queue))), fixed = TRUE)
  expect_match(prof, sprintf("global_path=%s", deparse(global)), fixed = TRUE)
  ## the interactive R the worker has always had, with its environment
  expect_match(cmd, "--interactive", fixed = TRUE)
  expect_match(cmd, "R_TESTS=", fixed = TRUE)
  expect_match(cmd, "SPADES_USE_REQUIRE=", fixed = TRUE)

  pids <- function() list.files(outdir, "^pid_")
  expect_true(wait_for(function() length(pids()) == 1L, timeout_s = 120))

  ## What up-arrow, Enter types: the same command, in a shell. Put the job back and run it in a new pane.
  q <- readRDS(queue); q$status <- "PENDING"; saveRDS(q, queue)
  pane2 <- trimws(tm("split-window", "-d", "-P", "-F", "#{pane_id}", "-t", "t")$stdout)
  tm("send-keys", "-t", pane2, keys[grepl("worker_launch_01", keys)], "C-m")
  expect_true(wait_for(function() length(pids()) == 2L, timeout_s = 120),
              info = paste(tm("capture-pane", "-p", "-t", pane2)$stdout))
})

test_that(".mirror_local_queue retries a timed-out lock and says so when it gives up", {
  skip_if_not_installed("filelock")
  q <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(status = "PENDING"), q)
  calls <- 0L
  testthat::local_mocked_bindings(lock = function(...) { calls <<- calls + 1L; NULL }, .package = "filelock")
  expect_message(SpaDES.project:::.mirror_local_queue(q, 1L, list(status = "RUNNING"), lockTries = 2L),
                 "NOT updated")
  expect_equal(calls, 2L)
  expect_equal(readRDS(q)$status, "PENDING")
})
