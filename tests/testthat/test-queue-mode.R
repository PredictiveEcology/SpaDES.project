# tests/testthat/test-queue-mode.R

testthat::test_that("queue mode: workers continue until all rows are DONE", {
  skip_if_no_tmux()

  td <- tempfile("tmux_queue")
  dir.create(td)
  global <- file.path(td, "global.R")
  queue_path <- file.path(td, "tmux_queue.rds")
  log_file <- file.path(td, "queue.log")
  outdir <- file.path(td, "out")
  dir.create(outdir)

  # global.R for queue: write line to log and save result per .ELFind
  writeLines(
    sprintf(
      'line <- paste(format(Sys.time(), "%%H:%%M:%%S"),
                     "ELFind=", get(".ELFind", envir=.GlobalEnv),
                     "rep=",    get(".rep",    envir=.GlobalEnv))
       cat(line, "\\n", file = "%s", append = TRUE)
       saveRDS(list(.ELFind=get(".ELFind", envir=.GlobalEnv),
                    .rep   =get(".rep",    envir=.GlobalEnv)),
               file.path("%s", paste0("res_", get(".ELFind", envir=.GlobalEnv), ".rds")))
      ', log_file, outdir
    ),
    global
  )

  # df with more rows than workers
  expt <- data.frame(
    .ELFind = c("A", "B", "C", "D"),
    .rep    = c(1, 2, 3, 4),
    check.names = FALSE
  )

  # Spawn two workers in queue mode (auto-writes queue)
  workers <- tmux_spawn_workers_from_df(
    df                  = expt,
    global_path         = global,
    n_workers           = 2,
    start_cmd           = "R",
    delay_before_source = 2,
    stagger_by          = 1,
    set_mouse           = TRUE,
    continue            = TRUE,
    queue_path          = queue_path
  )

  # Poll until queue shows all DONE
  ok <- wait_for(function() {
    if (!file.exists(queue_path)) return(FALSE)
    q <- readRDS(queue_path)
    all(q$status == "DONE")
  }, timeout_s = 120, poll_every = 0.5)

  testthat::expect_true(ok)

  # Validate outputs exist
  files <- list.files(outdir, pattern = "^res_.*\\.rds$", full.names = TRUE)
  testthat::expect_equal(length(files), 4)
  vals <- lapply(files, readRDS)
  testthat::expect_true(all(vapply(vals, function(x) x$.ELFind %in% c("A","B","C","D"), logical(1))))

  # Optional dev cleanup
  tmux_kill_panes(workers)
})