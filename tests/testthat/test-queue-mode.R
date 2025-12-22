
testthat::test_that("experimentTmux queue mode continues until DONE", {
  skip_if_no_tmux()
  td <- tempfile("tmux_queue"); dir.create(td)
  global <- file.path(td, "global.R")
  queue_path <- file.path(td, "tmux_queue.rds")
  outdir <- file.path(td, "out"); dir.create(outdir)

  writeLines(sprintf(
    'saveRDS(list(.ELFind=get(".ELFind"), .rep=get(".rep")),
             file.path("%s", paste0("res_", get(".ELFind"), ".rds")))',
    outdir
  ), global)

  expt <- data.frame(.ELFind = c("A","B","C","D"), .rep = c(1,2,3,4), check.names = FALSE)

  workers <- experimentTmux(
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

  ok <- wait_for(function() {
    file.exists(queue_path) && all(readRDS(queue_path)$status == "DONE")
  }, timeout_s = 120)
  testthat::expect_true(ok)

  files <- list.files(outdir, "^res_.*\\.rds$", full.names = TRUE)
  testthat::expect_equal(length(files), 4)
  tmux_kill_panes(workers)
})
