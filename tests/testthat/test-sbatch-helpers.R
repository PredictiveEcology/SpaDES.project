## Pure helpers from R/sbatch.R: script generation, argument serialization and
## job-id parsing. None of these need a SLURM controller -- the submit / await /
## kill paths that do are not covered here.

test_that(".deparse_one round-trips values through Rscript -e", {
  dp <- SpaDES.project:::.deparse_one

  expect_identical(dp("a/path"), "\"a/path\"")
  expect_identical(dp(NULL), "NULL")
  expect_identical(dp(42), "42")
  expect_identical(dp(TRUE), "TRUE")
  expect_identical(dp(c("a", "b")), "c(\"a\", \"b\")")

  # calls and symbols must stay unevaluated on the worker
  expect_identical(dp(quote(mean(x))), "quote(mean(x))")
  expect_identical(dp(quote(x)), "quote(x)")

  # ... but a character vector is not a language object, despite is.language
  expect_false(grepl("^quote", dp("mean(x)")))
})

test_that(".sbatch_parse_jobid pulls the trailing job id", {
  pj <- SpaDES.project:::.sbatch_parse_jobid

  expect_identical(pj("Submitted batch job 12345"), 12345L)
  expect_identical(pj("Submitted batch job 12345\n"), 12345L)

  # no trailing digits -> NA rather than an error
  expect_true(is.na(pj("sbatch: error: invalid partition")))
  expect_true(is.na(pj("")))
})

# Minimal argument set for .sbatch_write_script; individual tests override.
writeScript <- function(path, sbatch_opts = list(), worker_idx = 1L) {
  SpaDES.project:::.sbatch_write_script(
    script_path       = path,
    worker_idx        = worker_idx,
    log_file          = "/tmp/w.log",
    stop_file         = "/tmp/stop",
    queue_path        = "/tmp/queue.rds",
    global_path       = "/tmp/global.R",
    on_interrupt      = NULL,
    ss_id             = NULL,
    email             = NULL,
    cache_path        = "/tmp/cache",
    runNameLabel      = "run",
    activeRunningPath = "/tmp/active",
    dots_path         = NULL,
    sbatch_opts       = sbatch_opts,
    r_cmd             = "Rscript",
    r_libs            = "/tmp/lib"
  )
}

test_that(".sbatch_write_script writes an executable script with the reserved directives", {
  f <- withr::local_tempfile(fileext = ".sh")
  expect_identical(writeScript(f, worker_idx = 3L), f)

  body <- readLines(f)
  expect_identical(body[[1]], "#!/bin/bash")
  expect_true("#SBATCH --job-name=spades-worker-03" %in% body)
  expect_true("#SBATCH --output=/tmp/w.log" %in% body)
  expect_true("#SBATCH --error=/tmp/w.log" %in% body)
  expect_true("set -euo pipefail" %in% body)

  # 0755 -- the submit path execs this directly
  expect_identical(substr(as.character(file.mode(f)), 1, 3), "755")
})

test_that(".sbatch_write_script renders user options and normalises underscores", {
  f <- withr::local_tempfile(fileext = ".sh")
  writeScript(f, sbatch_opts = list(mem = "8G", cpus_per_task = 4))
  body <- readLines(f)

  expect_true("#SBATCH --mem=8G" %in% body)
  # cpus_per_task -> cpus-per-task
  expect_true("#SBATCH --cpus-per-task=4" %in% body)
})

test_that(".sbatch_write_script renders valueless flags without '='", {
  f <- withr::local_tempfile(fileext = ".sh")
  writeScript(f, sbatch_opts = list(exclusive = TRUE, requeue = NULL))
  body <- readLines(f)

  expect_true("#SBATCH --exclusive" %in% body)
  expect_true("#SBATCH --requeue" %in% body)
  expect_false(any(grepl("--exclusive=", body, fixed = TRUE)))
})

test_that(".sbatch_write_script refuses to let user options override reserved ones", {
  f <- withr::local_tempfile(fileext = ".sh")
  writeScript(f, sbatch_opts = list(output = "/evil.log", `job-name` = "evil",
                                    job_name = "evil2", error = "/evil.err"))
  body <- readLines(f)

  expect_true("#SBATCH --output=/tmp/w.log" %in% body)
  expect_false(any(grepl("evil", body)))
})

test_that(".sbatch_write_script serialises worker arguments into the Rscript call", {
  f <- withr::local_tempfile(fileext = ".sh")
  writeScript(f)
  body <- paste(readLines(f), collapse = "\n")

  expect_match(body, "tmuxRunWorkerLoop")
  expect_match(body, "queue_path = ", fixed = TRUE)
  # NULL arguments must survive as NULL, not as "NULL" the string
  expect_match(body, "on_interrupt = NULL", fixed = TRUE)
  expect_match(body, "pane_mode = ", fixed = TRUE)
})

test_that(".sbatch_squeue_alive reports NA when squeue is unavailable", {
  skip_if(nzchar(Sys.which("squeue")), "squeue is installed; this tests the absent path")

  res <- SpaDES.project:::.sbatch_squeue_alive(c(1L, 2L, NA_integer_))
  expect_length(res, 3L)
  expect_true(all(is.na(res)))
})

test_that("print.experimentSBATCH summarises workers and marks dry runs", {
  es <- structure(
    list(job_ids   = list(NA_integer_, 987L),
         queue_path = "/tmp/queue.rds",
         log_dir    = "/tmp/logs",
         log_files  = list("/tmp/logs/w1.log", "/tmp/logs/w2.log")),
    class = "experimentSBATCH"
  )

  out <- capture.output(print(es))

  expect_true(any(grepl("2 worker\\(s\\)", out)))
  expect_true(any(grepl("/tmp/queue.rds", out, fixed = TRUE)))
  expect_true(any(grepl("dry-run", out)))
  expect_true(any(grepl("987", out)))
  capture.output(expect_invisible(print(es)))
})
