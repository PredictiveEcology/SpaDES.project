## Standalone reproduction of the two-worker experimentTmux() case that fails on
## CI but passes locally in ~2s. Deliberately NOT a testthat test: the coverage
## job only echoes the tail of testthat.Rout.fail, which truncated away the
## worker logs the last time this was investigated.
##
## Everything it learns is printed to stdout and also written under $OUTDIR so
## the workflow can upload it as an artifact.

outdirRoot <- Sys.getenv("OUTDIR", unset = file.path(tempdir(), "tmuxdbg"))
dir.create(outdirRoot, recursive = TRUE, showWarnings = FALSE)

hdr <- function(...) cat("\n========== ", ..., " ==========\n", sep = "")
sh  <- function(...) {
  out <- suppressWarnings(system2(..., stdout = TRUE, stderr = TRUE))
  cat(paste(out, collapse = "\n"), "\n")
  out
}

hdr("environment")
cat("R           : ", R.version.string, "\n", sep = "")
cat("TMUX        : ", Sys.getenv("TMUX"), "\n", sep = "")
cat("TMUX_PANE   : ", Sys.getenv("TMUX_PANE"), "\n", sep = "")
cat("nproc       : ", system2("nproc", stdout = TRUE), "\n", sep = "")
cat("tmux version: ", system2("tmux", "-V", stdout = TRUE), "\n", sep = "")
cat("libPaths    : ", paste(.libPaths(), collapse = ", "), "\n", sep = "")
cat("SpaDES.project installed: ",
    requireNamespace("SpaDES.project", quietly = TRUE), " (",
    as.character(tryCatch(packageVersion("SpaDES.project"), error = function(e) NA)),
    ")\n", sep = "")

library(SpaDES.project)

td     <- file.path(outdirRoot, "run")
unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE)
outdir <- file.path(td, "out");  dir.create(outdir)
logs   <- file.path(td, "logs"); dir.create(logs)
global <- file.path(td, "global.R")
writeLines(sprintf(
  'cat("[worker] global.R sourced; .ELFind=", .ELFind, "\\n")
   res <- as.list(mget(ls(envir = environment(), all.names = TRUE), envir = environment()))
   saveRDS(res, file.path("%s", paste0("res_", res$.ELFind, ".rds")))
   cat("[worker] wrote result for", res$.ELFind, "\\n")', outdir), global)

expt <- data.frame(.ELFind = c("6.1.1", "6.2.2"), .rep = c(1, 1), check.names = FALSE)

hdr("launching experimentTmux with n_workers = 2")
workers <- experimentTmux(
  df = expt, global_path = global, n_workers = 2,
  start_cmd = "R", delay_before_source = 2, stagger_by = 1,
  set_mouse = TRUE, continue = FALSE, activeRunningPath = logs
)
cat("workers returned: ", paste(unlist(workers), collapse = ", "), "\n", sep = "")

## Poll, capturing pane state as we go so a pane that dies early is still seen.
## Sys.getenv() returns "" (not the default) when the var is set-but-empty,
## which is what a push event gives for an unsupplied workflow input.
deadline <- suppressWarnings(as.numeric(Sys.getenv("DEBUG_TIMEOUT")))
if (!isTRUE(is.finite(deadline))) deadline <- 180
t0 <- Sys.time(); n <- 0L; el <- 0
repeat {
  n <- length(list.files(outdir, "^res_.*\\.rds$"))
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (n >= 2L || el > deadline) break
  Sys.sleep(5)
}
cat("\nresult files after ", round(el), "s: ", n, " of 2\n", sep = "")

hdr("tmux list-panes (all sessions)")
sh("tmux", c("list-panes", "-a", "-F",
             "#{session_name}:#{window_index}.#{pane_index} id=#{pane_id} pid=#{pane_pid} dead=#{pane_dead} cmd=#{pane_current_command} title=#{pane_title}"))

hdr("pane capture for each worker pane")
for (w in unlist(workers)) {
  cat("\n---- pane ", w, " ----\n", sep = "")
  cap <- suppressWarnings(system2("tmux", c("capture-pane", "-p", "-S", "-2000", "-t", w),
                                  stdout = TRUE, stderr = TRUE))
  cat(paste(cap, collapse = "\n"), "\n")
  writeLines(cap, file.path(outdirRoot, paste0("pane_", gsub("%", "", w), ".txt")))
}

hdr("activeRunningPath contents")
lf <- list.files(logs, recursive = TRUE, full.names = TRUE)
cat("files: ", paste(basename(lf), collapse = ", "), "\n", sep = "")
for (f in lf) {
  cat("\n---- ", f, " ----\n", sep = "")
  cat(paste(readLines(f, warn = FALSE), collapse = "\n"), "\n")
}

hdr("outdir contents")
cat(paste(list.files(outdir), collapse = ", "), "\n")

hdr("worker_respawn.R / queue if present")
for (f in list.files(td, pattern = "respawn|queue", recursive = TRUE, full.names = TRUE)) {
  cat("\n---- ", f, " ----\n", sep = "")
  cat(paste(utils::head(readLines(f, warn = FALSE), 60), collapse = "\n"), "\n")
}

file.copy(td, outdirRoot, recursive = TRUE)
hdr("VERDICT")
cat(if (n >= 2L) "REPRODUCED: no -- both workers completed\n"
    else "REPRODUCED: yes -- result files missing\n")
