#' Find the project root directory
#'
#' Searches from current working directory for and Rstudio project file
#' or git repository, falling back on using the current working directory.
#'
#' @return `findProjectPath` returns an absolute path;
#'         `findProjectName` returns the basename of the path.
#'
#' @export
#' @importFrom rprojroot find_root from_wd is_git_root is_rstudio_project
#' @rdname findProject
findProjectPath <- function() {
  find_root(is_rstudio_project | is_git_root | from_wd, path = getwd())
}

#' @export
#' @rdname findProject
findProjectName <- function() {
  basename(findProjectPath())
}

isAbsolutePath <- function(pathnames) {
  keep <- is.character(pathnames)
  if (isFALSE(keep))
    stop("pathnames must be character")
  nams <- names(pathnames)
  ret <- fs::is_absolute_path(pathnames)
  if (!is.null(nams))
    names(ret) <- nams
  return(ret)
  # origPn <- pathnames
  # nPathnames <- length(pathnames)
  # if (nPathnames == 0L)
  #   return(logical(0L))
  # if (nPathnames > 1L) {
  #   res <- sapply(pathnames, FUN = isAbsolutePath)
  #   return(res)
  # }
  # if (is.na(pathnames))
  #   return(FALSE)
  # if (regexpr("^~", pathnames) != -1L)
  #   return(TRUE)
  # if (regexpr("^.:(/|\\\\)", pathnames) != -1L)
  #   return(TRUE)
  # components <- strsplit(pathnames, split = "[/\\]")[[1L]]
  # if (length(components) == 0L)
  #   return(FALSE)
  # (components[1L] == "")
}


#' Helpers to develop easier to understand code.
#'
#' A set of lightweight helpers that are often not strictly necessary, but they
#' make code easier to read.
#'
#' @export
#' @rdname helpers
#' @param username A character string of a username.
#' @return if `username` is non-NULL, returns a logical indicating whether
#' the current user matches the supplied `username`.
#' Otherwise returns a character string with the value of the current user.
user <- function(username = NULL) {
  if (is.null(username)) {
    Sys.info()[["user"]]
  } else {
    identical(username, Sys.info()[["user"]])
  }
}

#' @param machinename A character string, which will be used as a partial match via
#' `grep`, so the entire machine name is not necessary. A user can use `regex` if
#' needed, e.g., `"^machine1"` will match `"machine15"` and `"machine12"`, but not
#' `"thisIs_machine1"`.
#' @export
#' @rdname helpers
#' @return
#' `machine` returns a logical indicating whether the current machine name
#' `Sys.info()[["nodename"]]` is matched by `machinename`.
machine <- function(machinename = NULL) {
  if (is.null(machinename)) {
    Sys.info()[["nodename"]]
  } else {
    grepl(machinename, Sys.info()[["nodename"]])
  }
}

#' @export
#' @rdname helpers
#' @details
#' `node` is an alias for `machine`
node <- machine

#' Helpers for cleanup of global state in examples and tests
#'
#' 1. remove project library directory created using `setupProject()`;
#' 2. remove project paths created using `setupProject`;
#' 3. restore original library paths.
#'
#' Detect an interactive SSH PTY session
#'
#' Returns `TRUE` when R is running inside an SSH session that allocated a
#' pseudo-terminal (i.e., started with `ssh -t`).  The SSH server sets the
#' `SSH_TTY` environment variable to the slave PTY device path only in this
#' case; it is absent in plain `ssh` (no PTY), RStudio, local tmux, etc.
#'
#' This matters because `parallelly::makeClusterPSOCK()` spawns Rscript worker
#' processes that inherit the PTY slave file descriptors.  R resets its SIGHUP
#' handler during startup (overriding any inherited SIG_IGN), so if the PTY
#' reference count reaches zero after workers exit, the resulting SIGHUP kills
#' R silently.  Use this function to guard `makeClusterPSOCK` calls:
#'
#' ```r
#' if (!inSshPty()) {
#'   cl <- parallelly::makeClusterPSOCK(cores, ...)
#' } else {
#'   message("SSH PTY session: skipping makeClusterPSOCK (run sequentially)")
#'   cl <- NULL
#' }
#' ```
#'
#' @return Logical scalar: `TRUE` if `SSH_TTY` is set (non-empty).
#' @export
inSshPty <- function() nzchar(Sys.getenv("SSH_TTY"))

#' Diagnostic: does makeClusterPSOCK survive in an ssh -t PTY?
#'
#' Opens a new tmux window that `ssh -t`'s to `host`, starts R with
#' `R_PROFILE_USER` (matching `experimentTmux` exactly), calls
#' `makeClusterPSOCK(workers)`, and logs R's exit status to `/tmp/mcp_test.log`
#' on the remote so you can tell whether R died from a signal (exit >= 129,
#' e.g. SIGHUP = 129) or exited cleanly (0).
#'
#' @param host character; SSH hostname used in `experimentTmux`
#' @param workers integer; number of PSOCK workers (default 32)
#' @return invisibly the tmux pane id; call
#'   `system2("ssh", c(host, "cat /tmp/mcp_test.log"))` afterwards to read the
#'   result.
#' @export
testRemoteCluster <- function(host, workers = 32L) {
  logf <- "/tmp/mcp_test.log"

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'message("[mcp_test] PID=", Sys.getpid(),',
    '        "  SSH_TTY=\'", Sys.getenv("SSH_TTY"), "\'")',
    sprintf('message("[mcp_test] calling makeClusterPSOCK(%d)...")', workers),
    sprintf('cl <- tryCatch(parallelly::makeClusterPSOCK(%dL),', workers),
    '  error = function(e) { message("[mcp_test] ERROR: ", e$message); NULL })',
    'if (!is.null(cl)) {',
    '  message("[mcp_test] SUCCESS -- stopping cluster")',
    '  parallel::stopCluster(cl)',
    '} else {',
    '  message("[mcp_test] FAILED (cl is NULL)")',
    '}',
    'message("[mcp_test] calling quit(0)")',
    'quit(save = "no", status = 0L)'
  ), tmp)

  remote_r <- paste0("/tmp/", basename(tmp))
  system2("scp", c("-q", tmp, paste0(host, ":", remote_r)))

  # bash (not exec) so exit status is captured after R exits.
  # R_PROFILE_USER matches the experimentTmux startup path exactly.
  bash_cmd <- sprintf(
    "R_PROFILE_USER=%s R --no-save --no-restore --interactive; echo R_EXIT:$? | tee -a %s",
    remote_r, logf
  )

  win <- "mcp_test"
  system2("tmux", c("new-window", "-n", win))
  system2("tmux", c("send-keys", "-t", win,
                    sprintf("ssh -t %s bash --norc --noprofile -c %s",
                            host, shQuote(bash_cmd)),
                    "Enter"))

  message("Test running in tmux window '", win, "'")
  message("Read result: system2('ssh', c('", host, "', 'cat ", logf, "'))")
  invisible(win)
}

# Patch parallelly::makeClusterPSOCK so workers don't inherit the PTY slave
# fds.  Called from R_PROFILE_USER when SSH_TTY is set.
#
# How it works:
#   parallelly::makeClusterPSOCK accepts a `rscript` argument — a path to an
#   executable that is called with the same args as Rscript.  We write a tiny
#   shell wrapper that closes fd 0/1/2 (the PTY slave) with exec redirections
#   before exec'ing Rscript.  Workers never hold PTY slave fds, so no
#   reference-count change from their side can trigger SIGHUP.
#
#   Because `parallelly` may or may not be loaded when R_PROFILE_USER runs, we
#   register a packageEvent hook AND patch immediately if already loaded.
.patch_makecluster_pty <- function() {
  if (!nzchar(Sys.getenv("SSH_TTY"))) return(invisible(NULL))

  w <- tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh",
               "exec 0</dev/null 1>/dev/null 2>/dev/null",
               "exec Rscript \"$@\""), w)
  Sys.chmod(w, "755")

  do_patch <- function(...) {
    ns <- tryCatch(asNamespace("parallelly"), error = function(e) NULL)
    if (is.null(ns)) return()
    if (!exists("makeClusterPSOCK", envir = ns, inherits = FALSE)) return()
    orig <- get("makeClusterPSOCK", envir = ns, inherits = FALSE)
    ww <- w
    patched <- function(...) {
      args <- list(...)
      if (!"rscript" %in% names(args)) args[["rscript"]] <- ww
      do.call(orig, args)
    }
    environment(patched) <- list2env(list(orig = orig, ww = ww), parent = baseenv())
    tryCatch({
      unlockBinding("makeClusterPSOCK", ns)
      assign("makeClusterPSOCK", patched, envir = ns)
      lockBinding("makeClusterPSOCK", ns)
    }, error = function(e) NULL)
  }

  setHook(packageEvent("parallelly", "onLoad"), do_patch)
  if (isNamespaceLoaded("parallelly")) do_patch()

  message("[SpaDES.project] SSH PTY: makeClusterPSOCK patched — workers will",
          " not inherit PTY fds")
  invisible(NULL)
}

#' @note not intended to be called by users
#'
#' @param prjPaths character vector of paths to be removed
#'
#' @param origLibPaths character string giving the original library path to be restored
#'
#' @return NULL. Invoked for its side effects.
#'
#' @export
#' @importFrom fs path_has_parent path_rel path_split
#' @importFrom Require normPath
#' @importFrom tools R_user_dir
#' @importFrom utils head
#' @rdname test-helpers
.teardownProject <- function(prjPaths, origLibPaths) {
  curlibpath <- utils::head(.libPaths(), 1) |> Require::normPath()
  donttouch <- c(
    "~/R-dev",
    Sys.getenv("R_LIBS_USER"),
    Sys.getenv("R_LIBS_SITE") |> strsplit(":") |> unlist(),
    Sys.getenv("R_LIBS")
  )
  donttouch <- donttouch[nzchar(donttouch)] |>
    normPath() |>
    unique()

  if (!curlibpath %in% donttouch) {
    userDirRoot <- normPath(R_user_dir(""))
    if (path_has_parent(curlibpath, userDirRoot)) {
      prjLibPathRoot <- path_rel(curlibpath, userDirRoot) |>
        path_split() |>
        unlist() |>
        head(1)
      try(unlink(file.path(userDirRoot, prjLibPathRoot), recursive = TRUE))
    } else {
      try(unlink(curlibpath, recursive = TRUE))
    }
  }

  try(unlink(unlist(prjPaths), recursive = TRUE))

  .libPaths(origLibPaths)

  return(NULL)
}
