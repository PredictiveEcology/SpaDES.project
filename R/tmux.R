
# ======================================================================
# tmux orchestrator (package-friendly)
# - Starts plain R in N panes
# - Assigns objects from df rows (column names = object names)
# - Sources global_path
# - First pane: no delay. Subsequent panes: pane-internal Sys.sleep(...)
# - Optional mouse support (set-option -g mouse on)
# - Helper to kill spawned panes
# - All requireNamespace() checks live inside functions
# ======================================================================

#' Enable or disable tmux mouse interaction
#'
#' @description
#' Sets tmux mouse mode via `set-option -g mouse on/off`, enabling pane
#' selection, resizing, and scrolling with the mouse. See tmux manual for details. [1](https://www.rdocumentation.org/packages/rstudioapi/versions/0.17.0/topics/terminalExecute)
#'
#' @param on Logical; `TRUE` to enable, `FALSE` to disable. Default `TRUE`.
#' @return Invisibly returns `on`.
#' @export
tmux_set_mouse <- function(on = TRUE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  .tmux_run("set-option", "-g", "mouse", if (on) "on" else "off")
  invisible(on)
}

# ------------------------------------------------------------------
# Internal helper: prepare a remote machine before launching a worker
# ------------------------------------------------------------------
.setup_remote_machine <- function(host, global_path, queue_path, extra_args_path = NULL,
                                   cache_path = NULL) {
  message("Setting up remote machine: ", host)

  # Derive remote working directory: same relative path from ~ as local
  # NOTE: steps below use .ssh_r() which runs Rscript via non-PTY SSH.
  # Non-PTY SSH shells check $BASH_ENV; if BASH_ENV calls 'sleep $UNSET'
  # with set -e, it fails silently (non-PTY shell continues).
  # PTY SSH (ssh -t, used for interactive R) has a controlling terminal so
  # BASH_ENV terminal-checks ([ -t 1 ]) succeed → sleep fires → shell exits.
  # Fix: add 'set +e' guard to the BASH_ENV file so failures are non-fatal.
  local_home    <- path.expand("~")
  local_abs_dir <- dirname(normalizePath(global_path))
  rel_dir       <- sub(paste0("^", local_home, "/?"), "", local_abs_dir)
  remote_dir    <- paste0("~/", rel_dir)   # e.g. ~/GitHub/FireSenseTesting

  # Repos: local option + r-universe prepended (remote machines won't have this set)
  install_repos <- unique(c("https://predictiveecology.r-universe.dev", getOption("repos")))
  local_lib     <- .libPaths()[1L]

  # Internal helper: write R code to a local temp file, scp it to remote,
  # run it with Rscript (no shell quoting of R expressions needed), then delete.
  # Every script sets repos and cwd before running expr.
  .ssh_r <- function(expr, intern = FALSE) {
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp))
    writeLines(c(
      paste0(".libPaths(c(", deparse1(local_lib), ", .libPaths()))"),
      paste0("options(repos = ", deparse1(install_repos), ")"),
      paste0("setwd(path.expand('", remote_dir, "'))"),
      expr
    ), tmp)
    remote_tmp <- paste0("/tmp/", basename(tmp))
    system(paste0("scp -q ", shQuote(tmp), " ", host, ":", remote_tmp))
    system2("ssh", c(host, paste0("Rscript ", remote_tmp, "; rm -f ", remote_tmp)),
            stdout = intern, stderr = "")
  }

  # 0. Guard BASH_ENV file on remote.
  #
  #    Problem: when sshd runs a command it invokes the user's login shell as
  #      /bin/bash -c "OUR_COMMAND"
  #    That outer bash is a non-login, non-interactive shell, so it reads
  #    $BASH_ENV before executing OUR_COMMAND.  If BASH_ENV contains a failing
  #    'sleep $UNSET_VAR' (or an explicit 'exit') the outer bash may exit
  #    *before* OUR_COMMAND runs:
  #      - With set -e (bash default on some systems): exit non-zero → while
  #        loop stops (ok), but R never started.
  #      - With set +e / explicit 'exit 0': exit zero → while loop continues
  #        → infinite "Connection to ... closed." spam.
  #
  #    Fix: wrap the *entire* original BASH_ENV content in a subshell:
  #      set +e
  #      ( <original content> ) 2>/dev/null || true
  #    A subshell's 'exit' cannot kill the parent shell.  '|| true' ensures
  #    even a non-zero subshell exit is absorbed.  After this wrapper runs
  #    (harmlessly), the outer bash proceeds to execute OUR_COMMAND.
  #
  #    Our 'env -u BASH_ENV bash ...' in OUR_COMMAND then strips BASH_ENV so
  #    the inner bash we launch never reads the file at all.
  .ssh_r(paste0(
    "local({",
    "  be <- Sys.getenv('BASH_ENV');",
    "  if (!nzchar(be)) return(invisible(NULL));",
    "  be <- path.expand(be);",
    "  if (!file.exists(be)) return(invisible(NULL));",
    "  lines <- readLines(be, warn = FALSE);",
    # Already has the subshell wrapper — nothing to do.
    "  if (any(grepl('spades_guard_v2', lines, fixed = TRUE))) return(invisible(NULL));",
    # Strip the old v1 'set +e' prepend if present (same spades_guard tag,
    # different approach — replace it with the subshell wrapper below).
    "  lines <- lines[!grepl('spades_guard', lines, fixed = TRUE)];",
    "  wrapped <- c(",
    "    '# spades_guard_v2: original content wrapped in subshell so exit/failures cannot abort outer bash',",
    "    'set +e',",
    "    '(',",
    "    lines,",
    "    ') 2>/dev/null || true');",
    "  writeLines(wrapped, be);",
    "  message('  Wrapped BASH_ENV in subshell guard (v2): ', be)",
    "})"
  ))

  # 1. Create remote directory, scp global_path into it
  .ssh_r(paste0("dir.create(path.expand('", remote_dir,
                "'), showWarnings = FALSE, recursive = TRUE)"))
  scp_ret <- system(paste0("scp ", shQuote(normalizePath(global_path)),
                            " ", host, ":", remote_dir, "/"))
  if (scp_ret != 0L)
    stop("scp of global_path to '", host, "' failed.", call. = FALSE)
  scp_ret <- system(paste0("scp ", shQuote(normalizePath(queue_path)),
                            " ", host, ":", remote_dir, "/"))
  if (scp_ret != 0L)
    stop("scp of queue_path to '", host, "' failed.", call. = FALSE)

  if (!is.null(extra_args_path) && file.exists(extra_args_path)) {
    scp_ret <- system(paste0("scp ", shQuote(normalizePath(extra_args_path)),
                              " ", host, ":", remote_dir, "/"))
    if (scp_ret != 0L)
      warning("scp of extra_args_path to '", host, "' failed.", call. = FALSE)
  }

  # 1b. Rsync the project R/ folder (user-defined functions sourced by global.R).
  #     Use rsync --delete so removals on localhost propagate to remote.
  local_r_dir <- file.path(dirname(normalizePath(global_path)), "R")
  if (dir.exists(local_r_dir)) {
    message("  rsyncing R/ folder to ", host, ":", remote_dir, "/R/")
    rsync_r_ret <- system(paste0(
      "rsync -a --delete ",
      shQuote(paste0(local_r_dir, "/")),
      " ", host, ":", file.path(remote_dir, "R"), "/"
    ))
    if (rsync_r_ret != 0L)
      warning("rsync of R/ folder to '", host, "' failed.", call. = FALSE)
  }

  # 2. Persist repos, .libPaths(), and SSL env vars in ~/.Rprofile on remote.
  #    ~/.Rprofile runs at R startup before any packages load, so this ensures
  #    local_lib is first in .libPaths() from the very first namespace lookup —
  #    preventing system-lib packages (e.g. purrr 1.0.4) from loading ahead of
  #    the project versions.
  #    SSL vars (CURL_CA_BUNDLE / SSL_CERT_FILE) are set here so that libcurl
  #    inside R can find CA certificates for HTTPS downloads, even when R is
  #    launched via a non-login SSH session where /etc/profile.d/ is not sourced.
  repos_line   <- paste0("options(repos = ", deparse1(install_repos), ")")
  libpath_line <- paste0(".libPaths(c(", deparse1(local_lib), ", .libPaths()))")
  ssl_line     <- paste0(
    "local({",
    "  ca <- c('/etc/ssl/certs/ca-certificates.crt',",   # Debian/Ubuntu
    "          '/etc/pki/tls/certs/ca-bundle.crt',",      # CentOS/RHEL
    "          '/etc/ssl/cert.pem');",                     # macOS/Alpine
    "  ca <- ca[file.exists(ca)];",
    "  if (length(ca)) Sys.setenv(CURL_CA_BUNDLE = ca[1L], SSL_CERT_FILE = ca[1L])",
    "})"
  )
  .ssh_r(paste0(
    "rprof <- path.expand('~/.Rprofile'); ",
    "existing <- if (file.exists(rprof)) readLines(rprof, warn = FALSE) else character(0); ",
    "existing <- existing[!grepl('^options\\\\(repos|^\\\\.libPaths\\\\(|^local\\\\(\\\\{.*ca <-', existing)]; ",
    "writeLines(c(existing, ", deparse1(libpath_line), ", ", deparse1(repos_line), ", ", deparse1(ssl_line), "), rprof)"
  ))

  # 3. Verify Require matches local installation (version + source)
  local_req_ver <- as.character(packageVersion("Require", lib.loc = local_lib))
  local_req_dsc <- packageDescription("Require", lib.loc = local_lib)
  if (identical(local_req_dsc$RemoteType, "github")) {
    local_req_src <- paste0(local_req_dsc$RemoteUsername, "/", local_req_dsc$RemoteRepo)
    if (!is.null(local_req_dsc$RemoteRef))
      local_req_src <- paste0(local_req_src, "@", local_req_dsc$RemoteRef)
  } else if (!is.null(local_req_dsc$Repository)) {
    local_req_src <- local_req_dsc$Repository  # e.g. https://predictiveecology.r-universe.dev
  } else {
    local_req_src <- "CRAN"
  }

  remote_req_info <- trimws(paste(collapse = "",
    .ssh_r(paste0(
      "dsc <- packageDescription('Require');",
      "ver <- as.character(packageVersion('Require'));",
      "src <- if (identical(dsc$RemoteType, 'github')) {",
      "  ref <- paste0(dsc$RemoteUsername, '/', dsc$RemoteRepo);",
      "  if (!is.null(dsc$RemoteRef)) ref <- paste0(ref, '@', dsc$RemoteRef);",
      "  ref",
      "} else if (!is.null(dsc$Repository)) dsc$Repository else 'CRAN';",
      "cat(ver, src)"
    ), intern = TRUE)))
  remote_req_parts <- strsplit(trimws(remote_req_info), "\\s+")[[1L]]
  remote_req_ver   <- if (length(remote_req_parts) >= 1L) remote_req_parts[1L] else "0"
  remote_req_src   <- if (length(remote_req_parts) >= 2L) remote_req_parts[2L] else "CRAN"

  needs_install <- tryCatch(
    numeric_version(remote_req_ver) < numeric_version(local_req_ver),
    error = function(e) TRUE
  ) || !identical(remote_req_src, local_req_src)

  if (needs_install) {
    message("  Installing Require ", local_req_ver, " (", local_req_src, ") on ", host,
            "\n  (remote has: ", remote_req_ver, " / ", remote_req_src, ")")
    if (grepl("/", local_req_src)) {
      # GitHub source: install via pak so the exact ref is respected
      .ssh_r(paste0("pak::pak(", deparse1(local_req_src), ")"))
    } else {
      .ssh_r("install.packages('Require')")
    }
  }

  # 4. Install usethis
  .ssh_r("Require::Install('usethis')")

  # 5. Propagate GitHub credentials from localhost to remote.
  # Read the local token and pipe it into `git credential approve` on the remote
  # so the remote can clone/install from GitHub without interactive setup.
  local_creds <- tryCatch(gitcreds::gitcreds_get(), error = function(e) NULL)
  if (!is.null(local_creds)) {
    cred_input <- paste0(
      "protocol=https\n",
      "host=github.com\n",
      "username=", local_creds$username, "\n",
      "password=", local_creds$password, "\n"
    )
    message("  Propagating GitHub credentials to ", host)
    system2("ssh", c(host, "git credential approve"),
            input = strsplit(cred_input, "\n")[[1]])
  } else {
    # Fall back to checking whether the remote already has credentials
    creds_out <- trimws(paste(collapse = "",
      .ssh_r("tryCatch({gitcreds::gitcreds_get(); cat('ok')}, error=function(e) cat('none'))",
             intern = TRUE)))
    if (!grepl("ok", creds_out, fixed = TRUE))
      stop(
        "No GitHub credentials found locally or on '", host, "'. ",
        "Run: usethis::create_github_token() then gitcreds::gitcreds_set()",
        call. = FALSE
      )
  }

  # 6. Install system libraries required by R packages that compile from source.
  #    Covers: spatial (terra/sf), HTTP (curl/httr), XML, archive, git, fonts, graphics.
  sys_pkgs <- paste(
    "libgdal-dev libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev",  # spatial
    "libssl-dev libcurl4-openssl-dev libxml2-dev",                          # curl/httr/xml2
    "libarchive-dev",                                                        # archive pkg
    "libgit2-dev",                                                           # gert/git2r
    "libfontconfig1-dev libharfbuzz-dev libfribidi-dev",                    # textshaping/ragg
    "libpng-dev libjpeg-dev libtiff-dev",                                   # graphics devices
    "libfreetype6-dev",                                                      # freetypeharfbuzz
    "libabsl-dev",                                                           # abseil (grpc/protobuf deps)
    "r-base-dev"                                                             # R compilation headers
  )
  message("  Installing system libraries on ", host,
          "\n  (if this fails, configure passwordless sudo on ", host,
          " for apt-get, or run manually: ",
          "sudo apt-get install -y --no-install-recommends ", sys_pkgs, ")")
  # sudo -n: non-interactive (fails immediately if password required, no hang).
  # || true so a missing sudo/password doesn't abort the whole setup.
  system2("ssh", c(host, paste0(
    "DEBIAN_FRONTEND=noninteractive sudo -n apt-get install -y --no-install-recommends ",
    sys_pkgs,
    " || echo 'NOTE: sudo apt-get failed on ", host,
    " -- passwordless sudo may be needed; continuing anyway'"
  )))

  # 8. Ensure remote lib path exists (must match localhost so installed paths are identical).
  message("  Ensuring remote lib path exists: ", local_lib)
  system2("ssh", c(host, paste0("mkdir -p ", shQuote(local_lib))))

  # Rsync SpaDES.project installed directory to remote lib.
  # Both machines share the same platform, so the compiled lazy-load databases
  # and binary files are compatible — no R CMD INSTALL needed on the remote.
  local_sp_path <- find.package("SpaDES.project", lib.loc = local_lib)
  local_sp_ver  <- as.character(packageVersion("SpaDES.project", lib.loc = local_lib))
  message("  rsyncing SpaDES.project (", local_sp_ver, ") to ", host, ":", local_lib, "/")
  rsync_ret <- system(paste0(
    "rsync -a --delete ",
    shQuote(paste0(normalizePath(local_sp_path), "/")),
    " ", host, ":", file.path(local_lib, "SpaDES.project"), "/"
  ))
  if (rsync_ret != 0L)
    stop("rsync of SpaDES.project to '", host, "' failed.", call. = FALSE)

  # 9. Install SpaDES.project's dependencies on remote via Require::Install.
  # For Imports/Depends/LinkingTo: install all (hard requirements).
  # For Suggests: only those installed on localhost — avoids pulling in dev/test
  # packages (testthat, knitr, …) while still propagating runtime Suggests like
  # googlesheets4/googledrive/cli.
  .parse_desc_pkgs <- function(fields) {
    dsc <- read.dcf(system.file("DESCRIPTION", package = "SpaDES.project"),
                    fields = fields)
    raw  <- paste(dsc[!is.na(dsc)], collapse = ",")
    pkgs <- trimws(unlist(strsplit(raw, ",")))
    pkgs <- trimws(sub("\\s*\\(.*", "", pkgs))
    pkgs[nzchar(pkgs) & pkgs != "R"]
  }
  hard_pkgs      <- .parse_desc_pkgs(c("Imports", "Depends", "LinkingTo"))
  suggests_all   <- .parse_desc_pkgs("Suggests")
  local_inst     <- rownames(utils::installed.packages(lib.loc = local_lib))
  suggests_local <- intersect(suggests_all, local_inst)
  all_pkgs       <- unique(c(hard_pkgs, suggests_local))
  # Packages whose pre-compiled binaries link against specific system library
  # versions (e.g. libgdal.so.37) that may differ on the remote — compile from
  # source so they link against whatever version the remote actually has.
  src_pkgs  <- c("terra", "sf", "rgdal", "rgeos", "lwgeom")
  src_pkgs  <- intersect(src_pkgs, all_pkgs)
  bin_pkgs  <- setdiff(all_pkgs, src_pkgs)
  if (length(src_pkgs) > 0L) {
    message("  Compiling from source on ", host, ": ", paste(src_pkgs, collapse = ", "))
    .ssh_r(paste0("Require::Install(", deparse1(src_pkgs),
                  ", libPaths = .libPaths()[1L], type = 'source')"))
  }
  message("  Installing ", length(bin_pkgs), " dependency packages on ", host)
  # Pre-install packages whose system-lib versions are commonly too old to satisfy
  # transitive version requirements during compilation.  Require::Install searches
  # all .libPaths() when deciding what is "already installed", so without explicit
  # version constraints it would see purrr 1.0.4 in the system lib and skip it —
  # then furrr's R CMD INSTALL subprocess would find the wrong purrr.
  # By stating the minimum required version, Require is forced to install a
  # qualifying version to local_lib before the main batch install runs.
  .ssh_r(paste0(
    "Require::setLinuxBinaryRepo(); ",
    "Require::Install(",
    "c('purrr (>= 1.2.1)', 'rlang (>= 1.1.7)', 'cli (>= 3.6.0)', 'vctrs (>= 0.6.0)'),",
    " libPaths = .libPaths()[1L])"
  ))
  .ssh_r(paste0("Require::setLinuxBinaryRepo(); Require::Install(", deparse1(bin_pkgs),
                ", libPaths = .libPaths()[1L])"))

  # 10. Rsync Require package binary cache to speed up future installations on remote.
  local_cache <- Require::cachePkgDir()
  if (nzchar(local_cache) && dir.exists(local_cache)) {
    message("  Ensuring remote Require cache exists: ", local_cache)
    system2("ssh", c(host, paste0("mkdir -p ", shQuote(local_cache))))
    message("  rsyncing Require package cache to ", host, ":", local_cache)
    cache_rsync_ret <- system(paste0(
      "rsync -a ",
      shQuote(paste0(local_cache, "/")),
      " ", host, ":", local_cache, "/"
    ))
    if (cache_rsync_ret != 0L)
      warning("rsync of Require cache to '", host, "' may have failed.")
  }

  # 11. Rsync gargle OAuth cache so the remote can authenticate without a browser prompt.
  # Use the explicitly-passed cache_path first (must match what the worker uses),
  # falling back to getOption("gargle_oauth_cache") if not supplied.
  gargle_cache <- if (!is.null(cache_path)) cache_path else getOption("gargle_oauth_cache")
  if (!is.null(gargle_cache) && !isFALSE(gargle_cache)) {
    gargle_cache <- normalizePath(gargle_cache, mustWork = FALSE)
    if (dir.exists(gargle_cache)) {
      cache_parent <- dirname(gargle_cache)
      message("  Ensuring remote gargle cache parent exists: ", cache_parent)
      system2("ssh", c(host, paste0("mkdir -p ", shQuote(cache_parent))))
      message("  rsyncing gargle OAuth cache to ", host, ":", gargle_cache)
      cache_rsync_ret <- system(paste0(
        "rsync -a --delete ",
        shQuote(paste0(gargle_cache, "/")),
        " ", host, ":", gargle_cache, "/"
      ))
      if (cache_rsync_ret != 0L)
        warning("rsync of gargle OAuth cache to '", host, "' may have failed.")
    } else {
      message("  gargle_oauth_cache path does not exist locally, skipping: ", gargle_cache)
    }
  }

  invisible(TRUE)
}

#' Spawn tmux worker panes and process a job queue
#'
#' @description
#' Creates `n_workers` tmux panes in the current window, tiles them, and starts
#' a worker loop in each one that claims and runs jobs from a file-backed queue
#' (`queue_path`).  Control returns immediately to the **master pane**; all work
#' happens asynchronously inside the worker panes.
#'
#' ## Worker loop modes (`pane_mode`)
#'
#' ### `"killAndNewPane"` (default)
#' Each worker runs **one job per R session**, then exits.  A fresh R session
#' starts automatically for the next job, freeing all memory between runs.
#'
#' - **localhost panes**: After each job, `runWorkerLoop()` calls
#'   `tmux respawn-pane -k`, which replaces the current pane’s process
#'   in-place with a new `Rscript` invocation.  No retiling needed.
#' - **Remote panes** (`cores = "hostname"`): The local pane runs a bash
#'   while-loop: `ssh host Rscript -e "..."`.  Each `Rscript` exits with
#'   status 0 (job ran — loop continues) or status 1 (queue empty / fatal
#'   error — loop stops).  `ssh` is used *without* `-t` so that `Rscript`
#'   remains non-interactive and exit status is forwarded cleanly.
#'
#' ### `"reuse"`
#' Each worker loops inside a single R session (`repeat { runNextWorker() }`).
#' Memory accumulates across jobs — useful for lightweight simulations.
#'
#' ## Remote machines (`cores`)
#' Supplying a hostname in `cores` triggers `.setup_remote_machine()` before
#' workers start.  This step:
#' 1. Creates the remote working directory and copies queue/global files.
#' 2. Persists `options(repos = ...)` in `~/.Rprofile` on the remote.
#' 3. Verifies/installs `Require` on the remote.
#' 4. Checks for GitHub credentials (`gitcreds`).
#' 5. Installs system spatial libraries (`libgdal-dev`, etc.) via `apt-get`.
#' 6. Rsyncs `SpaDES.project` from the local installed library to the same
#'    path on the remote (both machines must share the same platform/R version).
#' 7. Installs `SpaDES.project` dependencies via `Require::Install()`.
#'    Packages with compiled system-library dependencies (`terra`, `sf`, etc.)
#'    are installed from source so they link against the remote’s actual GDAL/GEOS.
#' 8. Rsyncs the `Require` binary package cache (`Require::cachePkgDir()`) to
#'    speed up future installations.
#' 9. Rsyncs the gargle OAuth token cache so the remote can authenticate with
#'    Google APIs without a browser prompt.
#'
#' The remote R session sets `.libPaths(c(local_lib, .libPaths()))` at startup
#' so the project library takes precedence over system libraries.
#'
#' ## Staggered starts
#' Pane 1 starts immediately.  Pane `i > 1` waits
#' `delay_before_source + (i - 2) * stagger_by` seconds inside R before
#' claiming its first job, avoiding simultaneous queue contention at startup.
#' For remote workers in `killAndNewPane` mode the stagger only applies to the
#' first SSH invocation; subsequent loop iterations start immediately.
#'
#' ## Restarting a broken pane
#' If a worker pane is manually interrupted (e.g. Ctrl+C) and drops to a shell
#' prompt, restart it by pressing `↑` (up-arrow) in that pane and hitting Enter.
#' The full command is always in the pane’s bash history:
#' - **localhost**: `Rscript -e "..."` (re-enters `runWorkerLoop`; in
#'   `killAndNewPane` mode `respawn-pane` takes over from the first job onward).
#' - **remote**: `ssh host Rscript -e "..." && while ssh host Rscript -e "..."; do :; done`
#'   (restarts the bash while-loop from scratch).
#'
#' @param df A `data.frame`. Column names become object names in worker panes; values
#'   from each row are assigned prior to sourcing `global_path`.
#' @param global_path Character scalar. Absolute path to the script sourced for each job.
#' @param n_workers Integer. Number of worker panes to spawn. Defaults to `length(cores)`
#'   if `cores` is supplied, otherwise `4`.
#' @param delay_after_split Numeric. Seconds to wait after each `split-window`. Default `2`.
#' @param delay_after_layout Numeric. Seconds to wait after `select-layout`. Default `0.2`.
#' @param delay_between_R_start Numeric. Seconds to wait after starting R in each pane.
#'   Default `0.1`.
#' @param delay_before_source Numeric. Seconds panes 2..n wait before claiming their first
#'   job. Default `60`.
#' @param stagger_by Numeric. Additional seconds per pane beyond pane 2:
#'   pane `i > 1` waits `delay_before_source + (i - 2) * stagger_by`. Default `delay_before_source`.
#' @param activeRunningPath Directory for "running" flag files written while a job is
#'   active. Must be cleaned up manually if a job crashes without removing its flag.
#'   Default: `file.path("logs/", basename(queue_path))`.
#' @param folderWithIterInFilename A quoted expression (optionally using `runName`) for a
#'   folder whose filenames encode iteration info. Currently used by `fireSense_SpreadFit`.
#'   Default `getOption("spades.folderWithIterInFilename", NULL)`.
#' @param statusCalculate A quoted expression (optionally using `runName`) that evaluates
#'   to a path containing job-status output files. Currently used by `fireSense_SpreadFit`.
#'   Default `getOption("spades.statusCalculate", NULL)`.
#' @param continue Logical. Reserved for future single-shot mode; currently ignored.
#' @param queue_path Character. Path to the `.rds` queue file. Defaults to
#'   `file.path(dirname(global_path), "tmux_queue.rds")`.
#' @param on_interrupt `"requeue"` (default) or `"fail"`. Action when a job errors:
#'   requeue it for another worker, or mark it failed and stop this worker.
#' @param ss_id Optional Google Drive spreadsheet/folder ID for live status syncing via
#'   `googlesheets4`. `NULL` disables syncing.
#' @param email Optional email address for gargle/Google OAuth authentication.
#' @param cache_path Optional path to the gargle OAuth token cache directory.
#' @param pane_mode `"killAndNewPane"` (default) or `"reuse"`. See **Worker loop modes**
#'   above.
#' @param cores Character vector of machine hostnames, recycled to `n_workers`. Use
#'   `"localhost"` for the local machine or a bare hostname (e.g. `"sbw"`) for a remote
#'   machine reachable via passwordless SSH. When any remote hosts are listed,
#'   `.setup_remote_machine()` is called for each unique hostname before workers start.
#'   Default `NULL` (all localhost).
#' @param workersToMonitor Character vector of pane titles to monitor (currently unused).
#' @param runNameLabel A quoted expression evaluated against the queue `data.frame` to
#'   produce a human-readable job label used in log files and Google Sheet status updates.
#' @param dots_path Path to an `.rds` file containing extra named objects to load into
#'   each worker’s global environment before sourcing `global_path`. Useful for passing
#'   large objects that cannot easily be serialised into the queue row.
#' @param set_mouse Logical. Enable tmux mouse support (pane selection, scroll). Default `TRUE`.
#' @param ... Additional arguments passed to `.setup_remote_machine()`.
#'
#' @return Invisibly returns a character vector of tmux pane IDs for the spawned workers.
#'   Pass these to `tmux_kill_panes()` to tear down all workers at once.
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Basic local usage ---
#' workers <- experimentTmux(
#'   global_path         = "/abs/path/to/global.R",
#'   queue_path          = "/abs/path/to/queue.rds",
#'   n_workers           = 4,
#'   pane_mode           = "killAndNewPane",
#'   delay_before_source = 60,
#'   stagger_by          = 60,
#'   set_mouse           = TRUE
#' )
#'
#' # --- Mixed local + remote ---
#' # Runs 2 workers on localhost and 2 on remote host "sbw".
#' # .setup_remote_machine("sbw", ...) is called automatically before workers start.
#' workers <- experimentTmux(
#'   global_path = "/abs/path/to/global.R",
#'   queue_path  = "/abs/path/to/queue.rds",
#'   cores       = c("localhost", "localhost", "sbw", "sbw"),
#'   pane_mode   = "killAndNewPane",
#'   email       = "you@example.com",
#'   cache_path  = "/abs/path/to/.secret",
#'   ss_id       = "your-google-sheet-id"
#' )
#'
#' # --- Tear down all workers ---
#' tmux_kill_panes(workers)
#'
#' # --- Restart a single broken pane ---
#' # In the broken pane, press Up then Enter to re-run the last command.
#' }
experimentTmux <- function(df,
                           global_path = "global.R",
                           cores = NULL,
                           n_workers = if (is.null(cores)) 4L else length(cores),
                           delay_after_split = 0.4,
                           delay_after_layout = 0.4,
                           delay_between_R_start = 0.0,
                           delay_before_source = 60,
                           stagger_by = delay_before_source,
                           set_mouse = TRUE,
                           statusCalculate = getOption("spades.statusCalculate"),
                           # quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "objFun"))
                           folderWithIterInFilename = getOption("spades.folderWithIterInFilename"),
                           # quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "hists"))),
                           # --- new arguments ---
                           activeRunningPath = getOption("spades.activeRunningPath"),
                           continue = TRUE,
                           queue_path = NULL,
                           on_interrupt = c("requeue", "fail"),
                           pane_mode = c("killAndNewPane", "reuse"),
                           ss_id = NULL,
                           forceLocalQueueToGS = FALSE,
                           email = getOption("gargle_oauth_email"),
                           cache_path = getOption("gargle_oauth_cache"),
                           workersToMonitor = unique(if (is.null(cores)) "localhost" else cores),
                           runNameLabel = quote(colnames(q)[1:2]),
                           ...) {
  
  # -- dependency check
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  
  on_interrupt <- match.arg(on_interrupt)
  pane_mode    <- match.arg(pane_mode)
  # on_error     <- match.arg(on_error)
  
  # -- preconditions
  
  # Normalize to absolute paths immediately so worker panes launched via
  # `Rscript -e "..."` (which start in ~) can always locate these files.
  global_path <- normalizePath(global_path, mustWork = FALSE)
  if (is.null(queue_path)) {
    queue_path <- file.path(dirname(global_path), "tmux_queue.rds")
  }
  queue_path <- normalizePath(queue_path, mustWork = FALSE)
  #tmux_prepare_queue_from_df(df, queue_path)
  #q <- readRDS(queue_path)

  # Save ... args to RDS so panes can load complex objects (lists, etc.) directly
  dots_path <- file.path(dirname(normalizePath(queue_path)), ".tmux_dots.rds")
  if (length(list(...)) > 0L) {
    saveRDS(list(...), dots_path)
  } else if (file.exists(dots_path)) {
    unlink(dots_path)
  }
  # data.table::setDT(q)
  # list2env(as.list(q[, -..meta_cols]), envir = environment())
  # runNameLabel <- eval(runNameLabel)

  if (!is.null(ss_id)) {
    if (!is.null(email))      options(gargle_oauth_email = email)
    if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
    options(gargle_oauth_client_type = "web")
  }
  # GS sync: check for existing sheet state before overwriting
  if (!is.null(queue_path)) {
    # tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel)
    
    if (!is.null(ss_id)) {
      isDir <- isGoogleDriveDirectory(ss_id)
      if (isTRUE(isDir)) {
        reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
        # googledrive::drive_auth()
        
        # 1. Derive the name from the local queue file
        sheet_name <- gsub("\\.rds$", "", basename(queue_path))
        
        # 2. Check if it already exists in that folder to avoid duplicates
        existing <- googledrive::drive_ls(googledrive::as_id(ss_id), pattern = sheet_name)
        
        if (nrow(existing) > 0) {
          ss_id <- existing$id[1]
        } else {
          reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
          # googlesheets4::gs4_auth()
          # 3. Create the sheet (defaults to root) then move it to the folder
          googlesheets4::gs4_auth()
          googledrive::drive_auth()
          # googledrive::drive_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
          # googlesheets4::gs4_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
          
          new_sheet <- googlesheets4::gs4_create(name = sheet_name, sheets = "Status")
          googledrive::drive_mv(file = googledrive::as_id(new_sheet),
                                path = googledrive::as_id(ss_id))
          ss_id <- as.character(googledrive::as_id(new_sheet))
        }
      }
    }
  }
  
  if (!is.null(ss_id)) {
    # if (!is.null(email))      options(gargle_oauth_email = email)
    # if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
    gs_q <- try(.gs_read_queue(ss_id), silent = TRUE) 

    if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L && isFALSE(forceLocalQueueToGS)) {
      q <- data.table::setDT(gs_q)
      # GS has existing state — merge rather than overwrite
      # data_cols <- gsub("^.", "", data_cols)
      # data.table::setnames(q, new = gsub("^\\.", "", names(q)), old = names(q))
      # runNameLabel <- gsub("^\\.", "", runNameLabel)
      # for (rn in runNameLabel) {
      #   if (!is.character(q[[rn]])) {
      #     set(q, NULL, rn, as.character(q[[rn]]))
      #   }
      # }
      
      # data_cols  <- setdiff(names(q), meta_cols)
      # gs_dcols   <- intersect(data_cols, names(gs_q))
      # browser()
      # 
      # # any rows not represented
      # newRows <- gs_q[!q, on = runNameLabel]
      # localRowThatisDifftOnGS <- q[!gs_q, on = runNameLabel]
      # if (NROW(newRows)) {
      #   q <- rbindlist(list())
      # }
      # 
      # # Row-matching keys from data columns
      # local_keys <- apply(q[,  ..data_cols, drop = FALSE], 1, paste, collapse = "\t")
      # gs_keys    <- apply(gs_q[, ..gs_dcols, drop = FALSE], 1, paste, collapse = "\t")
      # 
      # # For rows present in local df: use GS metadata where row matches
      # for (j in seq_len(nrow(q))) {
      #   gs_match <- which(gs_keys == local_keys[j])
      #   if (length(gs_match) > 0L) {
      #     gs_row <- gs_q[gs_match[1L], ]
      #     for (mc in intersect(meta_cols, names(gs_q)))
      #       q[[mc]][j] <- gs_row[[mc]]
      #   }
      #   # No match → row is new in this run, stays PENDING
      # }
      # 
      # # Append GS rows not present in local df (history from prior runs)
      # gs_only <- which(!gs_keys %in% local_keys)
      # if (length(gs_only) > 0L) {
      #   gs_extra <- gs_q[gs_only, , drop = FALSE]
      #   # Align columns to local schema; fill missing with NA
      #   for (col in setdiff(names(q), names(gs_extra)))
      #     gs_extra[[col]] <- NA
      #   q <- rbind(q, gs_extra[, names(q), drop = FALSE])
      # }

      saveRDS(q, queue_path)
    }

    # Push merged (or fresh) queue to GS
    q_sync        <- as.data.frame(lapply(q, as.character))
    names(q_sync) <- gsub("^\\.", dotTxt, names(q_sync))
    try(googlesheets4::with_gs4_quiet(
      googlesheets4::range_write(ss = ss_id, data = q_sync,
                                  sheet = "Status", range = "A1", reformat = FALSE)
    ), silent = TRUE)
  }
  tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel, statusCalculate = statusCalculate,
                            activeRunningPath = activeRunningPath, ...)
  if (!is.data.frame(df)) stop("'df' must be a data.frame.", call. = FALSE)
  if (!file.exists(global_path)) {
    warning("global_path not found from master R working directory: ", global_path)
  }
  
  if (n_workers < 1L) stop("'n_workers' must be >= 1.", call. = FALSE)
  
  # if (!is.null(queue_path)) {
  #   # tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel)
  #   
  #   if (!is.null(ss_id)) {
  #     isDir <- isGoogleDriveDirectory(ss_id)
  #     if (isTRUE(isDir)) {
  #       reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
  #       # googledrive::drive_auth()
  #       
  #       # 1. Derive the name from the local queue file
  #       sheet_name <- gsub("\\.rds$", "", basename(queue_path))
  # 
  #       # 2. Check if it already exists in that folder to avoid duplicates
  #       existing <- googledrive::drive_ls(googledrive::as_id(ss_id), pattern = sheet_name)
  #       
  #       if (nrow(existing) > 0) {
  #         ss_id <- existing$id[1]
  #       } else {
  #         reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
  #         # googlesheets4::gs4_auth()
  #         # 3. Create the sheet (defaults to root) then move it to the folder
  #         googlesheets4::gs4_auth(email = email, cache = cache_path)
  #         googledrive::drive_auth(email = email, cache = cache_path)
  #         # googledrive::drive_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
  #         # googlesheets4::gs4_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
  #         
  #         new_sheet <- googlesheets4::gs4_create(name = sheet_name, sheets = "Status")
  #         googledrive::drive_mv(file = googledrive::as_id(new_sheet),
  #                               path = googledrive::as_id(ss_id))
  #         ss_id <- as.character(googledrive::as_id(new_sheet))
  #       }
  #     }
  #   }
  # }
  
  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = activeRunningPath, queue_path)
  
  inTmux <- Sys.getenv("TMUX") != ""

  # Convert a local absolute path to its remote equivalent (same relative to ~).
  # Used in both the tmux pane loop and the non-tmux fallback.
  .to_remote_path <- function(p) {
    if (is.null(p)) return(p)
    sub(paste0("^", path.expand("~")), "~", normalizePath(p, mustWork = FALSE))
  }

  # Resolve cores. Setup runs in parallel inside each pane (see below).
  if (is.null(cores)) cores <- rep("localhost", n_workers)

  # Clean up any stale ready-flags from a previous run
  for (.host in setdiff(unique(cores), "localhost")) {
    flag <- .setup_flag_path(.host)
    if (file.exists(flag)) unlink(flag)
  }

  if (inTmux) {
    #stop("Not inside tmux. Start/attach to a tmux session first.", call. = FALSE)
    #}
    
    # -- resolve current tmux window
    target_win <- .tmux_current_window()
    system("tmux set -g pane-border-status top") # sets so titles have names
    
    # -- list existing panes
    pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
    
    # -- mouse on, if requested
    if (set_mouse) tmux_set_mouse(TRUE)
    
    # 1. Create a new pane for the sync process
    if (!is.null(queue_path)) {
      #   # tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel)
      #   
      if (!is.null(ss_id)) {
        #     isDir <- isGoogleDriveDirectory(ss_id)
        #     if (isTRUE(isDir)) {
        #       reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
        #       # googledrive::drive_auth()
        #       
        #       # 1. Derive the name from the local queue file
        #       sheet_name <- gsub("\\.rds$", "", basename(queue_path))
        #       
        #       # 2. Check if it already exists in that folder to avoid duplicates
        #       existing <- googledrive::drive_ls(googledrive::as_id(ss_id), pattern = sheet_name)
        #       
        #       if (nrow(existing) > 0) {
        #         ss_id <- existing$id[1]
        #       } else {
        #         reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
        #         # googlesheets4::gs4_auth()
        #         # 3. Create the sheet (defaults to root) then move it to the folder
        #         googlesheets4::gs4_auth(email = email, cache = cache_path)
        #         googledrive::drive_auth(email = email, cache = cache_path)
        #         # googledrive::drive_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
        #         # googlesheets4::gs4_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
        #         
        #         new_sheet <- googlesheets4::gs4_create(name = sheet_name, sheets = "Status")
        #         googledrive::drive_mv(file = googledrive::as_id(new_sheet),
        #                               path = googledrive::as_id(ss_id))
        #         ss_id <- as.character(googledrive::as_id(new_sheet))
        #       }
        #     }
        
        # 1. Create the Monitoring Pane (Detached)
        mon_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")
        
        # 2. Label it for 2025 observability
        .tmux_run("select-pane", "-t", mon_id, "-T", "Cluster_Monitor")
        
        # 3. Construct the R command
        # We deparse the vector to ensure it's passed correctly as a character string
        # workersToMonitor
        # workersToMonitor <- c("birds", "biomass", "camas", "carbon", "caribou", "coco",
        #                   "core", "dougfir", "fire", "mpb", "sbw", "mega",
        #                   "acer", "abies", "pinus")
        
        mon_cmd <- sprintf(
          "clusters::monitorCluster(cores = %s)",
          deparse1(workersToMonitor)
        )
        
        # 4. Launch in the new pane
        .tmux_run("select-layout", "-t", target_win, "tiled")
        .tmux_run("select-pane", "-t", mon_id, "-T", "Cluster_Monitor")
        
        full_bash_mon_cmd <- sprintf("Rscript -e %s", shQuote(mon_cmd))
        .tmux_run("send-keys", "-t", mon_id, full_bash_mon_cmd, "C-m")
        
        
        # 1. Create the sync pane DETACHED (-d) and capture its unique ID (%)
        # This ensures the focus stays on the Master Pane
        sync_pane_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")
        .tmux_run("select-layout", "-t", target_win, "tiled")
        
        # 2. Prepare the command as a SINGLE line to prevent shell splitting
        # Load ... args from RDS so complex objects (lists, etc.) reach statusCalculate
        dots_preamble_sync <- if (file.exists(dots_path)) {
          sprintf("if (file.exists(%s)) list2env(readRDS(%s), envir = .GlobalEnv); ",
                  deparse1(dots_path), deparse1(dots_path))
        } else ""

        sync_cmd <- sprintf(
          "%soptions(gargle_oauth_email = %s, gargle_oauth_cache = %s); SpaDES.project:::.sync_loop_internal(queue_path=%s, ss_id=%s, email=%s, runNameLabel=quote(%s), statusCalculate=quote(%s), cache_path=%s)",
          dots_preamble_sync,
          deparse1(email),
          deparse1(normalizePath(cache_path)),
          deparse1(normalizePath(queue_path)),
          deparse1(as.character(ss_id)),
          deparse1(email),
          deparse1(runNameLabel),
          deparse1(statusCalculate, collapse = "\n"),
          deparse1(normalizePath(cache_path))
        )
        
        
        # 3. Send keys to the specific ID
        # Adding a leading space ' ' prevents the command from being saved in bash history;
        #  I took this away because I wanted access to the command
        full_bash_cmd <- sprintf("Rscript -e %s", shQuote(sync_cmd))
        .tmux_run("send-keys", "-t", sync_pane_id, full_bash_cmd, "C-m")
        
        # 4. Label the pane for clarity
        .tmux_run("select-pane", "-t", sync_pane_id, "-T", "GSheet_Sync")
      }
    }
    
    # -- Recycle cores; define helpers used per-iteration in the merged loop.
    cores_full <- rep_len(cores, n_workers)

    # For remote panes, the first pane per unique host runs .setup_remote_machine()
    # then starts working; subsequent panes for the same host wait for a local
    # flag file before starting, so all setups happen in parallel across hosts.
    setup_assigned <- character(0)
    setup_expr_for <- function(host) {
      sprintf(
        "SpaDES.project:::.setup_remote_machine(%s, %s, %s, extra_args_path=%s, cache_path=%s)",
        deparse1(host),
        deparse1(normalizePath(global_path, mustWork = FALSE)),
        deparse1(normalizePath(queue_path,  mustWork = FALSE)),
        deparse1(if (!is.null(dots_path) && file.exists(dots_path))
                   normalizePath(dots_path) else NULL),
        deparse1(if (!is.null(cache_path)) normalizePath(cache_path) else NULL)
      )
    }
    # bash snippet: run setup and write flag, or wait for flag (600s timeout).
    # Probe passwordless SSH; if it fails, copy the local public key to the
    # remote (~/.ssh/authorized_keys) via ssh-copy-id.
    ssh_ready_bash <- function(host)
      sprintf(
        "ssh -o BatchMode=yes -o ConnectTimeout=5 %s true 2>/dev/null || ssh-copy-id %s",
        host, host
      )
    setup_bash_for <- function(host, first) {
      flag <- shQuote(.setup_flag_path(host))
      if (first)
        sprintf("%s && Rscript -e %s && touch %s",
                ssh_ready_bash(host), shQuote(setup_expr_for(host)), flag)
      else
        sprintf("%s && i=0; until [ -f %s ] || [ $i -gt 300 ]; do sleep 2; i=$((i+1)); done; [ -f %s ]",
                ssh_ready_bash(host), flag, flag)
    }

    # -- Merged loop: create each pane, retile, and immediately send its
    # startup command.  Pane 1's remote setup starts running while pane 2 is
    # still being created — no waiting for all N panes before work begins.
    worker_ids <- character()
    for (i in seq_len(n_workers)) {
      # 1. Create pane detached so focus stays on Master
      new_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")
      worker_ids <- c(worker_ids, new_id)
      # MANDATORY: reset layout immediately so the next split has room
      .tmux_run("select-layout", "-t", target_win, "tiled")
      Sys.sleep(delay_after_split)

      # 2. Per-worker paths and payload expression
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      is_remote <- cores_full[i] != "localhost"
      qp  <- if (is_remote) .to_remote_path(queue_path)        else queue_path
      gp  <- if (is_remote) .to_remote_path(global_path)       else global_path
      arp <- if (is_remote) .to_remote_path(activeRunningPath) else activeRunningPath
      dp  <- if (is_remote) .to_remote_path(dots_path)         else dots_path
      payload <- .build_worker_r_expr(
        queue_path        = qp,
        global_path       = gp,
        on_interrupt      = on_interrupt,
        runNameLabel      = runNameLabel,
        activeRunningPath = arp,
        ss_id             = ss_id,
        pane_mode         = pane_mode,
        email             = email,
        cache_path        = if (!is.null(cache_path)) normalizePath(cache_path) else NULL,
        dots_path         = if (file.exists(dots_path)) dp else NULL,
        lib_path          = .libPaths()[1L]
      )

      # 3. Send startup command immediately to the freshly created pane
      if (is_remote && pane_mode == "killAndNewPane") {
        # Setup preamble: first pane per unique host runs .setup_remote_machine()
        # and writes a flag; subsequent panes for the same host wait for it.
        first_for_host <- !cores_full[i] %in% setup_assigned
        if (first_for_host) setup_assigned <- c(setup_assigned, cores_full[i])
        setup_pre <- paste0(setup_bash_for(cores_full[i], first_for_host), " && ")

        # Write the payload to a local temp file and scp to the remote.
        # Avoids all shell-quoting of the R expression.
        # No Sys.sleep in the R script: stagger delay is in bash (sleep N &&)
        # so R never starts until the delay is done — no PTY-buffering confusion
        # where the user types during Sys.sleep and input lands in the next job.
        # Build a multi-line R script sourced via R_PROFILE_USER that:
        #  1. Loads the worker call into readline history (up-arrow to re-run)
        #  2. Echoes it so it is visible in the pane
        #  3. Wraps execution in tryCatch + withCallingHandlers:
        #     - withCallingHandlers captures sys.calls() while stack is intact
        #     - tryCatch error handler shows the error and keeps R at '>'
        #     - On success runWorkerLoop() calls quit(0) → bash while-loop
        #       restarts for the next job
        # R_PROFILE_USER silently swallows errors that reach its startup
        # tryCatch, so we MUST catch and display here.
        .make_script <- function(expr, pre_sleep = 0) {
          c(
            # Stagger delay: only sleep on the FIRST run of this R_PROFILE_USER
            # script.  A flag file (script path + ".started") is created after the
            # first sleep so that subsequent loop iterations (same script, same
            # R_PROFILE_USER path) start immediately without sleeping again.
            # Sys.getenv("R_PROFILE_USER") reads the path set by the caller.
            if (pre_sleep > 0) c(
              "local({",
              "  .flag <- paste0(Sys.getenv('R_PROFILE_USER'), '.started')",
              sprintf("  if (!file.exists(.flag)) { Sys.sleep(%g); writeLines('', .flag) }", pre_sleep),
              "})"
            ) else NULL,
            paste0(".wc <- ", deparse1(expr)),
            # invisible(NULL) prevents file.remove()'s TRUE return from auto-printing
            "local({.h <- tempfile(); writeLines(.wc, .h); try(utils::loadhistory(.h), silent = TRUE); try(file.remove(.h), silent = TRUE); invisible(NULL)})",
            # Visual separator + PID so session boundaries and process identity
            # are obvious in the pane.
            # OSC 2 escape updates the tmux pane title via the PTY; works when
            # tmux option 'allow-passthrough' or 'set-titles' is on.
            "local({",
            "  .node  <- Sys.info()[[\"nodename\"]]",
            "  .pid   <- Sys.getpid()",
            "  .title <- paste0(.node, \"-\", .pid)",
            "  # Attempt to update the tmux pane title via terminal escape sequence",
            "  cat(sprintf(\"\\033]2;%s\\007\", .title))",
            "  message(\"\\n\", strrep(\"-\", 60))",
            "  message(\"[\", format(Sys.time(), \"%H:%M:%S\"), \"] New worker session\",",
            "          \"  node: \", .node, \"  PID: \", .pid)",
            "})",
            'message("\\nWorker call (up-arrow to re-run):\\n", .wc, "\\n")',
            ".spades_tb <- NULL",
            "tryCatch(",
            "  withCallingHandlers({",
            paste0("    ", expr),
            "  }, error = function(.e) {",
            "    .spades_tb <<- sys.calls()",
            "  }),",
            "  error = function(.e) {",
            # Standard R error format: "Error in CALL :\n  MESSAGE"
            "    .cl <- conditionCall(.e)",
            "    if (!is.null(.cl))",
            '      message("Error in ", deparse(.cl, nlines = 1L), " :\\n  ", conditionMessage(.e))',
            "    else",
            '      message("Error: ", conditionMessage(.e))',
            '    message("\\n(call stack in .spades_tb -- type traceback(.spades_tb) to inspect)")',
            '    message("\\nq(status=0L) to restart loop | q(status=1L) to stop.")',
            "  }",
            ")"
          )
        }
        # Single script for both first run and subsequent loop iterations.
        # The flag-based Sys.sleep inside .make_script ensures the stagger delay
        # fires only once (first run); subsequent runs skip it via the flag file.
        first_script <- tempfile(fileext = ".R")
        writeLines(.make_script(payload, pre_sleep = pre_sleep), first_script)
        remote_first <- paste0("/tmp/", basename(first_script))
        remote_loop  <- remote_first
        scp_pre      <- sprintf("scp -q %s %s:%s",
                                shQuote(first_script), cores_full[i], remote_first)

        # Set BASH_ENV="" in the LOCAL environment and forward the empty value
        # via ssh SendEnv so that sshd's outer bash (which invokes our command)
        # finds no BASH_ENV file to source.  env(1) then sets R_PROFILE_USER
        # without needing a bash -c wrapper, bypassing shell variable expansion.
        # trap '' INT keeps Ctrl-C from killing the local SSH process.
        r_run <- function(rpath) {
          sprintf("BASH_ENV= ssh -t -o SendEnv=BASH_ENV %s env R_PROFILE_USER=%s R --no-save --no-restore --interactive",
                  cores_full[i], shQuote(rpath))
        }
        bash_cmd <- sprintf("trap '' INT; %s%s && %s && while %s; do sleep 2; done",
                            setup_pre, scp_pre,
                            r_run(remote_first), r_run(remote_loop))
        remote_node <- tryCatch(
          trimws(system2("ssh", c(cores_full[i], "hostname -s"), stdout = TRUE,
                         stderr = FALSE)[1L]),
          error = function(e) ""
        )
        pane_title <- if (nzchar(remote_node) && remote_node != cores_full[i])
          paste0(cores_full[i], "-", remote_node)
        else
          cores_full[i]
        .tmux_run("select-pane", "-t", worker_ids[i], "-T", pane_title)
        .tmux_run("send-keys", "-t", worker_ids[i], bash_cmd, "C-m")
      } else if (is_remote) {
        # Remote + reuse mode: single SSH session; R loops internally.
        # Write a minimal R script and run it via R_PROFILE_USER so the session
        # is interactive (OSC 2 title updates work, errors stay at '>').
        remote_script  <- tempfile(fileext = ".R")
        remote_path    <- paste0("/tmp/", basename(remote_script))
        writeLines(c(
          if (pre_sleep > 0) sprintf("Sys.sleep(%g)", pre_sleep) else NULL,
          payload
        ), remote_script)
        scp_cmd <- sprintf("scp -q %s %s:%s",
                           shQuote(remote_script), cores_full[i], remote_path)
        ssh_cmd <- sprintf(
          "BASH_ENV= ssh -t -o SendEnv=BASH_ENV %s env R_PROFILE_USER=%s R --no-save --no-restore --interactive",
          cores_full[i], shQuote(remote_path)
        )
        remote_node2 <- tryCatch(
          trimws(system2("ssh", c(cores_full[i], "hostname -s"), stdout = TRUE, stderr = FALSE)[1L]),
          error = function(e) ""
        )
        pane_title2 <- if (nzchar(remote_node2) && remote_node2 != cores_full[i])
          paste0(cores_full[i], "-", remote_node2)
        else
          cores_full[i]
        .tmux_run("select-pane", "-t", worker_ids[i], "-T", pane_title2)
        .tmux_run("send-keys", "-t", worker_ids[i],
                  paste(scp_cmd, "&&", ssh_cmd), "C-m")
      } else {
        # Local worker: start Rscript non-interactively so R runs the payload
        # and exits (respawn-pane handles killAndNewPane; runWorkerLoop loops for reuse).
        local_script <- tempfile(fileext = ".R")
        writeLines(c(
          if (pre_sleep > 0) sprintf("Sys.sleep(%g)", pre_sleep) else NULL,
          payload
        ), local_script)
        .tmux_run("send-keys", "-t", worker_ids[i],
                  sprintf("Rscript %s", shQuote(local_script)), "C-m")
      }
    }  # end merged loop

    if (length(worker_ids) < n_workers)
      stop(sprintf("Expected %d new worker panes, found %d.", n_workers, length(worker_ids)))

  } else {
    # Not inside tmux: run first job inline in this R session.
    worker_ids <- NA_character_
    message("Not inside a tmux session; running first job sequentially in this R session.",
             call. = FALSE)
    pre_sleep <- 0
    payload <- .build_worker_r_expr(
      queue_path = queue_path, global_path = global_path,
      on_interrupt = on_interrupt, runNameLabel = runNameLabel,
      activeRunningPath = activeRunningPath, ss_id = ss_id,
      pane_mode = pane_mode, email = email,
      cache_path = if (!is.null(cache_path)) normalizePath(cache_path) else NULL,
      dots_path = if (file.exists(dots_path)) dots_path else NULL,
      lib_path = .libPaths()[1L]
    )
    code <- sprintf("Sys.sleep(%s); %s", pre_sleep, payload)
    message("running:\n")
    message(code)
    eval(parse(text = code))
  }

  invisible(worker_ids)
}


#' Run one queued job (claim-next semantics) in the current R session.
#'
#' @param queue_path character; path to the queue `.rds`
#' @param global_path character; script to source for the job
#' @param on_interrupt "requeue" or "fail". If the sourced script is interrupted, either requeue or mark as FAILED.
#' @param heartbeat_interval_s numeric; seconds between heartbeats while the job runs
#' @return "ok" | "interrupt" | "empty" (if no pending work found); used by runWorkerLoop()
#' @export
#' @param runNameLabel A quoted expression (possibly of `q`, which is the result of `q <- readRDS(queue_path)`).
#'   Default is the first 2 column names of `q`. These will be concatenated and used as
#'   labels for various things including the `activeRunningPath` file(s).
#' @param statusCalculate A quoted expression to compute job status from output files.
#'   Defaults to `getOption("spades.statusCalculate", NULL)`.
#' @param folderWithIterInFilename A quoted expression for a folder containing iteration
#'   info in filenames. Defaults to `getOption("spades.folderWithIterInFilename", NULL)`.
#' @param activeRunningPath Directory for "running" flag files. See `activeRunningPathForTmux`.
#' @export
runNextWorker <- function(queue_path, global_path,
                          on_interrupt = c("requeue","fail"),
                          heartbeat_interval_s = 60,
                          runNameLabel = quote(colnames(q)[1:2]),
                          statusCalculate = getOption("spades.statusCalculate"),
                          # quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "objFun"))
                          folderWithIterInFilename = getOption("spades.folderWithIterInFilename"),
                          # quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "hists"))),
                          activeRunningPath = getOption("spades.activeRunningPath"),
                          ss_id = NULL) {
  if (missing(global_path))
    global_path <- "global.R"
  stopifnot(file.exists(global_path))
  on_interrupt <- match.arg(on_interrupt)

  PANE      <- Sys.getenv("TMUX_PANE")
  use_gs    <- !is.null(ss_id)

  # ---- Google Sheets backend ----
  if (use_gs) {
    worker_id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())
    claimed   <- .gs_claim_next_job(ss_id, worker_id)
    if (is.null(claimed)) return("empty")   # queue empty or race lost — caller retries

    row_i     <- claimed$row_index
    sheet_row <- claimed$sheet_row
    col_pos   <- claimed$col_positions
    q         <- claimed$data
    data.table::setDT(q)
    q <- revertDotNames(q)
    
    data_cols    <- setdiff(names(q), meta_cols)

    for (nm in data_cols) {
      # try parsing as it could be an expression written/recorded as a character
      newPoss <- tryCatch(eval(parse(text = q[[nm]][1L])), error = function(err) q[[nm]][1L], silent = TRUE)
      assign(nm, newPoss, envir = .GlobalEnv)
    }

    # Compute runName from runNameLabel now that data cols are in .GlobalEnv
    runName <- tryCatch({
      raw <- eval(runNameLabel, envir = .GlobalEnv)
      gsub("[^[:alnum:]_.:-]", "-", paste(as.character(raw), collapse = "-"))
    }, error = function(e) paste(q[[data_cols[1L]]][1L], collapse = "-"))

    message("\n[", format(Sys.time(), "%H:%M:%S"), "] Claimed job: ", runName)

    if (nzchar(PANE))
      try({
        if (exists(".tmux_run", mode = "function"))
          .tmux_run("select-pane", "-t", PANE, "-T", runName)
        else
          processx::run("tmux", c("select-pane", "-t", PANE, "-T", runName),
                        echo_cmd = FALSE, echo = FALSE, error_on_status = FALSE)
      }, silent = TRUE)
    
    # Heartbeat: write directly to GS
    if (FALSE) {
      hb_thread <- try(parallel::mcparallel({
        repeat {
          Sys.sleep(heartbeat_interval_s)
          hb_vals <- get_latest_heartbeat(current_run, folderWithIterInFilename = folderWithIterInFilename)
          .gs_write_cells(ss_id, sheet_row,
                          updates       = list(heartbeat_at = hb_vals$ts, heartbeat_iter = hb_vals$iter),
                          col_positions = col_pos)
        }
      }), silent = TRUE)
    }

    # withCallingHandlers runs the error handler WITHOUT unwinding the call
    # stack, so traceback() shows the full chain inside source(global_path).
    # After the handler returns, the error propagates naturally to R's
    # top-level handler (interactive session stays alive).
    # tryCatch(interrupt=) wraps the outside so interrupts are still caught.
    outcome <- tryCatch(
      withCallingHandlers({
        source(global_path, local = .GlobalEnv)
        "ok"
      }, error = function(e) {
        now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        try(.gs_write_cells(ss_id, sheet_row,
                            updates       = list(status = txtInterrupted, finished_at = now),
                            col_positions = col_pos), silent = TRUE)
        # Return NULL; error continues propagating — call stack preserved.
      }),
      interrupt = function(e) "interrupt"
    )

    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message("[", now, "] Job finished: ", runName, "  outcome=", outcome)

    final <- if (isTRUE(outcome == "ok")) {
      list(status = txtDone, finished_at = now)
    } else if (on_interrupt == "requeue") {
      list(status = txtPending, claimed_by = NA_character_)
    } else {
      list(status = txtInterrupted, finished_at = now)
    }
    .gs_write_cells(ss_id, sheet_row, updates = final, col_positions = col_pos)
    return(outcome)
  }

  # ---- File-based backend (original) ----
  stopifnot(file.exists(queue_path))
  if (!requireNamespace("filelock", quietly = TRUE))
    stop("Package 'filelock' is required.")

  LOCKF <- paste0(queue_path, ".lock")

  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = activeRunningPath, queue_path)
  tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel, statusCalculate = statusCalculate,
                            activeRunningPath = activeRunningPath)
  lck <- filelock::lock(LOCKF, timeout = Inf)
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- readRDS(queue_path)
  q <- revertDotNames(q)
  
  runNameLabel <- eval(runNameLabel, envir = environment())

  pending_idx <- which(q$status %in% c(txtInterrupted, txtPending))[1]
  if (is.na(pending_idx)) {
    filelock::unlock(lck)
    return("empty")
  }
  i <- pending_idx

  data_cols <- setdiff(names(q), meta_cols)

  if (is.null(runNameLabel)) {
    if (length(data_cols) == 0L)
      stop("No data columns available to infer 'runNameLabel'.")
    runNameLabel <- data_cols[1L]
  }
  if (!all(runNameLabel %in% names(q)))
    stop(sprintf("runNameLabel '%s' is not a column in the queue.", runNameLabel))

  for (nm in data_cols)
    assign(nm, q[[nm]][i], envir = .GlobalEnv)

  q$status[i]       <- txtRunning
  q$claimed_by[i]   <- PANE
  q$started_at[i]   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  q$machine_name[i] <- Sys.info()[["nodename"]]
  q$process_id[i]   <- Sys.getpid()
  saveRDS(q, queue_path)
  filelock::unlock(lck)

  current_run <- getRunName(q, i, runNameLabel)
  runName     <- gsub("[^[:alnum:]_.:-]", "-", as.character(current_run))
  try({
    if (exists(".tmux_run", mode = "function"))
      .tmux_run("select-pane", "-t", PANE, "-T", runName)
    else
      processx::run("tmux", c("select-pane", "-t", PANE, "-T", runName),
                    echo_cmd = FALSE, echo = FALSE, error_on_status = FALSE)
  }, silent = TRUE)

  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
  startedFile <- file.path(activeRunningPath, paste0("Running_", runName, "_", Sys.getpid(), "_.rds"))
  reproducible::checkPath(dirname(startedFile), create = TRUE)
  saveRDS(runName, file = startedFile)
  on.exit(try(unlink(startedFile), silent = TRUE), add = TRUE)

  
  if (FALSE) {
    hb_thread <- try(parallel::mcparallel({
      repeat {
        Sys.sleep(heartbeat_interval_s)
        hb_vals <- get_latest_heartbeat(current_run, folderWithIterInFilename = folderWithIterInFilename)
        l2 <- filelock::lock(LOCKF, timeout = 10)
        on.exit(try(filelock::unlock(l2), silent = TRUE), add = TRUE)
        if (!is.null(l2)) {
          q2  <- readRDS(queue_path)
          q2 <- revertDotNames(q2)
          
          idx <- which(q2$status == txtRunning & q2$claimed_by == PANE)
          if (length(idx)) {
            q2$heartbeat_at[idx]   <- hb_vals$ts
            q2$heartbeat_iter[idx] <- hb_vals$iter
            saveRDS(q2, queue_path)
          }
          filelock::unlock(l2)
        }
      }
    }), silent = TRUE)
  }

  outcome <- tryCatch(
    withCallingHandlers({
      source(global_path, local = .GlobalEnv)
      "ok"
    }, error = function(e) {
      lck2 <- try(filelock::lock(LOCKF, timeout = 10L), silent = TRUE)
      if (!inherits(lck2, "try-error") && !is.null(lck2)) {
        try({
          q2 <- readRDS(queue_path)
          q2$status[i]      <- txtInterrupted
          q2$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          saveRDS(q2, queue_path)
        }, silent = TRUE)
        try(filelock::unlock(lck2), silent = TRUE)
      }
      # Return NULL; error propagates with call stack intact.
    }),
    interrupt = function(e) "interrupt"
  )

  lck <- filelock::lock(LOCKF, timeout = Inf)
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- readRDS(queue_path)
  if (outcome == "ok") {
    q$status[i]      <- txtDone
    q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  } else if (outcome == "interrupt") {
    if (on_interrupt == "requeue") {
      q$status[i]     <- txtPending
      q$claimed_by[i] <- NA_character_
    } else {
      q$status[i]      <- txtInterrupted
      q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
  }
  saveRDS(q, queue_path)
  filelock::unlock(lck)
  # if (!is.null(hb_thread)) try(tools::pskill(hb_thread$pid), silent = TRUE)
  outcome
}

#' Run queued jobs repeatedly (pane-local loop).
#'
#' @inheritParams runNextWorker
#' @param stop_file optional path; if present, stop after current iteration
#' @param pane_mode Character. `"reuse"` (default) loops inside the same R session.
#'   `"killAndNewPane"` runs one job, spawns a fresh replacement pane, retiles the
#'   tmux window, then kills the current pane — freeing all R memory between jobs.
#' @param email gargle OAuth email; forwarded to replacement panes in `killAndNewPane` mode.
#' @param cache_path gargle OAuth cache path; forwarded to replacement panes.
#' @param dots_path Path to `.tmux_dots.rds` holding extra `...` args; forwarded to
#'   replacement panes so they can reload complex objects before sourcing.
#' @return invisibly TRUE
#' @export
runWorkerLoop <- function(queue_path, global_path,
                          on_interrupt = c("requeue", "fail"),
                          heartbeat_interval_s = 60,
                          stop_file = NULL,
                          activeRunningPath = getOption("spades.activeRunningPath"),
                          runNameLabel = quote(colnames(q)[1:2]),
                          ss_id = NULL,
                          pane_mode = c("reuse", "killAndNewPane"),
                          email = getOption("gargle_oauth_email"),
                          cache_path = getOption("gargle_oauth_cache"),
                          dots_path = NULL) {
  on_interrupt <- match.arg(on_interrupt)
  pane_mode    <- match.arg(pane_mode)
  # Authenticate with Google before any sheet access.
  # Setting options alone is not sufficient in a non-interactive Rscript session;
  # gs4_auth() must be called explicitly so gargle loads the cached token.
  options(gargle_oauth_client_type = "web")
  if (!is.null(email))      options(gargle_oauth_email = email)
  if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
  if (!is.null(ss_id) && !is.null(email) && !is.null(cache_path)) {
    tryCatch(
      googlesheets4::gs4_auth(email = email, cache = cache_path),
      error = function(e) message("gs4_auth warning: ", conditionMessage(e))
    )
  }

  if (missing(global_path)) global_path <- "global.R"

  # ------------------------------------------------------------------
  # killAndNewPane: run one job, respawn this pane in-place, die.
  # respawn-pane -k replaces the current pane's process with a fresh
  # Rscript — same tmux position, no split/retile needed.
  # ------------------------------------------------------------------
  if (pane_mode == "killAndNewPane") {
    if (!is.null(stop_file) && isTRUE(file.exists(stop_file))) return(invisible(TRUE))

    res <- runNextWorker(queue_path, global_path, on_interrupt,
                         heartbeat_interval_s, runNameLabel = runNameLabel,
                         activeRunningPath = activeRunningPath, ss_id = ss_id)

    should_continue <- !identical(res, "empty") &&
                       !(identical(res, "interrupt") && on_interrupt == "fail")

    if (should_continue && nzchar(Sys.getenv("TMUX"))) {
      # Local tmux pane: respawn-pane replaces this process with a fresh Rscript.
      # (Remote workers are handled by the bash while-loop in the local pane.)
      PANE        <- Sys.getenv("TMUX_PANE")
      respawn_cmd <- sprintf("Rscript -e %s",
                             shQuote(.build_worker_r_expr(
                               queue_path        = queue_path,
                               global_path       = global_path,
                               on_interrupt      = on_interrupt,
                               runNameLabel      = runNameLabel,
                               activeRunningPath = activeRunningPath,
                               ss_id             = ss_id,
                               pane_mode         = "killAndNewPane",
                               email             = email,
                               cache_path        = cache_path,
                               dots_path         = dots_path,
                               lib_path          = .libPaths()[1L]
                             )))
      .tmux_run("respawn-pane", "-k", "-t", PANE, respawn_cmd)
    }
    if (should_continue) {
      # Job ran OK (res=ok) or incomplete (res=interrupt+requeue) →
      # exit 0 so the remote bash while-loop restarts for the next job.
      message("[", format(Sys.time(), "%H:%M:%S"), "] res=", res,
              " — restarting R session for next job (sleep 2s)")
      quit(save = "no", status = 0L)
    } else {
      # Queue empty or interrupt+fail: stay interactive so the user can debug.
      # R returns to '>' — the SSH connection (and tmux pane) stays alive.
      # Type q(status=0L) to let the while-loop restart, q(status=1L) to stop.
      message("\nWorker idle: res=", res,
              "\n  q(status=0L) to retry  |  q(status=1L) to stop the loop")
      return(invisible(res))
    }
  }

  # ------------------------------------------------------------------
  # reuse (default): loop inside the same R session
  # ------------------------------------------------------------------
  repeat {
    if (!is.null(stop_file) && isTRUE(file.exists(stop_file))) break
    res <- runNextWorker(queue_path, global_path, on_interrupt,
                         heartbeat_interval_s, runNameLabel = runNameLabel,
                         activeRunningPath = activeRunningPath, ss_id = ss_id)
    if (identical(res, "empty")) break
    if (identical(res, "interrupt") && on_interrupt == "fail") break
    Sys.sleep(stats::runif(1, 0.05, 0.2))
  }
  invisible(TRUE)
}


#' Kill a set of tmux panes (e.g., those spawned by experimentTmux)
#'
#' @description
#' Development utility: kills all panes identified by their tmux pane IDs.
#' Uses `kill-pane -t <pane-id>`; panes already gone are ignored. See tmux manual. [1](https://www.rdocumentation.org/packages/rstudioapi/versions/0.17.0/topics/terminalExecute)
#'
#' @param panes Character vector of tmux pane IDs (e.g., `c("%2", "%3")`) returned by
#'   `experimentTmux()`.
#' @return Invisibly returns the subset of `panes` successfully targeted.
#' @export
tmux_kill_panes <- function(panes) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  if (!length(panes)) return(invisible(character()))
  killed <- character()
  for (pid in panes) {
    ok <- try(.tmux_run("kill-pane", "-t", pid), silent = TRUE)
    if (!inherits(ok, "try-error")) killed <- c(killed, pid)
  }
  invisible(killed)
}

# ---- internal helpers --------------------------------------------------

# Build the R expression string that launches a worker pane.
# Returns a character(1) suitable for send-keys (interactive R) or
# wrapping in `Rscript -e shQuote(.)` (non-interactive / respawn).
# No options() preamble — runWorkerLoop() sets gargle options from its params.
.build_worker_r_expr <- function(queue_path, global_path, on_interrupt, runNameLabel,
                                  activeRunningPath, ss_id, pane_mode, email, cache_path,
                                  dots_path, lib_path = .libPaths()[1L]) {
  # Ensure project lib is first so correct package versions are loaded
  lib_pre <- sprintf(".libPaths(c(%s, .libPaths())); ", deparse1(lib_path))
  # setwd so Rscript -e "..." launched from ~ finds relative-to-project files
  wd      <- dirname(normalizePath(queue_path, mustWork = FALSE))
  wd_pre  <- sprintf("setwd(%s); ", deparse1(wd))
  dots_pre <- if (!is.null(dots_path) && file.exists(dots_path))
    sprintf("if (file.exists(%s)) list2env(readRDS(%s), envir = .GlobalEnv); ",
            deparse1(dots_path), deparse1(dots_path))
  else ""
  sprintf(
    paste0("%s%s%sSpaDES.project::runWorkerLoop(",
           "queue_path=%s, global_path=%s, on_interrupt=%s,",
           " runNameLabel=quote(%s), activeRunningPath=%s, ss_id=%s,",
           " pane_mode=%s, email=%s, cache_path=%s, dots_path=%s)"),
    lib_pre, wd_pre, dots_pre,
    deparse1(queue_path), deparse1(global_path), deparse1(on_interrupt),
    deparse1(runNameLabel), deparse1(activeRunningPath), deparse1(ss_id),
    deparse1(pane_mode), deparse1(email), deparse1(cache_path), deparse1(dots_path)
  )
}

#' @keywords internal
#' @noRd
.setup_flag_path <- function(host) {
  file.path(tempdir(), paste0(".spades_ready_", gsub("[^a-zA-Z0-9]", "_", host)))
}

#' @keywords internal
#' @noRd
.tmux_run <- function(...) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  processx::run("tmux", c(...), echo_cmd = FALSE, echo = FALSE, error_on_status = TRUE)
}

#' @keywords internal
#' @noRd
.tmux_out <- function(...) {
  res <- .tmux_run(...)
  out <- strsplit(res$stdout, "\n", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

#' @keywords internal
#' @noRd
.tmux_current_window <- function() {
  pane_id <- Sys.getenv("TMUX_PANE")
  if (pane_id == "") stop("Not inside tmux (TMUX_PANE is empty).", call. = FALSE)
  sw <- .tmux_out("display-message", "-p", "-t", pane_id, "#{session_name}:#{window_index}")  # formats  [2](https://callr.r-lib.org/)
  if (length(sw) != 1L || !grepl("^.+:[0-9]+$", sw)) {
    stop("Failed to resolve current tmux session:window from TMUX_PANE=", pane_id, call. = FALSE)
  }
  sw
}

#' @keywords internal
#' @noRd
.make_assignment_code <- function(df, row_i, global_path, pre_sleep = 0) {
  cols <- names(df)
  vals <- df[row_i, , drop = FALSE]

  assigns <- vapply(seq_along(cols), function(j) {
    nm <- cols[j]
    v  <- vals[[j]]
    if (is.factor(v)) v <- as.character(v)

    if (is.character(v)) {
      sprintf('%s <- "%s"', nm, v)
    } else if (is.logical(v)) {
      sprintf('%s <- %s', nm, if (is.na(v)) "NA" else if (v) "TRUE" else "FALSE")
    } else if (is.numeric(v) || is.integer(v)) {
      sprintf('%s <- %s', nm, as.character(v))
    } else if (is.na(v)) {
      sprintf('%s <- NA', nm)
    } else {
      sprintf('%s <- %s', nm, deparse(v))
    }
  }, character(1))

  # Prepend pane-internal sleep if requested (FIXED: correct sprintf + quoting)
  sleep_code <- if (pre_sleep > 0) sprintf("Sys.sleep(%s); ", pre_sleep) else ""

  paste0(
    sleep_code,
    paste(assigns, collapse = "; "),
    "; ",
    sprintf('source(%s)', deparse(global_path))
  )
}

#' Initialize a file-backed queue from a data.frame (extended schema)
#'
#' Adds metadata columns used by workers:
#' - status:       PENDING | RUNNING | DONE | FAILED
#' - claimed_by:   tmux pane id that claimed the row
#' - started_at:   "YYYY-MM-DD HH:MM:SS"
#' - finished_at:  "YYYY-MM-DD HH:MM:SS"
#' - DEoptimElapsedTime: numeric seconds (`sum(diff(allIterations[allIterations < 20 minutes]))`)
#' - machine_name: `Sys.info()[["nodename"]]`
#' - process_id:   Sys.getpid()
#' - heartbeat_at: latest timestamp (as character) detected by heartbeat
#' - heartbeat_iter: latest iteration number (integer) detected by heartbeat
#'
#' @param df data.frame; experiment rows (columns become objects in workers)
#' @param queue_path character; path to the queue `.rds` (absolute recommended)
#' @return Invisibly returns `queue_path`.
#' @export
tmux_prepare_queue_from_df <- function(df, queue_path) {
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1)
  q <- cbind(
    df,
    status         = txtPending,
    claimed_by     = NA_character_,
    started_at     = as.character(NA),
    finished_at    = as.character(NA),
    DEoptimElapsedTime   = as.numeric(NA),
    machine_name   = NA_character_,
    process_id     = as.integer(NA),
    heartbeat_at   = as.character(NA),
    heartbeat_iter = as.integer(NA),
    iterationsTotal= as.integer(NA)
  )
  saveRDS(q, queue_path)
  invisible(queue_path)
}


# internal helper: write the worker loop that consumes the queue

# internal helper: write the worker loop that consumes the queue
# .write_worker_loop <- function(queue_path,
#                                global_path,
#                                on_interrupt = c("requeue","fail"),
#                                # on_error     = c("fail","requeue"),
#                                heartbeat_interval_s = 60) {
#   stopifnot(file.exists(queue_path), file.exists(global_path))
#   on_interrupt <- match.arg(on_interrupt)
#   # on_error     <- match.arg(on_error)
# 
#   # --- Inside your .write_worker_loop generator ---
#     # --- Inside .write_worker_loop ---
#     # Use R's raw string syntax to protect the regex \d
#     template <- R"-(
#   if (!requireNamespace("filelock", quietly = TRUE)) {
#     stop("Package filelock is required in worker panes.")
#   }
# 
#   QUEUE  <- %s
#   LOCKF  <- paste0(QUEUE, ".lock")
#   GLOBAL <- %s
#   PANE   <- Sys.getenv("TMUX_PANE")
#   HB_INT <- %d
# 
# 
#   hb_thread <- NULL
# 
#   repeat {
#     # 0. Random jitter
#     Sys.sleep(runif(1, 0.1, 0.8))
# 
#     # 1. Acquire EXCLUSIVE lock on the lock file
#     lck <- filelock::lock(LOCKF, timeout = Inf)
#     on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
# 
#     # 2. Read the queue inside the lock
#     q <- readRDS(QUEUE)
# 
# 
#     # 3. Find all PENDING rows
#     # pending_idx <- which(q$status == txtPending)
#     pending_idx <- which(q$status %%in%% c(txtInterrupted, txtPending))[1]
# 
#     if (length(pending_idx) == 0) {
#       filelock::unlock(lck)
#       break
#     }
# 
#     # 4. Take the VERY FIRST one and claim it IMMEDIATELY
#     i <- pending_idx[1]
# 
#     # Identify non-metadata columns to inject as global variables
#     meta_cols <- c("status", "claimed_by", "started_at", "finished_at",
#                    "DEoptimElapsedTime", "machine_name", "process_id",
#                    "heartbeat_at", "heartbeat_iter", "iterationsTotal")
#     data_cols <- setdiff(names(q), meta_cols)
# 
#     for (nm in data_cols) {
#       # USE envir = .GlobalEnv EXPLICITLY
#       # This ensures variables like .ELFind are available to source(GLOBAL)
#       assign(nm, q[[nm]][i], envir = .GlobalEnv)
#       # 2. Diagnostic (Visible in the tmux pane)
#       message("Injected ", nm, " = ", q[[nm]][i], " into .GlobalEnv")
#     }
# 
#     q$status[i]      <- txtRunning
#     q$claimed_by[i]  <- PANE
#     q$started_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#     q$machine_name[i] <- Sys.info()[["nodename"]]
#     q$process_id[i]   <- Sys.getpid()
# 
# 
#     saveRDS(q, QUEUE)
#     filelock::unlock(lck)
# 
#     current_elfind <- q[[".ELFind"]][i]
# 
#     if (!is.null(hb_thread)) {
#       try(tools::pskill(hb_thread$pid), silent = TRUE)
#       try(parallel::mccollect(hb_thread, wait = FALSE), silent = TRUE)
#     }
# 
#     hb_thread <- parallel::mcparallel({
#       repeat {
#         Sys.sleep(HB_INT)
#         hb_vals <- get_latest_heartbeat(current_elfind)
#         lck2 <- filelock::lock(LOCKF, timeout = 10)
#         on.exit(try(filelock::unlock(lck2), silent = TRUE), add = TRUE)
#         if (!is.null(lck2)) {
#           q2 <- readRDS(QUEUE)
#           idx <- which(q2$status == txtRunning & q2$claimed_by == PANE)
#           if (length(idx)) {
#             q2$heartbeat_at[idx]   <- hb_vals$ts
#             q2$heartbeat_iter[idx] <- hb_vals$iter
#             saveRDS(q2, QUEUE)
#           }
#           filelock::unlock(lck2)
#         }
#       }
#     }, silent = TRUE)
# 
#     outcome <- tryCatch({
#       source(GLOBAL, local = .GlobalEnv)
#       "ok"
#     }, interrupt = function(e) "interrupt")
# 
#     lck <- filelock::lock(LOCKF, timeout = Inf)
#     on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
#     q <- readRDS(QUEUE)
# 
#     if (identical(outcome, "ok")) {
#       q$status[i]      <- txtDone
#       q$finished_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#     } else if (identical(outcome, "interrupt")) {
#       if ("%s" == "requeue") {
#         q$status[i]      <- txtPending
#         q$claimed_by[i]  <- NA_character_
#       } else {
#         q$status[i]      <- "FAILED"
#         q$finished_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#       }
#     }
# 
#     saveRDS(q, QUEUE)
#     filelock::unlock(lck)
#     if (identical(outcome, "interrupt")) break
#   }
#   if (!is.null(hb_thread)) try(tools::pskill(hb_thread$pid), silent = TRUE)
#   )-"
# 
#   # Now run sprintf with exactly 5 arguments
#   loop <- sprintf(
#     template,
#     deparse1(normalizePath(queue_path)),
#     deparse1(normalizePath(global_path)),
#     as.integer(heartbeat_interval_s),
#     on_interrupt
#     # on_error
#   )
# 
# 
#   script_path <- file.path(dirname(queue_path), "tmux_worker_loop.R")
#   writeLines(loop, script_path)
#   normalizePath(script_path)
# }


#' Assess simulation status from PNG outputs
#' @param runName Directory containing the figures/hists
#' @param timeout_min Threshold for inactivity (e.g., 20)
#' @param statusCalculate A quoted expression to compute job status from output files.
#'   Defaults to `getOption("spades.statusCalculate", NULL)`.
assessDoneInFigure <- function(runName, timeout_min = 20, 
                               statusCalculate = getOption("spades.statusCalculate")) {
                               # quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "objFun"))

  # startedFiles <- dir(activeRunningPath, pattern = txtRunning, ignore.case = TRUE)
  # is_running <- runName %in% sapply(startedFiles, function(x) strsplit(x, "_")[[1]][[2]])
  # if (isTRUE(is_running)) {
  #   return(txtRunning)
  # }

  if (!is.null(statusCalculate)) {
    
    browser()
    if (is.call(statusCalculate))
      statusCalculate <- eval(statusCalculate, envir = environment())
    # statusCalculate <- file.path("outputs", runName, "figures", "objFun")
    if (length(statusCalculate) == 0 || !dir.exists(statusCalculate)) return(txtPending)
    
    # Find most recent PNG
    png_files <- list.files(statusCalculate, pattern = "\\.png$", full.names = TRUE)
    if (length(png_files) == 0) return(txtPending)
    
    # Get most recent file
    finfo <- file.info(png_files)
    latest_file <- png_files[which.max(finfo$mtime)]
    last_mod <- finfo$mtime[which.max(finfo$mtime)]
    
    # Check for staleness
    is_stale <- difftime(Sys.time(), last_mod, units = "mins") > timeout_min
    if (!is_stale) return(txtRunning)
    
    # Visual check for "ter" using magick
    # Red pixels typically have high R and low G/B values
    reproducible::.requireNamespace("magick", stopOnFALSE = TRUE)
    img <- magick::image_read(latest_file)
    img_data <- as.integer(img[[1]]) # Get RGBA array
    
    # Heuristic: Red channel > 200 AND Green/Blue < 100
    # img_data is [height, width, channels]
    red_pixels <- sum(img_data[,,1] > 200 & img_data[,,2] < 100 & img_data[,,3] < 100)
    # This is simpler heuristic. Maybe not accurate enough though
    # red_pixels <- sum(img_data[,,1] > 200)
    
    has_red <- red_pixels > 0
    
    if (has_red) {
      return(txtDone)
    } else {
      # return(txtDone)
      return(txtInterrupted)
    }
  } else {
    return(txtPending)
  }
}

#' Refresh and Assess Queue Status from Simulation Outputs
#'
#' Scans the simulation output directories (defined by `runNameLabel`) to assess
#' current status based on file timestamps and visual content of PNGs.
#' If a PNG has not been updated for a specified timeout, the task is marked
#' as "FINISHED" (if red pixels are detected) or "INTERRUPTED" (if no red
#' is detected).
#'
#' @param queue_path Character. Absolute path to the `experiment_queue.rds` file.
#' @param timeout_min Numeric. Minutes of inactivity before a task is
#'   considered stale. Defaults to 20.
#' @param runNameLabel A quoted expression to derive a run label from the queue. Default uses first two columns.
#' @param statusCalculate A quoted expression to compute job status from output files.
#'   Defaults to `getOption("spades.statusCalculate", NULL)`.
#' @param folderWithIterInFilename A quoted expression for a folder with iteration info in filenames.
#'   Defaults to `getOption("spades.folderWithIterInFilename", NULL)`.
#' @param recheckDone Logical. If `TRUE`, re-evaluate DONE status. Default `FALSE`.
#' @param activeRunningPath Directory for "running" flag files. See `activeRunningPathForTmux`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame (the updated queue), invisibly.
#'   As a side effect, updates the RDS file on disk.
#'
#' @export
#' @importFrom filelock lock unlock
#' @importFrom magick image_read
#'
#' @examples
#' \dontrun{
#' # Assessment of all simulations in the current project
#' tmux_refresh_queue_status("experiment_queue.rds", timeout_min = 30)
#' }
tmux_refresh_queue_status <- function(queue_path, timeout_min = 20, runNameLabel = quote(colnames(q)[1:2]),
                                      statusCalculate = getOption("spades.statusCalculate"),
                                      folderWithIterInFilename = getOption("spades.folderWithIterInFilename"),
                                      recheckDone = FALSE, #!is.null(statusCalculate),
                                      activeRunningPath = getOption("spades.activeRunningPath"),
                                      ...) {
  
  if (file.exists(queue_path)) {
    lck <- filelock::lock(paste0(queue_path, ".lock"), timeout = 10000)
    on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
    if (is.null(lck)) stop("Could not lock queue for refresh.")

    q <- try(readRDS(queue_path))
    q <- revertDotNames(q)
    # remove initial dot -- googlesheets can't handle starting dot; so remove all
    # data.table::setnames(q, new = gsub("^\\.", dotTxt, names(q)), old = names(q))
    
    if (is(q, "try-error")) {
      unlink(queue_path)
      return(invisible(NULL))
    }

    # Only refresh rows that aren't already marked DONE
    to_check <- if (!isTRUE(try(recheckDone))) {
      which(!q$status %in% txtDone)
    } else {
      seq_along(q$status)
    }
      # to_check <- seq_len(NROW(q))#which(!q$status %in% txtDone)

    activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
    for (i in to_check) {
      new_status <- txtPending
      
      # put the values from the q columns into this environment so runNameLabel can use them
      list2env(as.list(q[i, -..meta_cols]), envir = environment())
      
      runNameLabel <- eval(runNameLabel, envir = environment())
      if (missing(runNameLabel) || is.null(runNameLabel)) {
        runNameLabel <- setdiff(colnames(q), meta_cols)[1L]
      }
      
      runName <- runNameLabel
      # runName <- getRunName(q, i, runNameLabel)# 
      # runNameSimples <- sapply(runNameLabel, function(rnl) q[i, ..rnl])
      
      # runName <- q[i, runNameLabel] |> paste(collapse = "-")
      # if (runName == "14.1") {
      #   debug(get_latest_heartbeat)
      # }
      # runName <- q[i, runNameLabel] |> paste(collapse = "-")
      allFiles <- dir(activeRunningPath)
      allFilesFull <- dir(activeRunningPath, full.names = TRUE)
      runningFiles <- grep(allFiles, pattern = txtRunning, ignore.case = TRUE, value = TRUE)
      runningFilesFull <- grep(allFilesFull, pattern = txtRunning, ignore.case = TRUE, value = TRUE)
      
      is_running <- sapply(strsplit(runningFiles, "_"), function(x) runName %in% x)
      if (any(is_running)) {
        # confirm that they are still running using pids
        filename <- runningFilesFull[is_running]
        if (length(filename)) {
          toRm <- Map(fiHere = filename, function(fiHere) {
            pid <- strsplit(basename(fiHere), split = "_")[[1]][3] |> as.integer()
            alive <- is_pid_alive_tools(pid)
            if (isFALSE(alive)) {
              return(fiHere)
            } else {
              return(NULL)
            }
            
          })
          toRm <- unlist(toRm)
          if (length(toRm)) {
            unlink(toRm)
            toRmInd <- match(toRm, allFilesFull)
            allFiles <- allFiles[-toRmInd]
            allFilesFull <- allFilesFull[-toRmInd]
            # toRmIndForPartialRunningFiles <- match(toRm, runningFilesFull[is_running])
            toRmIndForRunning <- match(runningFilesFull[is_running], runningFilesFull)
            is_running[toRmIndForRunning] <- FALSE
          }
        }

        
      }
      
      done <- FALSE
      if (!is.null(statusCalculate)) {
        # Unpack ... into local scope so statusCalculate can reference them by name
        dots <- list(...)
        if (length(dots)) list2env(dots, envir = environment())
        # put the values from the q columns into this environment so runNameLabel can use them
        list2env(as.list(q[i, -..meta_cols]), envir = environment())
        evaled <- try(eval(statusCalculate, envir = environment()))
        if (is(evaled, "try-error") || is.null(evaled)) {
          statusCalculate <- NULL
        } else {
          cn <- meta_cols
          for (cc in cn) {
            q[[cc]][i] <- NA
            if (exists(cc, inherits = FALSE)) {
              outHere <- get(cc, inherits = FALSE)
              if (length(outHere) > 0) 
              # if (is(outHere, "try-error")) browser()
                q[[cc]][i] <- outHere
              
            }
          }
        }
      }
      if (!isTRUE(done)) {
        if (isTRUE(any(is_running))) {
          new_status <- txtRunning
        } #else if (isTRUE(is_done)) {
          #new_status <- txtDone
        #} # else {
        #   new_status <- assessDoneInFigure(runName = runName, timeout_min = timeout_min, 
        #                                    statusCalculate = statusCalculate)#, 
        #   #activeRunningPath = activeRunningPath)
        # }
        hb <- NA
        if (!is.null(folderWithIterInFilename)) {
          browser()
          hb <- get_latest_heartbeat(runName, folderWithIterInFilename = folderWithIterInFilename)
          elapsedTime <- hb$elapsed
        } 
        
        fi <- activeRunningFileInfo(activeRunningPath = activeRunningPath, runName = runName, queue_path = queue_path)
        hasFI <- (!is.null(fi) && NROW(fi))
        machine_name <- Sys.info()[["nodename"]]
        if (hasFI) {
          startedAt <- format(fi$mtime, "%Y-%m-%d %H:%M:%S")
          process_id <- gsub(".+([[:digit:]]{6,7}).+","\\1", basename(rownames(fi)))
        }
        
        if (any(unlist(hb) %in% NA) ) {
          if (new_status %in% txtPending) {
            cns <- setdiff(meta_cols, "status")
            for (cn in cns)
              q[[cn]][i] <- NA
          } else {
            # This is RUNNING; but not at DEoptim yet; can also be DONE
            # fi <- activeRunningFileInfo(activeRunningPath = activeRunningPath, runName = runName, queue_path = queue_path)
            if (hasFI) {
              # q[q$process_id %in% names(pidsToRm)[pidsToRm],"status"] <- txtRunning
              # 3
              q$started_at[i] <- startedAt#format(fi$mtime, "%Y-%m-%d %H:%M:%S")
              q$machine_name[i] <- machine_name
            }
            q$DEoptimElapsedTime[i] <- NA
            # q$process_id[i] <- NA
          }
        } else {
          # Update status and timestamps if changed
          # fi <- activeRunningFileInfo(activeRunningPath = activeRunningPath, runNameLabel = runNameLabel, queue_path = queue_path)
          # pids <- Map(fiHere = rownames(fi), function(fiHere) {
          #   pid <- strsplit(fiHere, split = "_")[[1]][3] |> as.integer()
          #   alive <- is_pid_alive_tools(pid)
          #   if (isFALSE(alive)) {
          #     unlink(fiHere)
          #   }
          # })
          
          q$process_id[i] <- NA
          if (hasFI) {
            procId <- strsplit(basename(rownames(fi)), split = "_")[[1]][3]
            if (is_pid_alive_tools(as.integer(procId)) )
              new_status <- txtRunning
            
            if (!is.na(suppressWarnings(as.numeric(procId))))
              q$process_id[i] <- procId
            if (!is.character(hb$started) || is.na(hb$started)) {
              
              q$started_at[i] <- startedAt#format(fi$mtime, "%Y-%m-%d %H:%M:%S")
            } else {
              # elapsedTime <- difftime(fi$mtime, hb$started)
              
              if (elapsedTime > 0)
                q$started_at[i] <- format(as.POSIXct(hb$ts) - elapsedTime, "%Y-%m-%d %H:%M:%S")
              else
                q$started_at[i] <- startedAt#format(fi$mtime, "%Y-%m-%d %H:%M:%S")
            }
            if (isTRUE(is.na(q$machine_name[i])))
              q$machine_name[i] <- Sys.info()[["nodename"]]
          } else {
            
            # Says it is RUNNING --> but is actually INTERRUPTED because it has no log file (fi)
            if (isTRUE(is.na(q$started_at[i])))
              q$started_at[i] <- hb$started
            if (isTRUE(is.na(q$machine_name[i])))
              q$machine_name[i] <- Sys.info()[["nodename"]]
            
            new_status <- txtInterrupted
            if (q$heartbeat_iter[i] %% 25 %in% 0)
              new_status <- txtDone
            
          }
          
          q$heartbeat_iter[i] <- hb$iter
          q$heartbeat_at[i] <- hb$ts
          # dt <- try(format(round(difftime(q$heartbeat_at[i], q$started_at[i], units = "days"), 2), digits = 2))
          q$DEoptimElapsedTime[i] <- format(round(elapsedTime, 2), digits = 2, units = "days")
          if (new_status %in% txtDone) {
            q$finished_at[i] <- q$heartbeat_at[i]
            q$heartbeat_at[i] <- NA
            q$iterationsTotal[i] <- q$heartbeat_iter[i]
            q$heartbeat_iter[i] <- NA
            q$claimed_by[i] <- NA
          }
          
        }
      } else {
        new_status <- txtDone
      }

      if (!q$status[i] %in% new_status) {
        q$status[i] <- new_status
        # If newly finished, record current time
        # if (new_status  "FINISHED") {
        #   q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        # }
      }
      
    }
    
    #q <- data.table::as.data.table(q);
    # ord <- order(as.package_version(q[[runNameLabel]]))
    # q <- q[ord, ]
    # q1 <- data.table::rbindlist(list(q[status %in% txtDone], q[status %in% txtRunning]))
    # q2 <- q[!q1, on = runNameLabel][grep("^(5.|6.|14.|9.|12.)", .ELFind),]
    # q3 <- rbindlist(list(q1, q2))
    # q <- rbindlist(list(q3, q[!q3, on = runNameLabel]))
    
    #q <- as.data.frame(q)
    
    saveRDS(q, queue_path)
    filelock::unlock(lck)
    invisible(q)
  }
}

get_latest_heartbeat <- function(runName, 
                                 folderWithIterInFilename = getOption("spades.folderWithIterInFilename",
                                                                      quote(file.path("outputs", runName, "figures", "fireSense_SpreadFit", "hists")))) {
  retNames <- c("ts", "iter", "started", "elapsed")
  
  if (!is.null(folderWithIterInFilename)) {
    if (is.call(folderWithIterInFilename))
      folderWithIterInFilename <- eval(folderWithIterInFilename)
    
    retEarly <- list(ts = NA_character_, iter = NA_integer_)
    # folderWithIterInFilename <- file.path("outputs", runName, "figures", "hists")
    if (length(folderWithIterInFilename) == 0) return(retEarly)
    if (!dir.exists(folderWithIterInFilename))  {
      folderWithIterInFilename <- file.path("outputs", runName, "figures", "fireSense_SpreadFit", "hists")
      if (!dir.exists(folderWithIterInFilename))
        return(retEarly)
    }
    
    files <- list.files(folderWithIterInFilename, pattern = "iter", full.names = FALSE)
    if (length(files) == 0) return(list(ts = NA_character_, iter = NA_integer_))
    
    parse_one <- function(fn) {
      parts <- strsplit(fn, "_", fixed = TRUE)[[1]]
      ts <- if (length(parts) >= 1) parts[length(parts)] else NA_character_
      # Clean regex: no extra backslashes needed because of Raw String wrapper
      iter_idx <- which(grepl("iter\\d+", parts))
      iter <- if (length(iter_idx)) {
        as.integer(sub("iter(\\d+)", "\\1", parts[iter_idx[1]]))
      } else NA_integer_
      list(ts = ts, iter = iter)
    }
    
    parsed <- lapply(files, parse_one)
    a <- rbindlist(parsed) |> data.table::setorderv("iter")
    ts_vec <- vapply(parsed, function(x) x$ts, character(1))
    ts_vec <- fs::path_ext_remove(ts_vec)
    iter_vec <- vapply(parsed, function(x) x$iter, integer(1))
    ord <- order(ts_vec, na.last = TRUE)
    elapsed <- diff(as.POSIXct(ts_vec[ord]), units = "mins")
    elapsedTime <- sum(elapsed[elapsed < 20])
    units(elapsedTime) <- "days"
    ret <- list(ts = ts_vec[ord[length(ord)]], iter = iter_vec[ord[length(ord)]],
                started = head(ts_vec[ord], 1), elapsed = elapsedTime) |> setNames(retNames)
  } else {
    ret <- lapply(1:4, function(x) NA) |> setNames(retNames)
  }
  ret
}


activeRunningFileInfo <- function(activeRunningPath = getOption("spades.activeRunningPath"), pattern = txtRunning, queue_path, runName) {
  if (is.null(activeRunningPath))
    activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
  
  startedFiles <- dir(activeRunningPath, pattern = pattern, ignore.case = TRUE)
  if (length(startedFiles)) {
    startedFilesELFind <- sapply(startedFiles, function(x) strsplit(x, "_")[[1]][[2]])
    if (missing(runName) || is.null(runName) || all(!nzchar(runName))) {
      wh <- seq(NROW(startedFiles))
    } else {
      wh <- which(startedFilesELFind == runName)
    }
    startedFilesFull <- dir(activeRunningPath, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    fi <- file.info(startedFilesFull[wh])
  } else {
    fi <- NULL
  }
  fi
}

meta_cols <- c("status","claimed_by","started_at","finished_at",
               "DEoptimElapsedTime","machine_name","process_id",
               "heartbeat_at","heartbeat_iter","iterationsTotal")

is_pid_alive_tools <- function(pid) {
  stopifnot(length(pid) == 1L, is.numeric(pid), pid > 0)
  out <- tryCatch(
    tools::pskill(pid, signal = 0L),  # TRUE if PID exists & you have permission
    error = function(e) NA
  )
  isTRUE(out)
}



txtInterrupted <- "INTERRUPTED"
txtPending <- "PENDING"
txtDone <- "DONE"
txtRunning <- "RUNNING"

#' Log path for default tmux status
#' 
#' Just a default path.
#' 
#' @param activeRunningPath Optional character path. If `NULL` (default), derived from
#'   `prefix` and `queue_path`.
#' @param queue_path Character. Path to the queue `.rds` file, used to derive the default path.
#' @param prefix Character. Directory prefix for the path. Default `"logs"`.
#' @param suffix Character. Suffix used in the path. Defaults to `queue_path`.
#' @return The default path.
#' @export
activeRunningPathForTmux <- function(activeRunningPath = NULL, queue_path, prefix = "logs", suffix = queue_path) {
  if (is.null(activeRunningPath)) {
    if (missing(queue_path))
      suffix <- "tmuxStatus"
    activeRunningPath <- file.path(prefix, basename(suffix))
  }
  activeRunningPath
}


getRunName <- function(queue, i, runNameLabel) {
  queue[i, ..runNameLabel] |> paste(collapse = "-")
}



statusCalculator <- function(type = "fireSense") {
  
  if (identical(type, "fireSense")) {
    calc <- quote({
      dirWithUpdatedElf <- gsub("4.3", strsplit(runName, "-")[[1]][[1]], outputPath)
      dirWithUpdatedElf <- gsub("rep1", paste0("rep", strsplit(runName, "-")[[1]][[2]]), dirWithUpdatedElf)
      dd <- dir(dirWithUpdatedElf, recursive = TRUE, full.names = TRUE)
      ee <- grep(value = TRUE, pattern = "burnMap.*tif$", dd)
      done <- grepl(paste0("year", endTime), ee)
      if (done %in% FALSE) {
        runningFile <- dir(activeRunningPathForTmux(queue_path = queue_path), pattern = runName, full.names = TRUE)
        fi <- file.info(runningFile)
        started_at <- format(fi[, "mtime"])
        ff <- grep(value = TRUE, pattern = "Annual Fire Maps", dd)
        fi2 <- file.info(ff)
        mostRecentFile <- tail(ff[fi2[, "mtime"] > fi[, "mtime"]], 1)
        heartbeat_at <- if (length(mostRecentFile) > 0) format(file.info(mostRecentFile)[, "mtime"]) else NA
        heartbeat_iter <- gsub(".+Maps ([[:digit:]]{4,4}).+", "\\1", mostRecentFile)
      }
      finishedFile <- ee[done]
      if (length(finishedFile)) {
        iterationsTotal <- gsub(".+year([[:digit:]]{4,4}).+", "\\1", finishedFile)
        finished_at <- if (length(finishedFile) > 0) format(file.info(finishedFile)[, "mtime"]) else NA
        done <- any(done)
      }
    })
  }
 
  return(calc) 
}

revertDotNames <- function(q) {
  dotCols <- grep(dotTxt, names(q), value = TRUE)
  dotColsGsub <- gsub(dotTxt, ".", dotCols)
  data.table::setnames(q, old = dotCols, new = dotColsGsub)
  q[]
}

dotTxt <- "dot"