## .tmux_cluster_aliases() and tmuxSetPaneTitle() from R/tmux.R.
##
## .tmux_cluster_aliases() reads /etc/hosts and ~/.ssh/config; both are mocked
## through readLines/file.exists so the tests do not depend on this machine's
## configuration. tmuxSetPaneTitle()'s validation and socket-discovery errors
## are reachable without a tmux server.

# --- .tmux_cluster_aliases ----------------------------------------------------

## Serve canned /etc/hosts and ~/.ssh/config content.
mockHostFiles <- function(hosts = character(0), sshConfig = NULL,
                          envir = parent.frame()) {
  sshPath <- path.expand("~/.ssh/config")
  testthat::local_mocked_bindings(
    readLines = function(con, ...) {
      if (identical(con, "/etc/hosts")) return(hosts)
      if (identical(con, sshPath)) return(sshConfig %||% character(0))
      character(0)
    },
    file.exists = function(...) {
      p <- c(...)
      if (identical(p[[1]], sshPath)) return(!is.null(sshConfig))
      base::file.exists(...)
    },
    .package = "base",
    .env = envir
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that(".tmux_cluster_aliases collects short names from /etc/hosts", {
  mockHostFiles(hosts = c("132.156.148.169 mega mega.example.com",
                          "132.156.148.170 birds"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_true(all(c("mega", "birds") %in% res))
  # FQDNs are dropped, short names kept
  expect_false("mega.example.com" %in% res)
})

test_that(".tmux_cluster_aliases skips comments, blanks and loopback entries", {
  mockHostFiles(hosts = c("# a comment", "",
                          "127.0.0.1 localhost skipme",
                          "::1 ip6-localhost",
                          "10.0.0.1 realhost"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_true("realhost" %in% res)
  expect_false("localhost" %in% res)
  expect_false("skipme" %in% res)
  expect_false("ip6-localhost" %in% res)
})

test_that(".tmux_cluster_aliases reads Host entries from ~/.ssh/config", {
  mockHostFiles(hosts = character(0),
                sshConfig = c("Host mega", "  HostName 1.2.3.4",
                              "Host birds owls", "  User someone"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_true(all(c("mega", "birds", "owls") %in% res))
})

test_that(".tmux_cluster_aliases ignores wildcards, FQDNs and comments in ssh config", {
  mockHostFiles(hosts = character(0),
                sshConfig = c("Host *", "Host *.example.com",
                              "Host good # trailing comment",
                              "# Host commentedOut",
                              "  IdentityFile ~/.ssh/id_rsa"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_true("good" %in% res)
  expect_false(any(grepl("[*]", res)))
  expect_false("commentedOut" %in% res)
})

test_that(".tmux_cluster_aliases handles a CRLF ssh config", {
  # this machine's ~/.ssh/config has CRLF line endings; unstripped, the parsed
  # host name would carry a trailing \r and never match a real alias
  mockHostFiles(hosts = character(0),
                sshConfig = c("Host mega\r", "  HostName 1.2.3.4\r"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_true("mega" %in% res)
  expect_false(any(grepl("\r", res)))
})

test_that(".tmux_cluster_aliases returns unique names across both sources", {
  mockHostFiles(hosts = "10.0.0.1 mega",
                sshConfig = c("Host mega", "Host extra"))

  res <- SpaDES.project:::.tmux_cluster_aliases()

  expect_identical(sum(res == "mega"), 1L)
  expect_true("extra" %in% res)
})

# --- tmuxSetPaneTitle ---------------------------------------------------------

test_that("tmuxSetPaneTitle validates both titles", {
  expect_error(tmuxSetPaneTitle(1, "new"))
  expect_error(tmuxSetPaneTitle("old", ""))
  expect_error(tmuxSetPaneTitle(c("a", "b"), "new"))
  expect_error(tmuxSetPaneTitle("old", NULL))
})

test_that("tmuxSetPaneTitle short-circuits when the titles match", {
  msgs <- capture_messages(res <- tmuxSetPaneTitle("same", "same"))

  expect_true(any(grepl("identical", msgs)))
  expect_identical(res, character())
})

test_that("tmuxSetPaneTitle errors when the uid cannot be determined", {
  testthat::local_mocked_bindings(
    system2 = function(...) character(0),
    .package = "base"
  )

  expect_error(tmuxSetPaneTitle("old", "new"), "Could not determine uid")
})

test_that("tmuxSetPaneTitle errors when there is no socket directory", {
  td <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    system2 = function(...) "12345",
    .package = "base"
  )
  # point socket discovery at a directory with no tmux-<uid> inside
  withr::local_envvar(TMUX_TMPDIR = td)

  expect_error(tmuxSetPaneTitle("old", "new"), "No tmux socket directory")
})

test_that("tmuxSetPaneTitle errors when the socket directory is empty", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "tmux-12345"))
  testthat::local_mocked_bindings(
    system2 = function(...) "12345",
    .package = "base"
  )
  withr::local_envvar(TMUX_TMPDIR = td)

  expect_error(tmuxSetPaneTitle("old", "new"), "No tmux sockets found")
})

# --- localHostLabel -----------------------------------------------------------
#
# Resolves this machine's short name from its IPs, preferring /etc/hosts and
# falling back to ~/.ssh/config. `hostname -I`, both files, and their existence
# are mocked, so the result does not depend on the machine running the tests.

mockHostLookup <- function(ips = "10.0.0.1", hosts = character(0),
                           sshConfig = NULL, envir = parent.frame()) {
  sshPath <- path.expand("~/.ssh/config")
  testthat::local_mocked_bindings(
    system2 = function(command, ...) if (identical(command, "hostname")) ips else character(0),
    readLines = function(con, ...) {
      if (identical(con, "/etc/hosts")) return(hosts)
      if (identical(con, sshPath)) return(if (is.null(sshConfig)) character(0) else sshConfig)
      character(0)
    },
    file.exists = function(...) {
      p <- c(...)
      if (identical(p[[1]], sshPath)) return(!is.null(sshConfig))
      base::file.exists(...)
    },
    .package = "base",
    .env = envir
  )
}

test_that("localHostLabel prefers the shortest name in /etc/hosts", {
  mockHostLookup(ips = "132.156.148.169",
                 hosts = c("# comment",
                           "132.156.148.169 mega.example.com mega longername"))

  expect_identical(localHostLabel(), "mega")
})

test_that("localHostLabel matches the IP exactly, not as a prefix", {
  mockHostLookup(ips = "10.0.0.1",
                 hosts = c("10.0.0.10 wronghost", "10.0.0.1 righthost"))

  expect_identical(localHostLabel(), "righthost")
})

test_that("localHostLabel falls back to ~/.ssh/config HostName", {
  mockHostLookup(ips = "1.2.3.4", hosts = character(0),
                 sshConfig = c("Host mega", "  HostName 1.2.3.4",
                               "Host other", "  HostName 9.9.9.9"))

  expect_identical(localHostLabel(), "mega")
})

test_that("localHostLabel strips CRLF before matching the ssh config", {
  # ~/.ssh/config on this cluster has CRLF endings; unstripped, "1.2.3.4\r"
  # would never equal the IP and the lookup would silently fall through
  mockHostLookup(ips = "1.2.3.4", hosts = character(0),
                 sshConfig = c("Host mega\r", "  HostName 1.2.3.4\r"))

  expect_identical(localHostLabel(), "mega")
})

test_that("localHostLabel ignores comments and blanks in the ssh config", {
  mockHostLookup(ips = "1.2.3.4", hosts = character(0),
                 sshConfig = c("", "# Host decoy", "  ", "Host real",
                               "  HostName 1.2.3.4"))

  expect_identical(localHostLabel(), "real")
})
