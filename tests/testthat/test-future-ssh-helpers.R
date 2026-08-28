## The ssh-facing helpers from R/future.R. Each is a thin wrapper around one
## system2("ssh", ...) call, so mocking system2 in the package namespace covers
## them without any remote host. What is under test is the command each builds
## and how it parses the reply.

## Capture the system2() call and return a canned reply.
mockSystem2 <- function(reply, capture = new.env(parent = emptyenv()), envir = parent.frame()) {
  testthat::local_mocked_bindings(
    system2 = function(command, args, ...) {
      capture$command <- command
      capture$args <- args
      if (is.function(reply)) reply(args) else reply
    },
    .package = "base",
    .env = envir
  )
  capture
}

# --- .ssh_pids_alive ----------------------------------------------------------

test_that(".ssh_pids_alive returns logical(0) for no pids", {
  expect_identical(SpaDES.project:::.ssh_pids_alive("host", integer(0)), logical(0))
})

test_that(".ssh_pids_alive maps alive/dead onto the pids in order", {
  cap <- mockSystem2(c("alive", "dead", "alive"))

  res <- SpaDES.project:::.ssh_pids_alive("node1", c(10, 11, 12))

  expect_identical(res, c(TRUE, FALSE, TRUE))
  expect_identical(cap$command, "ssh")
  expect_true("node1" %in% cap$args)
  # BatchMode keeps it from blocking on a password prompt
  expect_true(all(c("-o", "BatchMode=yes") %in% cap$args))
  expect_true(any(grepl("/proc/\\$pid", cap$args)))
})

test_that(".ssh_pids_alive returns NULL when the reply length does not match", {
  mockSystem2(c("alive"))

  # two pids asked about, one line back -- treated as unusable, not recycled
  expect_null(SpaDES.project:::.ssh_pids_alive("node1", c(1, 2)))
})

test_that(".ssh_pids_alive returns NULL when ssh errors", {
  mockSystem2(function(args) stop("ssh: connect failed"))

  expect_null(SpaDES.project:::.ssh_pids_alive("node1", 1))
})

# --- .ssh_pids_log_file -------------------------------------------------------

test_that(".ssh_pids_log_file returns an empty named vector for no pids", {
  res <- SpaDES.project:::.ssh_pids_log_file("host", integer(0))

  expect_identical(res, setNames(character(0), character(0)))
})

test_that(".ssh_pids_log_file parses the pid/path pairs", {
  mockSystem2(c("10\t/var/log/a.log", "11\t/var/log/b.log"))

  res <- SpaDES.project:::.ssh_pids_log_file("node1", c(10, 11))

  expect_identical(res[["10"]], "/var/log/a.log")
  expect_identical(res[["11"]], "/var/log/b.log")
})

test_that(".ssh_pids_log_file leaves a pid with no path as NA", {
  # readlink prints nothing for a pid whose fd/1 is not a file
  mockSystem2(c("10\t/var/log/a.log", "11\t"))

  res <- SpaDES.project:::.ssh_pids_log_file("node1", c(10, 11))

  expect_identical(res[["10"]], "/var/log/a.log")
  expect_true(is.na(res[["11"]]))
})

test_that(".ssh_pids_log_file keeps every requested pid as a name", {
  # only one of the two pids comes back
  mockSystem2(c("10\t/var/log/a.log"))

  res <- SpaDES.project:::.ssh_pids_log_file("node1", c(10, 11))

  expect_named(res, c("10", "11"))
  expect_true(is.na(res[["11"]]))
})

test_that(".ssh_pids_log_file returns NULL when ssh errors", {
  mockSystem2(function(args) stop("ssh: connect failed"))

  expect_null(SpaDES.project:::.ssh_pids_log_file("node1", 1))
})

# --- .ssh_kill_pids -----------------------------------------------------------

test_that(".ssh_kill_pids does nothing for no pids", {
  cap <- mockSystem2("")

  expect_null(SpaDES.project:::.ssh_kill_pids("node1", integer(0)))
  expect_null(cap$command)   # ssh was never invoked
})

test_that(".ssh_kill_pids sends the requested signal to every pid", {
  cap <- mockSystem2("")

  SpaDES.project:::.ssh_kill_pids("node1", c(21, 22), signal = "KILL")

  expect_identical(cap$command, "ssh")
  joined <- paste(cap$args, collapse = " ")
  expect_true(grepl("kill -KILL", joined))
  expect_true(grepl("21 22", joined))
  # failures are swallowed so one dead pid does not abort the rest
  expect_true(grepl("|| true", joined, fixed = TRUE))
})

test_that(".ssh_kill_pids defaults to TERM and survives an ssh error", {
  cap <- mockSystem2(function(args) stop("ssh: connect failed"))

  expect_no_error(SpaDES.project:::.ssh_kill_pids("node1", 21))
  expect_true(grepl("kill -TERM", paste(cap$args, collapse = " ")))
})

# --- .ef_build_host_map -------------------------------------------------------

test_that(".ef_build_host_map is empty for an empty ef list", {
  expect_identical(SpaDES.project:::.ef_build_host_map(NULL), character(0))
  expect_identical(SpaDES.project:::.ef_build_host_map(list()), character(0))
})

test_that(".ef_build_host_map skips local aliases", {
  cap <- mockSystem2("someremote")
  ef <- list(list(cores = c("localhost", "127.0.0.1", "mynode")))

  res <- SpaDES.project:::.ef_build_host_map(ef, local_node = "mynode")

  # every core was a local alias, so ssh is never called
  expect_identical(res, character(0))
  expect_null(cap$command)
})

test_that(".ef_build_host_map maps a reported hostname back to its core", {
  mockSystem2(c("node1short"))
  ef <- list(list(cores = c("localhost", "node1.example.com")))

  res <- SpaDES.project:::.ef_build_host_map(ef, local_node = "mynode")

  expect_identical(unname(res), "node1.example.com")
  expect_identical(names(res), "node1short")
})

test_that(".ef_build_host_map ignores a host that answers with nothing", {
  mockSystem2(character(0))
  ef <- list(list(cores = "unreachable.example.com"))

  res <- SpaDES.project:::.ef_build_host_map(ef, local_node = "mynode")

  expect_identical(res, character(0))
})

# --- .future_tmux_tail_cmd ----------------------------------------------------

test_that(".future_tmux_tail_cmd builds a single-pane session", {
  cmd <- SpaDES.project:::.future_tmux_tail_cmd("/var/log/one.log", session = "logs")

  expect_true(grepl("tmux new-session -d -s 'logs'", cmd))
  expect_true(grepl("tail -F", cmd))
  expect_true(grepl("/var/log/one.log", cmd))
  expect_true(grepl("select-layout -t 'logs' tiled", cmd))
  expect_true(grepl("attach -t 'logs'", cmd))
  # one log -> no split
  expect_false(grepl("split-window", cmd))
})

test_that(".future_tmux_tail_cmd splits one pane per extra log", {
  cmd <- SpaDES.project:::.future_tmux_tail_cmd(
    list("/l/a.log", "/l/b.log", "/l/c.log"), session = "s")

  expect_identical(lengths(regmatches(cmd, gregexpr("split-window", cmd)))[[1]], 2L)
  for (f in c("/l/a.log", "/l/b.log", "/l/c.log")) expect_true(grepl(f, cmd, fixed = TRUE))
  # the parts are chained as one shell command
  expect_true(grepl(" \\; ", cmd, fixed = TRUE))
})
