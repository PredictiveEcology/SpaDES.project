## Helpers for the dataInventory() and checkProject() tests.
##
## Write a minimal module `name` under `mp`. `inputs` is a character vector of
## expectsInput(...) calls, `outputs` of createsOutput(...) calls, `code` the text of the
## module's own functions (after the template), `children` its childModules, `params` defineParameter(...) calls.
diWriteModule <- function(mp, name, inputs = character(), outputs = character(),
                          code = "", children = character(), params = character()) {
  dir.create(file.path(mp, name), recursive = TRUE, showWarnings = FALSE)
  bind <- function(x) if (length(x)) paste0("bindrows(", paste(x, collapse = ",\n"), ")") else "bindrows()"
  txt <- c(
    "defineModule(sim, list(",
    sprintf("  name = \"%s\", description = \"d\", keywords = \"k\",", name),
    "  authors = person(\"A\", \"B\", email = \"a@b.c\", role = c(\"aut\", \"cre\")),",
    sprintf("  childModules = %s,", if (length(children)) deparse(children) else "character(0)"),
    "  version = list(x = \"0.0.1\"), timeframe = as.POSIXlt(c(NA, NA)),",
    "  timeunit = \"year\", citation = list(), documentation = list(), reqdPkgs = list(),",
    sprintf("  parameters = bindrows(%s),",
            paste(c("defineParameter(\".useCache\", \"character\", \".inputObjects\", NA, NA, \"d\")", params),
                  collapse = ",\n")),
    sprintf("  inputObjects = %s,", bind(inputs)),
    sprintf("  outputObjects = %s", bind(outputs)),
    "))",
    code)
  writeLines(txt, file.path(mp, name, paste0(name, ".R")))
  invisible(file.path(mp, name, paste0(name, ".R")))
}

diInput <- function(obj, url) {
  sprintf("expectsInput(\"%s\", \"character\", \"d\", sourceURL = %s)", obj,
          if (is.na(url)) "NA" else dQuote(url, FALSE))
}
diOutput <- function(obj) sprintf("createsOutput(\"%s\", \"character\", \"d\")", obj)

diFakeSim <- function(records) {
  testthat::skip_if(is.null(utils::getS3method("urlLog", "simList", optional = TRUE,
                                               envir = asNamespace("reproducible"))),
                    "SpaDES.core without a urlLog() method for a simList")
  sim <- methods::new(methods::getClass("simList", where = asNamespace("SpaDES.core")))
  log <- new.env(parent = emptyenv())
  log$records <- records
  SpaDES.core::envir(sim)$._urlLog <- log
  sim
}


## A bare repository <td>/<repo>.git with one module, <repo>/<repo>.R, on branch `br`.
mkBareModule <- function(td, repo, br, childModules = character(), reqdPkgs = '"fs"') {
  bare <- file.path(td, paste0(repo, ".git"))
  gert::git_init(bare, bare = TRUE)
  writeLines(paste0("ref: refs/heads/", br), file.path(bare, "HEAD"))
  seed <- file.path(td, paste0(repo, "-seed"))
  dir.create(seed)
  gert::git_init(seed)
  writeLines(paste0("ref: refs/heads/", br), file.path(seed, ".git", "HEAD"))
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', repo),
      sprintf('  version = list(%s = "1.0.0"),', repo),
      sprintf("  childModules = %s,", paste(deparse(childModules), collapse = "")),
      sprintf("  reqdPkgs = list(%s)", reqdPkgs),
      "))"),
    file.path(seed, paste0(repo, ".R")))
  sig <- gert::git_signature("test", "test@example.com")
  gert::git_add(paste0(repo, ".R"), repo = seed)
  gert::git_commit("init", author = sig, repo = seed)
  gert::git_remote_add(url = bare, name = "origin", repo = seed)
  gert::git_push("origin", refspec = paste0("refs/heads/", br), repo = seed, verbose = FALSE)
  bare
}

## A module `name` at `version` (its `defineModule()` only) in `modulePath`.
mkVersionedModule <- function(modulePath, name, version = "1.0.0") {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      sprintf('  version = list(%s = "%s")', name, version),
      "))"),
    file.path(modulePath, name, paste0(name, ".R"))
  )
}
