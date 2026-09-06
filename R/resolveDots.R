## Resolution of `...` ("dot") arguments and `defaultDots` for setupProject()
## and the setup* helpers.
##
## Model: a single scope environment whose parent is the genuine caller. Only
## VALUES are ever assigned into it -- resolved dots, defaultDots entries the
## caller did not supply, and each formal once setupProject() has evaluated it.
## Every argument is evaluated once, in the order it was written, in that scope,
## so a later argument sees an earlier one by name exactly as in a script, and a
## dot's own name is not bound while its own expression runs (unless the caller
## defined it, or defaultDots pre-bound it).

## Is `nm` defined somewhere the *user* can see from `env`? Walks the caller's
## enclosure chain up to and including the global environment, skipping
## namespaces, imports, attached packages and base: a name that only resolves
## to a package function (`.mode`, `c`, `q`) is not a caller-supplied value.
.userVisible <- function(nm, env) {
  while (!identical(env, emptyenv())) {
    envName <- environmentName(env)
    skip <- isNamespace(env) || identical(env, baseenv()) ||
      grepl("^(package:|imports:)", envName)
    if (!skip && exists(nm, envir = env, inherits = FALSE)) return(TRUE)
    if (identical(env, globalenv())) break
    env <- parent.env(env)
  }
  FALSE
}

## Evaluate one expression in `scope`. Returns list(ok, value). On failure the
## error is recorded in the diagnostic scope under `context` (see
## R/diagnostics.R) and a slow failure is escalated by errorIfTooLong().
## Warnings raised while evaluating are collected into the same record rather
## than escaping mid-setup.
.evalInScope <- function(ex, context, scope) {
  if (!is.language(ex)) return(list(ok = TRUE, value = ex))
  stStart <- Sys.time()
  warns <- character()
  res <- withCallingHandlers(
    tryCatch(list(ok = TRUE, value = eval(ex, envir = scope)),
             error = function(e) list(ok = FALSE, error = e)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (isTRUE(res$ok)) {
    if (length(warns)) setupDiagRecord(context = context, expr = ex, warnings = warns)
    return(res)
  }
  msg <- conditionMessage(res$error)
  errorIfTooLong(stStart, ex, tryError = list(msg))
  setupDiagRecord(context = context, expr = ex, error = msg, warnings = warns)
  res
}

## The `defaultDots` entries as a named list of expressions. A literal
## `list(...)` call is kept unevaluated so an entry may reference another
## default or an earlier dot; anything else (a variable holding a list) is
## forced via `force`.
defaultDotExprs <- function(defaultDotsSUB, force) {
  if (is.call(defaultDotsSUB) && identical(defaultDotsSUB[[1L]], as.name("list")))
    return(as.list(defaultDotsSUB)[-1L])
  as.list(force())
}

## Bind, as values in `scope`, every default whose name the caller did not
## supply. Because this happens before any argument is evaluated, a default is
## available to every argument that references it -- a dot, or a formal such as
## `times = as.list(unlist(.times))`. Also bound into `envirCur` when given:
## several setup* helpers evaluate their formal with setupProject()'s own frame
## as the outermost fallback (`modules = unlist(.modules)` reaches setupModules()
## with `envir = envirCur`), so the defaults must be visible there too. Returns
## the names bound, invisibly.
bindDefaultDots <- function(exprs, scope, callerEnv, envirCur = NULL) {
  nms <- names(exprs)
  if (!length(exprs) || is.null(nms)) return(invisible(character()))
  bound <- character()
  for (i in seq_along(exprs)) {
    nm <- nms[[i]]
    if (!nzchar(nm) || .userVisible(nm, callerEnv)) next
    res <- .evalInScope(exprs[[i]], context = paste0("defaultDots$", nm), scope)
    if (!isTRUE(res$ok)) next
    assign(nm, res$value, envir = scope)
    if (!is.null(envirCur)) assign(nm, res$value, envir = envirCur)
    bound <- c(bound, nm)
  }
  invisible(bound)
}

## Resolve dots in order. Each is evaluated once in `scope`; the value (or, when
## it cannot be evaluated, the unevaluated expression -- see the setupProject()
## docs, "Can hard code arguments that may be missing") is published into
## `scope`, and into `envirCur` when given, before the next dot runs. NULL is a
## value and is kept under its name. Returns the named list of results.
resolveDots <- function(exprs, scope, envirCur = NULL) {
  out <- list()
  for (nm in names(exprs)) {
    ex <- exprs[[nm]]
    res <- .evalInScope(ex, context = nm, scope)
    val <- if (isTRUE(res$ok)) res$value else ex
    out[nm] <- list(val)
    assign(nm, val, envir = scope)
    if (!is.null(envirCur)) assign(nm, val, envir = envirCur)
  }
  out
}

## Copy already-evaluated bindings from `from` into `scope`, by name. Used by
## setupProject() to publish a formal (paths, modules, ...) once it exists.
publishToScope <- function(scope, from, nms) {
  for (nm in nms)
    if (exists(nm, envir = from, inherits = FALSE))
      assign(nm, get(nm, envir = from, inherits = FALSE), envir = scope)
  invisible(NULL)
}

## Entry point used by the setup* helpers on their own `...`, and directly by
## tests. Same contract as above with a scope built here: parent `callingEnv`,
## results published into `envir`.
evalDots <- function(dots, dotsSUB, defaultDots, envir = parent.frame(),
                     callingEnv = sys.frame(-2), dotsScope = FALSE) {
  exprs <- if (missing(dotsSUB)) list() else dotsSUB
  if (!missing(dots) && length(dots)) exprs <- append(dots, exprs)
  scope <- new.env(parent = callingEnv)
  if (!missing(defaultDots))
    bindDefaultDots(as.list(defaultDots), scope = scope, callerEnv = callingEnv, envirCur = envir)
  resolveDots(exprs, scope = scope, envirCur = envir)
}
