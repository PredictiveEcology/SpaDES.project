validUrl <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

validUrlMemoise <- function(url, account, repo, t = 2) {
  cacheDir <- file.path(tools::R_user_dir("SpaDES.project", "cache"), account)
  cacheFile <- file.path(cacheDir, paste0(repo, ".rds"))
  if (file.exists(cacheFile)) {
    # remove after 1 day
    if ((file.info(cacheFile)$mtime + 60*60*24) > Sys.time()) {
      urlExists <- readRDS(cacheFile)
    } else {
      file.remove(cacheFile)
    }
  }
  if (!exists("urlExists", inherits = FALSE)) {
    urlExists <- validUrl(url, t)
    checkPath(cacheDir, create = TRUE)
    saveRDS(urlExists, cacheFile)
  }
  urlExists
}


#' Tools for examining modules on known repositories
#'
#' When exploring existing modules, these tools help identify and navigate modules
#' and their interdependencies.
#'
#' @return
#' `listModules` returns a named list of all SpaDES modules in the given repositories with
#' the `accounts` and `keywords` provided.
#'
#' @param keywords A vector of character strings that will be used as keywords for identify
#' modules
#'
#' @param accounts A vector of character strings identifying GitHub accounts e.g.,
#' `PredictiveEcology` to search.
#'
#' @param omit A vector of character strings of repositories to ignore.
#'
#' @param purge There is some internal caching that occurs. Setting this to `TRUE` will
#'   remove any cached data that is part of the requested `accounts` and `keywords`.
#' @param modules A named list of character strings of full module names, as returned
#'   by `listModules`.
#' @inheritParams Require::Require
#'
#' @rdname listModules
#' @export
#' @examples
#' library(reproducible)
#' mods <- Cache(listModules, c("Biomass", "WBI", "LandR", "fireSense", "CBM", "LandMine", "LandWeb",
#'                              "NRV", #"scfm",
#'                              "priority",
#'                              "dataPrep", "DataPrep", "RoF", "Ontario", "ROF"),
#'               accounts = c("PredictiveEcology", "ianmseddy", "achubaty", "FOR-CAST", "eliotmcintire",
#'                            "tati-micheletti"),
#'               cacheRepo = "LandR_Module_Cache")
#'
#' out <- Map(mod = mods, nam = names(mods), function(mod, nam) {
#'        out <- getModule(paste0(nam, "/", mod), modulePath = "testMods")
#'        out
#' })
#'
#' DT <- moduleDependencies(mods, modulePath = "testMods")
#' graph <- moduleDependenciesToGraph(DT)
#' vn <- PlotModuleGraph(graph)
#'
#' # get all the fireSense modules from the Predictive Ecology GitHub repository
#' Account <- "PredictiveEcology"
#' mods <- listModules("fireSense", Account)
#' out <- setupProject(modules = file.path(Account, mods[[Account]]),
#'                     paths = list(projectPath = "~/fireSense"))
listModules <- function(keywords, accounts, omit = c("fireSense_dataPrepFitRas"),
                        purge = FALSE,
                        verbose = getOption("Require.verbose", 1L)) {
  names(accounts) <- accounts
  outs <- lapply(accounts, function(account) {
    url <- paste0("https://api.github.com/users/",account,"/repos?per_page=200")
    names(url) <- account

    tf <- tempfile()
    download.file(url, destfile = tf)
    suppressWarnings(repos <- readLines(tf))
    repos <- unlist(strsplit(repos, ","))

    out <- lapply(keywords, function(mg) {
      messageVerbose("searching keyword: ", mg, " in ", account, verbose = verbose)
      if (grepl("PredictiveEcology", url) && mg == "scfm") browser()
      outs <- grep(mg, repos, value = TRUE)
      gitRepo <- grep("full_name", outs, value = TRUE)
      gitRepo <- strsplit(gitRepo, "\"")
      gitRepo <- grep(mg, unlist(gitRepo), value = TRUE)
      if (length(gitRepo)) {
        gitPaths <- paste0("https://github.com/",gitRepo,"/blob/master/",
                           basename(gitRepo),".R")
        isRepo <- unlist(
          Map(validUrlMemoise, url = gitPaths, repo = basename(gitRepo), MoreArgs = list(account = account) ))
        if (any(!isRepo)) {
          notRepo <- gitRepo[!isRepo]
          message("These are not SpaDES modules: ", paste(notRepo, collapse = ", "))
        }
        gitRepo <- gitRepo[isRepo]
        outs <- grep("\"name", outs, value = TRUE)
        outs <- strsplit(outs, "\"")
        outs <- unlist(outs)
        outs <- grep(mg, outs, value = TRUE)
        outs <- intersect(basename(gitRepo), outs)
      } else {
        outs <- gitRepo
      }
      outs

    })
    setdiff(unlist(out), omit)
  })
  outs
}


#'
#' @rdname listModules
#' @export
moduleDependencies <- function(modules, modulePath) {

  modsFlat <- unlist(modules)
  names(modsFlat) <- modsFlat
  if (!requireNamespace("SpaDES.core")) stop("Need SpaDES.core")
  obs <- lapply(modsFlat, function(mod) {
    io <- SpaDES.core::inputObjects(module = mod, path = modulePath)
    oo <- SpaDES.core::outputObjects(module = mod, path = modulePath)
    list(io = io[[mod]], oo = oo[[mod]], name = mod)
  })

  sim.in <- sim.out <- data.table(objectName = character(0),
                                  objectClass = character(0),
                                  module = character(0))

  lapply(obs, function(x) {
    if (!is.null(x)) {
      if (NROW(x$io)) {
        z.in <- as.data.table(x$io)[, .(objectName, objectClass)]
      } else {
        z.in <- data.table(objectName = character(), objectClass = character())
      }
      if (NROW(x$oo)) {
        z.out <- as.data.table(x$oo)[, .(objectName, objectClass)]
      } else {
        z.out <- data.table(objectName = character(), objectClass = character())
      }
      z.in$module <- z.out$module <- x$name
      if (!all(is.na(z.in[, objectName]), is.na(z.in[, objectClass]))) {
        sim.in <<- rbindlist(list(sim.in, z.in), use.names = TRUE)
      }
      if (!all(is.na(z.out[, 1:2]), is.na(z.out[, objectClass]))) {
        sim.out <<- rbindlist(list(sim.out, z.out), use.names = TRUE)
      }
    }
    return(invisible(NULL)) # return from the lapply
  })

  data.table::setkey(sim.in, "objectName")
  data.table::setkey(sim.out, "objectName")

  if ((nrow(sim.in)) && (nrow(sim.out))) {
    dx <- sim.out[sim.in, nomatch = NA_character_, allow.cartesian = TRUE]
    dx[is.na(module), module := "_INPUT_"]
    DT <- dx[, list(from = module, to = i.module,
                    objName = objectName, objClass = i.objectClass)]

  } else {
    DT <- data.table(from = character(0), to = character(0),
                     objName = character(0), objClass = character(0))
  }
  data.table::setorder(DT, "from", "to", "objName")
  DT <- DT[!grepl("INPUT", from)]
  return(DT)
}


#' @export
#' @rdname listModules
moduleDependenciesToGraph <- function(md) {
  mods <- unique(c(md$from, md$to))
  m <- unlist(mods)
  v <- unique(c(md$to, md$from, m)) # so no need to remove them
  if (requireNamespace("igraph"))
    graph <- igraph::graph_from_data_frame(md, vertices = v, directed = TRUE)
}


#' @export
#' @rdname listModules
PlotModuleGraph <- function(graph) {
  if (!requireNamespace("igraph") && !requireNamespace("dplyr") && !requireNamespace("visNetwork"))
    stop("need igraph, dplyr and visNetwork")
  browser()
  graph <- igraph::simplify(graph)

  names <- igraph::V(graph)$name
  groups <- ifelse(grepl("Biomass", names), "Biomass",
                   ifelse (grepl("fireSense", ignore.case = TRUE, names), "FireSense",
                           ifelse (grepl("CBM", ignore.case = TRUE, names), "CBM",
                                   ifelse (grepl("ROF", ignore.case = TRUE, names), "RoF", "Other"))))

  nodes <- data.frame(id = igraph::V(graph)$name, title = igraph::V(graph)$name, group = groups)
  nodes <- nodes[order(nodes$id, decreasing = F),]
  edges <- igraph::get.data.frame(graph, what="edges")[1:2]


  visNetwork::visNetwork(nodes, edges, width = "100%") %>%
    visNetwork::visIgraphLayout(layout = "layout_with_fr", type = "full") %>%
    visNetwork::visGroups(groupname = "Biomass", color = "orange",
              shadow = list(enabled = TRUE)) %>%
    # red triangle for group "B"
    visNetwork::visGroups(groupname = "FireSense", color = "red") %>%
    visNetwork::visGroups(groupname = "CBM", color = "green") %>%
    visNetwork::visGroups(groupname = "RoF", color = "lightgreen") %>%
    # visPhysics(repulsion = list(nodeDistance = 100)) %>%
    visNetwork::visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE,
               height="800px", width = "130%",
               #highlightNearest = list(enabled = T, degree = 1, hover = F),
               collapse = TRUE) %>%
    visNetwork::visInteraction(navigationButtons = TRUE)
}
