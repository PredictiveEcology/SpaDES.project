#' This is copied from SpaDES.installl
splitGitRepo <- function(gitRepo) {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  grAcct <- strsplit(gitRepo, "/")[[1]] # only account and repo
  if (length(grAcct) == 1) {
    acct <- "PredictiveEcology"
    grSplit <- append(list(acct), grSplit)
  } else {
    acct <- grSplit[[1]]
  }
  repo <- grSplit[[2]]
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "main"
  }
  list(acct = acct, repo = repo, br = br)
}

# Copied from http://memosisland.blogspot.com/2012/03/check-url-existance-with-r.html
urlExists <- function(address) {
  tryCatch ({
    con <- url(address)
    browser()
    a  <- capture.output(suppressWarnings(readLines(con)))
    close(con)
    TRUE;
  },
  error = function(err) {
    occur <- grep("cannot open the connection", capture.output(err));
    if(length(occur) > 0) FALSE;
  })
}
