#' Reproducibility receipt for Rmarkdown documents
#'
#' Insert git repository and R session info into Rmarkdown documents.
#' Based on suggestions in a Twitter thread by Miles McBain
#' (<https://twitter.com/MilesMcBain/status/1263272935197782016?s=20>).
#'
#' Add the following to your Rmd files:
#'
#' \verb{
#' -----
#'
#' ```{r details, echo=FALSE}
#' pemisc::reproducibilityReceipt()
#' ```
#' }
#'
#' @param title Header title for the inserted details section.
#'
#' @export
reproducibilityReceipt <- function(title = "Reproducibility receipt") {
  if (requireNamespace("details", quietly = TRUE)) {
    details::details({
      if (requireNamespace("git2r", quietly = TRUE)) {
        if (requireNamespace("here", quietly = TRUE)) {
          if (git2r::in_repository(".")) {
            gitinfo <- git2r::repository(here::here())
          } else {
            gitinfo <- NULL
          }
        }
      }

      if (requireNamespace("sessioninfo", quietly = TRUE)) {
        sessinfo <- sessioninfo::session_info()
      } else {
        sessinfo <- utils::sessionInfo()
      }

      .spatialPkgs <- paste("raster", "rgdal", "sf", "sp", "terra", sep = "|")
      if (any(grepl(.spatialPkgs, sessinfo))) {
        spatialLibs <- sf::sf_extSoftVersion()
      }

      timestamp <- Sys.time()

      list(`Git repository` = gitinfo, `External spatial libraries` = spatialLibs,
           `R session info` = sessinfo, `Timestamp` = timestamp)
    }, summary = title)
  } else {
    stop("Suggested packages 'details', 'git2r', and 'sessioninfo' are required.")
  }
}
