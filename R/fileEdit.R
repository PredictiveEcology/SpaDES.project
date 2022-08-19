## !!
## !! copied from SpaDES.core; do not edit without also editing there
## !!

#' Open a file for editing
#'
#' RStudio's `file.edit` behaves differently than `utils::file.edit`.
#' The workaround is to have the user manually open the file if they are using
#' RStudio, as suggested in the RStudio support ticket at
#' <https://support.rstudio.com/hc/en-us/community/posts/206011308-file-edit-vs-utils-file-edit>.
#'
#' @param file  Character string giving the file path to open.
#'
#' @return  Invoked for its side effect of opening a file for editing.
#'
#' @author Alex Chubaty
#' @importFrom utils file.edit
#' @keywords internal
#' @rdname fileEdit
#'
.fileEdit <- function(file) {
  if (Sys.getenv("RSTUDIO") == "1") {
    file <- gsub(file, pattern = "\\./", replacement = "")
    if (requireNamespace("rstudioapi")) {
      rstudioapi::navigateToFile(file)
    } else {
      message("Using RStudio, open file manually with:\n",
              paste0("file.edit('", file, "')")
      )
    }

  } else {
    file.edit(file)
  }
}
