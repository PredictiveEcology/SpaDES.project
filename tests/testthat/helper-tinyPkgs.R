## Tiny local packages, installed with R CMD INSTALL; no network. Shared by test-devLibrary.R
## and test-setupSpaDES.ProjectDeps.R.

mkTinyPkg <- function(name, version, root = withr::local_tempdir(.local_envir = parent.frame())) {
  src <- file.path(root, paste0(name, "_", version), name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(paste0("Package: ", name), paste0("Version: ", version), "Title: Tiny",
               "Description: Tiny.", "License: GPL-3", "Authors@R: person('A', 'B', role = c('aut', 'cre'), email = 'a@b.c')"),
             file.path(src, "DESCRIPTION"))
  writeLines(paste0("export(f_", name, ")"), file.path(src, "NAMESPACE"))
  writeLines(sprintf("f_%s <- function() '%s'", name, version), file.path(src, "R", "f.R"))
  src
}

installTiny <- function(srcs, lib) {
  for (s in srcs) {
    st <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-docs", "--no-test-load", "-l", shQuote(lib), shQuote(s)),
                  stdout = FALSE, stderr = FALSE)
    if (st != 0L) stop("R CMD INSTALL failed for ", s)
  }
}
## Also the installFun given to newDevLibrary() (`packages` are source directories here). It is sent
## to another process, so it carries nothing of the test's environment.
environment(installTiny) <- globalenv()

inodeOf <- function(p) as.numeric(fs::file_info(p)$inode)
