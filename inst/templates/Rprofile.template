## set CRAN repos; use binary linux packages if on Ubuntu
local({
    options(Ncpus = parallel::detectCores() / 2)
    options("repos" = c(CRAN = "https://cran.rstudio.com"))

    if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
        .os.version <- strsplit(system("lsb_release -c", intern = TRUE), ":\t")[[1]][[2]]
        .user.agent <- paste0(
            "R/", getRversion(), " R (",
            paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]),
            ")"
        )
        options(repos = c(CRAN = paste0("https://packagemanager.rstudio.com/all/__linux__/",
                                        .os.version, "/latest")))
        options(HTTPUserAgent = .user.agent)
    }
})

## package installation location
pkgDir <- file.path("packages", version$platform, paste0(version$major, ".",
                                                         strsplit(version$minor, "[.]")[[1]][1]))

if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
}
.libPaths(pkgDir)
