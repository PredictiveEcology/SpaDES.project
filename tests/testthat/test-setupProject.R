
if (interactive() && FALSE) {
  options(repos = c(PE = "https:/predictiveecology.r-universe.dev", CRAN = "https://cran.rstudio.com" ))
  prjPath <- file.path("~", "GitHub/SpaDES.project")
  pkgPath <- file.path(prjPath, "R"); dir.create(pkgPath, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgPath)
  if (!require("Require")) install.packages("Require")
  Require::Require("PredictiveEcology/SpaDES.project@transition")
  setupProject(name = "TestProject", paths = list(modulePath = "m"),
               modules = c("PredictiveEcology/Biomass_borealDataPrep@development")
  )
}



