# This is an example 'options' file that can be called from `setupOptions` and `setupProject`.
# Any named list will be appended to any subsequent named list, sequentially: if there
# are >1 named list with the same element, the final one will be used.
# There are several types of approaches that can be used, but because this file is used
# for "options" only, nothing will be returned to the user or functions; this should
# only be used to create named lists.

# If R packages are needed, it is likely wise to prefix the function with the package name;
# any package that is needed can be added to the `require` argument in `setupProject`.

# This file will have access to the arguments passed into `setupParams` and `setupProject`,
# such as `paths`, `times`, or any other named argument passed to the `...`.

# Example -- local object that can be defined and used below
maxMemory <- 5e+9 # if (grepl("LandWeb", runName)) 5e+12 else 5e+9

# Example -- Use any arbitrary object that can be passed in the `...` of `setupOptions`
#   or `setupProject`
if (.mode == "development") {
  list(test = 2)
}

# Can use
if (.mode == "batch") {
  list(test = 1)
}

if (nodes == "batch") {
  list(test = 3)
}


# Example -- large named list of options
opts <- list(
  "fftempdir" = paths$scratchPath,
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = normPath(paths$inputPath), # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = paths$tilePath,
  "map.maxNumCores" = 2,
  # "map.useParallel" = mapParallel,
  "rasterMaxMemory" = maxMemory,
  # "rasterTmpDir" = scratchDir,
  "reproducible.destinationPath" = normPath(paths$inputPath),
  "reproducible.futurePlan" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
  # "reproducible.useMemoise" = ifelse(isTRUE(batchMode), FALSE, if (user("emcintir")) FALSE else TRUE),
  "reproducible.useGDAL" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "spades.inputPath" = normPath(paths$outputPath),
  "spades.moduleCodeChecks" = FALSE,
  "spades.recoveryMode" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

opts <- list(
  "map.overwrite" = FALSE
)

# Example -- conditional using `if`. This example is "user" specific.
#   These statements must be simple as the parsing
#   cannot understand if this `if` is too complicated
if (user("emcintir"))
  list(
    "reproducible.showSimilar" = FALSE
  )

if (user("emcintir") && requireNamespace("Require")) {
  list(
    "reproducible.inputPaths" = Require::checkPath("~/data", create = TRUE),
    "reproducible.devMode" = TRUE
  )
}

# Example -- this will be ignored because it is not part of a named list --> pass this
#   to `sideEffects` in `setupProject`
if (requireNamespace("googledrive", quietly = TRUE))
  googledrive::drive_auth(email = "eliotmcintire@gmail.com", cache = "~/.secret")

# Example -- machine specific
if (machine("A127")) {
  list(Ncpus = 2)
}


# Example -- this will be ignored because it is not part of a named list --> pass this
#   to `sideEffects` in `setupProject`
if (requireNamespace("httr"))
  httr::set_config(httr::config(http_version = 0)) # not run b/c not named list

