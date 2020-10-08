## cache database connection (requires reproducbile >= 1.0.0)
cacheDBconn <- if (config::get("cachedb") == "sqlite") {
  #DBI::dbConnect(drv = RSQLite::SQLite())
  NULL ## should default to sqlite
} else if (config::get("cachedb") == "postgresql") {
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = Sys.getenv("PGHOST"),
                 port = Sys.getenv("PGPORT"),
                 dbname = Sys.getenv("PGDATABASE"),
                 user = Sys.getenv("PGUSER"),
                 password = Sys.getenv("PGPASSWORD"))
} else {
  stop("Unsupported cache database type '", config::get("cachedb"), "'")
}

rep <- config::get("rep")

maxMemory <- 5e+9

rasterOptions(default = TRUE)
opts <- options(
  "fftempdir" = scratchDir,
  "future.globals.maxSize" = 1000*1024^2,
  "rasterMaxMemory" = maxMemory,
  "rasterTmpDir" = scratchDir,
  "reproducible.cachePath" = file.path(scratchDir, "cache"),
  "reproducible.cacheSaveFormat" = "rds", ## can be "qs" or "rds"
  "reproducible.conn" = cacheDBconn,
  "reproducible.destinationPath" = normPath(paths1$inputPath),
  "reproducible.futurePlan" = FALSE
  "reproducible.inputPaths" = NULL,
  "reproducible.nThreads" = 2,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: GDAL can be faster, but mixing GDAL with raster causes inconsistencies
  "reproducible.useMemoise" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "reproducible.useRequire" = FALSE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.nThreads" = 4,
  "spades.recoveryMode" = FALSE,
  "spades.restartR.restartDir" = paths3$outputPath,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

library(googledrive)

httr::set_config(httr::config(http_version = 0))

token <- NA_character_

if (is.na(token) || !file.exists(token))
  message(crayon::red("No Google service token found; authenticating with user token..."))

drive_auth(email = config::get("googleuser"), use_oob = quickPlot::isRstudioServer())

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
