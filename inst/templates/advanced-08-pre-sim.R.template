## pre-simulation preparation

do.call(SpaDES.core::setPaths, paths3)

times3 <- list(start = 0, end = endTime)

modules3 <- list()

objects3 <- list()

parameters3 <- list()

## RNG
fseed <- file.path(Paths$outputPath, "seed.rds")
fseed2 <- extension(fseed, "txt")
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e4, 1)
  saveRDS(seed, fseed)
}
print(paste("random seed:", seed))
cat(paste("Setting seed in newStart.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)
