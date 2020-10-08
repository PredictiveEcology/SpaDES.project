## run additional data prep modules

do.call(SpaDES.core::setPaths, paths2)

modules2 <- list()

objects2 <- list()

parameters2 <- list()

simOutDataPrep <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters2,
                        modules = modules2,
                        objects = objects2,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = if (isTRUE(rerunSpeciesLayers)) "overwrite" else TRUE,
                        useCloud = useCloudCache,
                        cloudFolderID = cloudCacheFolderID,
                       .plotInitialTime = .plotInitialTime,
                        paths = paths2,
                        debug = 1)
