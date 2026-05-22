\dontrun{
  if (require("ggplot2", quietly = TRUE) &&
      require("NLMR", quietly = TRUE) &&
      require("RColorBrewer", quietly = TRUE)) {
    library(SpaDES.core)
    library(SpaDES.project)

    tmpdir <- file.path(tempdir(), "examples")
    # Make 3 simLists -- set up scenarios
    endTime <- 2

    # Example of changing parameter values
    # Make 3 simLists with some differences between them
    mySim <- lapply(c(10, 20, 30), function(nFires) {
      simInit(
        times = list(start = 0.0, end = endTime, timeunit = "year"),
        params = list(
          .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
          # Turn off interactive plotting
          fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(10)),
          caribouMovement = list(.plotInitialTime = NA),
          randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
        ),
        modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
        paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                     outputPath = tmpdir),
        # Save final state of landscape and caribou
        outputs = data.frame(
          objectName = c(rep("landscape", endTime), "caribou", "caribou"),
          saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime / 2), endTime))),
          stringsAsFactors = FALSE
        )
      )
    })

    planTypes <- c("sequential") # try others! ?future::plan
    sims <- experiment2(sim1 = mySim[[1]], sim2 = mySim[[2]], sim3 = mySim[[3]],
                        replicates = 3)

    # Try pulling out values from simulation experiments
    # 2 variables
    df1 <- as.data.table(sims, vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))))

    # Now use objects that were saved to disk at different times during spades call
    df1 <- as.data.table(sims,
                         vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))),
                         objectsFromOutputs = list(nPixelsBurned = NA, NCaribou = "caribou"))


    # now calculate 4 different values, some from data saved at different times
    # Define new function -- this calculates perimeter to area ratio
    fn <- quote({
      landscape$Fires[landscape$Fires[] == 0] <- NA;
      a <- boundaries(landscape$Fires, type = "inner");
      a[landscape$Fires[] > 0 & a[] == 1] <- landscape$Fires[landscape$Fires[] > 0 & a[] == 1];
      peri <- table(a[]);
      area <- table(landscape$Fires[]);
      keep <- match(names(area),names(peri));
      mean(peri[keep]/area)
    })

    df1 <- as.data.table(sims,
                         vals = c("nPixelsBurned",
                                  perimToArea = fn,
                                  meanFireSize = quote(mean(table(landscape$Fires[])[-1])),
                                  caribouPerHaFire = quote({
                                    NROW(caribou) /
                                      mean(table(landscape$Fires[])[-1])
                                  })),
                         objectsFromOutputs = list(NA, c("landscape"), c("landscape"),
                                                   c("landscape", "caribou")),
                         objectsFromSim = "nPixelsBurned")

    if (interactive()) {
      # with an unevaluated string
      library(ggplot2)
      p <- lapply(unique(df1$vals), function(var) {
        ggplot(df1[vals == var,],
               aes(x = saveTime, y = value, group = simList, color = simList)) +
          stat_summary(geom = "point", fun.y = mean) +
          stat_summary(geom = "line", fun.y = mean) +
          stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
          ylab(var)
      })

      # Arrange all 4 -- could use gridExtra::grid.arrange -- easier
      pushViewport(viewport(layout = grid.layout(2, 2)))
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      print(p[[1]], vp = vplayout(1, 1))
      print(p[[2]], vp = vplayout(1, 2))
      print(p[[3]], vp = vplayout(2, 1))
      print(p[[4]], vp = vplayout(2, 2))
    }
  }
}
