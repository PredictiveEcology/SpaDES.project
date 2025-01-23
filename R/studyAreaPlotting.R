areas <- function(x) {
  if (is(x, "SpatVector"))
    sum(terra::expanse(x))
  else if (is(x, "sf"))
    sum(sf::st_area(x))
  else
    NULL
}

plotSAs <- function(inSim, country = "CAN", saCols = c("purple", "blue", "green", "red"),
                    title = "Study Areas",
                    rasterToMatchLabel = "Stand Age", rasterToMatchPalette = "muted") {
  library(tidyterra)
  Canada <- {projectTo(SpaDES.project::setupStudyArea(studyArea = list(country = country)),
                       projectTo = inSim$studyArea) |>
      reproducible::postProcess(cropTo = inSim$studyAreaPSP)
  } |> Cache()
  p <- ggplot2::ggplot()
  rtms <- grep(names(inSim), pattern = "rasterToMatch", value = TRUE)
  sizesRtms <- sapply(inSim[rtms], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  sas <- grep(names(inSim), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sas, function(sa) areas(inSim[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  i <- 0
  subTitle <- character()
  for (rtm in inSim[rtms[min(ordRtms)]] ) {
    rtm[rtm[] == 0] <- NA
    p <- p + geom_spatraster(data = rtm)
    # p <- p + geom_spatvector(data = inSim[[sa]], fill = saCols[i]) #, aes(fill = tavg_04)) +
    subTitle <- c(subTitle, rtms[min(ordRtms)])
  }


  for (sa in rev(sas[ord])) {
    i <- i + 1
    p <- p + geom_spatvector(data = inSim[[sa]], fill = NA, col = saCols[i], lwd = 0.5) #, aes(fill = tavg_04)) +
    subTitle <- c(subTitle, paste0(sa, " (", saCols[i], ")"))
  }

  p <- p + geom_spatvector(data = Canada, fill = "NA")


  p +
    scale_fill_whitebox_c(
      na.value = "transparent",
      palette = rasterToMatchPalette
    ) +
    ggplot2::labs(
      fill = rasterToMatchLabel,
      title = title,
      subtitle = paste(subTitle, collapse = ", ")
    ) +
    ggplot2::theme_bw()
}

plotSAsLeaflet <- function(inSim, saCols = c("purple", "blue", "green", "red"),
                           title = "Study Areas",
                           rasterToMatchLabel = "Stand Age", rasterToMatchPalette = "muted") {
  rtmsNames <- grep(names(inSim), pattern = "rasterToMatch", value = TRUE)
  sizesRtms <- sapply(inSim[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  sas <- grep(names(inSim), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sas, function(sa) areas(inSim[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  i <- 0
  subTitle <- character()
  largest <- inSim[rtmsNames[max(ordRtms)]]
  names(largest[[1]]) <- rasterToMatchLabel
  stk <- largest

  for (rtmName in rtmsNames[-max(ordRtms)] ) {
    # rtm[rtm[] == 0] <- NA
    rtm <- terra::extend(inSim[[rtmName]], largest[[1]])
    names(rtm) <- rasterToMatchLabel
    stk <- append(list(rtm) |> setNames(rtmName), stk)
  }
  rtms <- terra::rast(stk)


  rtms <- terra::project(rtms, "epsg:4326") |> Cache()
  names(rtms) <- paste0(names(rtms), "_", rasterToMatchLabel)
  a <- terra::plet(rtms, seq_len(nlyr(rtms)), collapse = FALSE)#, maxcell=5000000)
  subTitle <- c(subTitle, rtmsNames[min(ordRtms)])

  v <- list()
  for (sa in rev(sas[ord])) {
    i <- i + 1
    if (!is(inSim[[sa]], "SpatVector")) {
      vv <- terra::vect(inSim[[sa]])
    } else {
      vv <- inSim[[sa]]
    }
    a <- a |> polys(vv, fill = 0, border = saCols[i], col = saCols[i], alpha = 1) #|>
  }
  a
}
