#' Plot studyArea** and rasterToMatch** with ggplot2 or leaflet
#'
#' Plot all studyArea** and rasterToMatch** objects within a list-like object.
#'
#' @return Run primarily for side effects. `plotSAs` plots (and returns) a `ggplot2` object.
#'   `plotSAsLeaflet` creates a leaflet page in a viewer (if using Rstudio).
#'
#' @export
#' @param ll Any list-like object with named elements. Names must include at least
#'   one that starts with `studyArea` or `rasterToMatch`. Thus any of the permutations
#'   like `studyAreaLarge` or `rasterToMatchPSP` all are fine.
#' @param ... Any objects to plot. Currently, they must be named arguments, and they must
#'   have prefixes `studyArea` or `rasterToMatch` to be visualized.
#' @param saCols A vector of same length as number of `studyArea**` objects, that defines
#'   the studyArea polygon boundary colours. These will be used in sequence from largest
#'   to smallest in polygon area.
#' @param title The main title for the ggplot2 object. Defaults to one or both of
#'   "studyArea" and "rasterToMatch" or their plurals.
#' @param rasterToMatchLabel Used in rasterToMatch legend
#' @param rasterToMatchPalette A palette to be used for colour scheme in rasterToMatch plotting.
#'   Can be any that work with `tidyterra::whitebox.colors`.
#' @param country The country for jurisdiction boundaries; defaults to "CAN". Passed to
#'   `geodata::gadm`
#' @param latlong Logical. Should all layers be converted to `latlong` for `plotSAs` prior
#'   to plotting. This means that "North will be up"; this could be slow for large rasters.
#'   This happens by default with `plotSAsLeaflet` and can't be turned off.
#' @param minArea In m^2. This is the minimium area for the entire plot. If this is too
#'   small then the legislative boundaries may not appear. The area covered by the plot
#'   will the maximum of the studyArea** or rasterToMatch** and this `minArea` value.
plotSAs <- function(ll, ..., saCols = c("purple", "blue", "green", "red"),
                    title,
                    rasterToMatchLabel = "Stand Age", rasterToMatchPalette = "muted",
                    country = "CAN", latlong = FALSE,
                    minArea = 7e11) {
  library(tidyterra)
  dots <- list(...)
  if (length(dots))
    ll <- appendDotsToLL(ll, dots)

  sasNames <- grep(names(ll), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sasNames, function(sa) areas(ll[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])
  biggestSA <- names(which.max(sizesOrdered))

  # in canada, need it to be a certain size, in most areas, to see jurisdiction boundaries
  cropTo <- ll[[biggestSA]]
  minArea <- 7e11
  if (max(sizes) < minArea) {
    factorToEnlarge <- minArea/max(sizes)
    # pi*r^2 calculations
    factorToWidth <- sqrt(max(sizes)/pi)*sqrt(factorToEnlarge) - sqrt(max(sizes)/pi)
    cropTo <- terra::buffer(ll[[biggestSA]], width = factorToWidth)
  }

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  rtmsNames <- grep(names(ll), pattern = "rasterToMatch", value = TRUE)

  if (isTRUE(latlong)) {
    ll <- toLatLong(ll, rtmsNames, sasNames)
    # projectTo <- "epsg:4326"
    # if (length(rtmsNames)) {
    #   ll[rtmsNames] <- Map(rtm = rtmsNames, function(rtm)
    #     postProcessTo(ll[[rtm]], projectTo = projectTo))
    # }
    # if (length(sasNames)) {
    #   ll[sasNames] <- Map(sa = sasNames, function(sa)
    #     postProcessTo(ll[[sa]], projectTo = projectTo))
    # }
  } else {
    projectTo <- ll$studyArea
  }

  Canada <- {
    SpaDES.project::setupStudyArea(studyArea = list(country = country)) |>
      postProcessTo(projectTo = projectTo,
                    cropTo = cropTo) #|>
  } |> Cache()
  p <- ggplot2::ggplot()
  sizesRtms <- sapply(ll[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)




  i <- 0
  subTitle <- character()
  if (length(rtmsNames) > 0)
    for (rtm in ll[rtmsNames[min(ordRtms)]] ) {
      rtm[rtm[] == 0] <- NA
      p <- p + geom_spatraster(data = rtm)
      # p <- p + geom_spatvector(data = ll[[sa]], fill = saCols[i]) #, aes(fill = tavg_04)) +
      subTitle <- c(subTitle, rtmsNames[min(ordRtms)])
    }

  if (length(sasNames) > 0)
    for (sa in rev(sasNames[ord])) {
      i <- i + 1
      p <- p + geom_spatvector(data = ll[[sa]], fill = NA, col = saCols[i], lwd = 0.5) #, aes(fill = tavg_04)) +
      subTitle <- c(subTitle, paste0(sa, " (", saCols[i], ")"))
    }

  p <- p + geom_spatvector(data = Canada, fill = "NA")

  if (missing(title)) {
    titleSA <- if (length(sasNames) == 0) character() else
      Require:::singularPlural(c("studyArea", "studyAreas"), l = sasNames)
    titleRTM <- if (length(rtmsNames) == 0) character() else
      Require:::singularPlural(c("rasterToMatch", "rasterToMatches"), l = rtmsNames)
    title <- paste0(ifelse(length(titleSA), titleSA, ""),
                    ifelse(length(titleSA) && length(titleRTM), " and ", ""),
                    ifelse(length(titleRTM), titleRTM, ""))

  }
  p <- p +
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
  p
}

#' @rdname plotSAs
#' @export
plotSAsLeaflet <- function(ll, ..., saCols = c("purple", "blue", "green", "red"),
                           title = "Study Areas",
                           rasterToMatchLabel = "Stand Age",
                           rasterToMatchPalette = "muted") {

  dots <- list(...)
  if (length(dots))
    ll <- appendDotsToLL(ll, dots)

  rtmsNames <- grep(names(ll), pattern = "rasterToMatch", value = TRUE)
  sizesRtms <- sapply(ll[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  sasNames <- grep(names(ll), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sasNames, function(sa) areas(ll[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  i <- 0
  subTitle <- character()

  if (length(rtmsNames) > 0) {
    largest <- ll[rtmsNames[max(ordRtms)]]
    names(largest[[1]]) <- rasterToMatchLabel
    stk <- largest

    for (rtmName in rtmsNames[-max(ordRtms)] ) {
      # rtm[rtm[] == 0] <- NA
      rtm <- terra::extend(ll[[rtmName]], largest[[1]])
      names(rtm) <- rasterToMatchLabel
      stk <- append(list(rtm) |> setNames(rtmName), stk)
    }
    rtms <- terra::rast(stk)


    rtms <- terra::project(rtms, "epsg:4326") |> Cache()
    names(rtms) <- paste0(names(rtms), "_", rasterToMatchLabel)
    a <- terra::plet(rtms, seq_len(terra::nlyr(rtms)), collapse = FALSE)#, maxcell=5000000)
    subTitle <- c(subTitle, rtmsNames[min(ordRtms)])
  }

  ll <- toLatLong(ll, rtmsNames, sasNames) |> Cache()

  v <- list()

  if (!exists("a", inherits = FALSE)) {
    a <- leaflet() |>
      addTiles() #|>
  }

  if (length(sasNames) > 0) {
    for (sa in rev(sasNames[ord])) {
      i <- i + 1
      if (!is(ll[[sa]], "SpatVector")) {
        vv <- terra::vect(ll[[sa]])
      } else {
        vv <- ll[[sa]]
      }

      # browser()
      # labels <- sprintf(
      #   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      #   states$name, states$density
      # ) %>% lapply(htmltools::HTML)

      a <- a |> leaflet::addPolygons(data=vv, weight = 3,
                            label = ~paste0(sa),
                            fillColor = saCols[i], color = saCols[i],
                            fillOpacity = 0,
                            highlight = leaflet::highlightOptions(weight = 10#,
                                                         # fillOpacity = 0.7
                                                         # color = "blue",
                                                         # bringToFront = TRUE
                                                         ))

      # browser()
      # args <- list(fill = 0, border = saCols[i], col = saCols[i], alpha = 1, label = TRUE)
      # if (!exists("a", inherits = FALSE)) {
      #   a <- do.call(terra::plet, append(list(vv), args))
      # } else {
      #   a <- list(a, vv) |> append(args) |> do.call(terra::polys, args = _)
      #   # a <- a |> terra::polys(vv, fill = 0, border = saCols[i], col = saCols[i], alpha = 1) #|>
      # }
    }
  }
  a
}

areas <- function(x) {
  if (is(x, "SpatVector"))
    sum(terra::expanse(x))
  else if (is(x, "sf"))
    sum(sf::st_area(x))
  else
    NULL
}

appendDotsToLL <- function(ll, dots) {
  keepers <- grep("studyArea|rasterToMatch", names(dots), value = TRUE)
  if (missing(ll)) {
    ll <- dots[keepers]
  } else {
    ll <- append(ll, dots[keepers])
  }
}

toLatLong <- function(ll, rtmsNames, sasNames) {
  projectTo <- "epsg:4326"
  if (length(rtmsNames)) {
    ll[rtmsNames] <- Map(rtm = rtmsNames, function(rtm)
      postProcessTo(ll[[rtm]], projectTo = projectTo))
  }
  if (length(sasNames)) {
    ll[sasNames] <- Map(sa = sasNames, function(sa)
      postProcessTo(ll[[sa]], projectTo = projectTo))
  }
}
