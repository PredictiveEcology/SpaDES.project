utils::globalVariables(c(
  c("ordRtms", "ordSas", "rtmsNames", "sasNames", "sizes")
))


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
#' @param include Either logical or a character vector. If logical, this indicates whether all maps in the `ll` object should
#'   be plotted (if `TRUE`) or, if `FALSE`, no extra maps (on top of the defaults listed in
#'   `ll` argument description. If a character vector, then the objects indicated will also
#'   be plotted. Default is `FALSE` to prevent inadvertent (slow) plotting of
#'   potentially many layers.
#' @param exclude A character vector of spatial objects contained within `ll` to exclude
#'   from plotting. This is run after `include`, so it will override any named objects
#'   specified in `include`.
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
#' @importFrom grDevices colorRampPalette
plotSAs <- function(ll, ..., include = TRUE, exclude, saCols = c("purple", "blue", "green", "red"),
                    title,
                    rasterToMatchLabel = "Stand Age", rasterToMatchPalette = c("Set1", "Set2", "Set3"),
                    country = "CAN", latlong = FALSE,
                    minArea = 7e11) {

  pkgs <- c("tidyterra", "reproducible", "terra", "RColorBrewer",
            "sf", "patchwork", "ggplot2")
  requireNamespaces(pkgs)

  llPlus <- makeListToPlot(ll, include, exclude, ...)
  list2env(llPlus, envir = environment()) # ll, sizes, sasNames, rtmsNames, ordRtms, ordSas
  if (!(length(sasNames) || length(rtmsNames))) {
    stop("No spatial objects to plot")
  }

  if (length(sasNames)) {
    biggestSA <- names(which.max(ordSas))

    # in canada, need it to be a certain size, in most areas, to see jurisdiction boundaries
    cropTo <- ll[[biggestSA]]
    minArea <- 7e11
    if (max(sizes) < minArea) {
      factorToEnlarge <- minArea/max(sizes)
      # pi*r^2 calculations
      factorToWidth <- sqrt(max(sizes)/pi)*sqrt(factorToEnlarge) - sqrt(max(sizes)/pi)
      cropTo <- buffs(ll[[biggestSA]], width = factorToWidth)
    }
    saCols <- saCols[cumsum(!duplicated(ordSas))]
  } else { # doesn't have any polygons
    cropTo <- ll[[1]]
  }

  if (isTRUE(latlong)) {
    ll <- toLatLong(ll, rtmsNames, sasNames) # |> reproducible::Cache()
    projectTo <- "epsg:4326"
  } else {
    projectTo <- if (!is.null(ll[["studyArea"]]))
      ll$studyArea
    else
      ll[[1]]
  }

  Canada <- {
    SpaDES.project::setupStudyArea(studyArea = list(country = country)) |>
      reproducible::postProcessTo(projectTo = projectTo,
                    cropTo = cropTo) #|>
  } |> reproducible::Cache()
  p <- ggplot2::ggplot()
  g <- list()
  sizesRtms <- sapply(ll[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  subTitle <- list()
  if (length(rtmsNames) > 0) {
    for (rtmNam in rtmsNames ) {
      rtm <- ll[[rtmNam]]
      rtm[rtm[] == 0] <- NA
      g[[rtmNam]] <- p + tidyterra::geom_spatraster(data = rtm)
      subTitle[[rtmNam]] <- rtmNam
    }

    if (missing(title)) {
      titleSA <- if (length(sasNames) == 0) character() else
        singularPlural(c("studyArea", "studyAreas"), l = sasNames)
      titleRTM <- if (length(rtmsNames) == 0) character() else
        singularPlural(c("rasterToMatch", "rasterToMatches"), l = rtmsNames)
      title <- paste0(ifelse(length(titleSA), titleSA, ""),
                      ifelse(length(titleSA) && length(titleRTM), " and ", ""),
                      ifelse(length(titleRTM), titleRTM, ""))

    }

    rasterToMatchPaletteNamed <- rasterToMatchPaletteNamed(rasterToMatchPalette)
    rasterToMatchPalette <- rasterToMatchPaletteUpdate(rasterToMatchPalette, rtmsNames)

    for (rtmNam in rtmsNames) {
      g[[rtmNam]] <- g[[rtmNam]] + tidyterra::geom_spatvector(data = Canada, fill = "NA")
      paletteThisRas <- if (rtmNam %in% names(rasterToMatchPaletteNamed))
        rasterToMatchPaletteNamed[rtmNam] else rasterToMatchPalette[[1]]


      isWhitebox <- paletteThisRas %in% WhiteboxCols
      isRColBrew <- paletteThisRas %in% rownames(RColorBrewer::brewer.pal.info)

      if (isRColBrew) {
        theColFun <- RColorBrewer::brewer.pal(9, paletteThisRas) |>
          colorRampPalette()

        g[[rtmNam]] <- g[[rtmNam]] +
          ggplot2::scale_fill_gradientn(name = rtmNam,
                                        na.value = "transparent",
                                        colours = theColFun(20))

      } else {
        g[[rtmNam]] <- g[[rtmNam]] +
          tidyterra::scale_fill_whitebox_c(
            na.value = "transparent",
            palette = paletteThisRas
          )
      }
    }
  }

  if (length(rtmsNames) > 0) {
    for (rtmNam in rtmsNames ) {
      if (length(sasNames) > 0) {
        i <- 0
        for (sa in names(ordSas)) {
          i <- i + 1
          g[[rtmNam]] <- g[[rtmNam]] + tidyterra::geom_spatvector(data = ll[[sa]], fill = NA, col = saCols[i], lwd = 0.5) #, aes(fill = tavg_04)) +
          subTitle[[rtmNam]] <- c(subTitle[[rtmNam]], paste0(sa, " (", saCols[i], ")"))
        }
      }
      g[[rtmNam]] <- g[[rtmNam]] +
        ggplot2::labs(
          fill = rtmNam,
          # title = title,
          subtitle = paste(subTitle[[rtmNam]], collapse = ", ")
        ) +
        ggplot2::theme_bw()

    }
  }

  gg <- patchwork::wrap_plots(g)
  gg <- gg + patchwork::plot_annotation(
    title = title,
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14)))
  gg
}

#' @rdname plotSAs
#' @export
plotSAsLeaflet <- function(ll, ..., include = TRUE, exclude, saCols = c("purple", "blue", "green", "red"),
                           title = "Study Areas",
                           rasterToMatchLabel = "Stand Age",
                           rasterToMatchPalette = c("Set1", "Set2", "Set3")) {
  pkgs <- c("leaflet", "leafem", "tidyterra", "reproducible", "sf", "terra", "RColorBrewer")
  requireNamespaces(pkgs)

  llPlus <- makeListToPlot(ll, include, exclude, ...)
  list2env(llPlus, envir = environment()) # ll, sizes, sasNames, rtmsNames, ordRtms, ordSas
  if (!(length(sasNames) || length(rtmsNames))) {
    stop("No spatial objects to plot")
  }

  saCols <- saCols[cumsum(!duplicated(ordSas))]

  subTitle <- character()

  if (length(rtmsNames) > 0) {
    largest <- ll[rtmsNames[max(ordRtms)]]
    names(largest[[1]]) <- rasterToMatchLabel
    stk <- largest

    # geoTiffFile <- Map(ras = ll[rtmsNames], nam = rtmsNames, function(ras, nam) {
    #   tf <- tempfile(fileext = ".tif")
    #   terra::writeRaster(ras, filename = tf)
    #   tf
    # })

    namsRTMP <- names(rasterToMatchPalette)
    rasterToMatchPaletteNamed <- rasterToMatchPaletteNamed(rasterToMatchPalette)
    rasterToMatchPalette <- rasterToMatchPaletteUpdate(rasterToMatchPalette, rtmsNames)

    for (rasFileIndex in seq_along(rtmsNames)) {
      rtmNam <- rtmsNames[rasFileIndex]

      geoTiffFile <- tempfile(fileext = ".tif")
      terra::writeRaster(ll[[rtmNam]], filename = geoTiffFile)

      if (!exists("a", inherits = FALSE)) {
        a <- terra::plet() |> leaflet::addTiles()
      }
      paletteThisRas <- if (rtmNam %in% namsRTMP)
        rasterToMatchPaletteNamed[rtmNam] else rasterToMatchPalette[[rasFileIndex]]
      isWhitebox <- rasterToMatchPalette[[rasFileIndex]] %in% WhiteboxCols
      isRColBrew <- rasterToMatchPalette[[rasFileIndex]] %in% rownames(RColorBrewer::brewer.pal.info)
      if (isWhitebox) {
        pal <- tidyterra::whitebox.colors(n = 37, palette = paletteThisRas,
                                          alpha = 1, rev = FALSE)
      } else {
        pal <- paletteThisRas
      }

      a <- leafem::addGeotiff(a, geoTiffFile,
                              group = rtmNam,
                              layerId = rtmNam,
                              colorOptions = leafem::colorOptions(
                                palette = unname(pal), # "Set1", #hcl.colors(256, palette = "inferno")
                                , na.color = "transparent"
                              ))
      # a <- leafem::addImageQuery(a, x = raster::raster(ll[[rtmNam]]),
      #                           layerId = rtmNam,
      #                           group = rtmNam,
      #                           type='click',
      #                           digits=0)
      pal2 <- leaflet::colorNumeric(pal, domain = NULL)
      a <- suppressWarnings(
        # falseWarnings = "n too large",
        leaflet::addLegend(a, position = "bottomright",
                           pal = pal2,
                           group = rtmNam,
                           title= rtmNam,
                           values = sort(unique(terra::values(ll[[rtmNam]])))))
    }

    a <- leafem::garnishMap(a, leaflet::addScaleBar, leafem::addMouseCoordinates)
    subTitle <- c(subTitle, rtmsNames[min(ordRtms)])
  }

  ll <- toLatLong(ll, rtmsNames, sasNames) # |> reproducible::Cache()

  v <- list()

  if (!exists("a", inherits = FALSE)) {
    a <- leaflet::leaflet() |>
      leaflet::addTiles() #|>
  }


  if (length(sasNames) > 0) {
    i <- 0
    for (sa in names(ordSas)) {
      i <- i + 1
      if (!is(ll[[sa]], "SpatVector")) {
        vv <- terra::vect(ll[[sa]])
      } else {
        vv <- ll[[sa]]
      }
      a <- a |> leaflet::addPolygons(data=vv, weight = 3,
                            label = ~paste0(sa),
                            fillColor = saCols[i], color = saCols[i],
                            fillOpacity = 0, group = sa,
                            highlight = leaflet::highlightOptions(weight = 10#,
                                                         # fillOpacity = 0.7
                                                         # color = "blue",
                                                         # bringToFront = TRUE
                                                         ))
    }
  }

  a <- leaflet::addLayersControl(a, overlayGroups = c(sasNames, rtmsNames),
                                 options = leaflet::layersControlOptions(collapse = FALSE),
                                 position = "bottomleft")

  keepLargestSA <- grep("studyArea", names(ordSas), value = TRUE)[1]
  wh <- which(names(ordSas) == keepLargestSA)
  a <- leaflet::hideGroup(a, c(names(ordSas)[-wh], rtmsNames[rev(ordRtms)][-1]))

  exts <- extInLatLong(ll[[names(ordSas)[[1]]]])
  a <- leaflet::fitBounds(a, lng1 = xminFn(exts), lat1 = yminFn(exts),
                          lng2 = xmaxFn(exts), lat2 = ymaxFn(exts))
  a <- leafem::addHomeButton(a, as.vector(exts), "Full Extent")

  a
}


minmaxFn <- function(x, whMinMax = c("xmin", "xmax", "ymin", "ymax")) {
  if (is(x, "SpatVector") || is(x, "SpatRaster") || is(x, "SpatExtent"))
    get(whMinMax[1], envir = asNamespace("terra"))(x)
  else if (is(x, "sf") || is(x, "sfc") || is(x, "bbox"))
    sf::st_bbox(x)[[whMinMax[1]]]
  else
    NULL
}

xmaxFn <- function(x)
  minmaxFn(x, "xmax")
xminFn <- function(x)
  minmaxFn(x, "xmin")
ymaxFn <- function(x)
  minmaxFn(x, "ymax")
yminFn <- function(x)
  minmaxFn(x, "ymin")

areas <- function(x) {
  if (is(x, "SpatVector")) {
    sum(terra::expanse(x))
  } else if (is(x, "sf") || is(x, "sfc")) {
    sum(sf::st_area(x))
  } else {
    if (is(x, "SpatRaster")) {
      sum(!is.na(x[])) * prod(res(x))
    } else {
      NULL
    }
  }
}

buffs <- function(x, ...) {
  dots <- list(...)

  if (is(x, "SpatVector")) {
    if ("dist" %in% names(dots)) {
      dots$width <- dots$dist
      dots$dist <- NULL

    }
    do.call(terra::buffer, append(list(x), dots))
  } else if (is(x, "sf") || is(x, "sfc")) {
    if ("width" %in% names(dots)) {
      dots$dist <- dots$width
      dots$width <- NULL
    }
    do.call(sf::st_buffer, append(list(x), dots))
  } else {
    NULL
  }
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
      reproducible::postProcessTo(ll[[rtm]], projectTo = projectTo)  |>
        reproducible::Cache())
  }
  if (length(sasNames)) {
    ll[sasNames] <- Map(sa = sasNames, function(sa)
      reproducible::postProcessTo(ll[[sa]], projectTo = projectTo) |>
        reproducible::Cache())
  }
}

makeListToPlot <- function(ll, include, exclude, ...) {
  dots <- list(...)
  if (length(dots))
    ll <- appendDotsToLL(ll, dots)

  sasNames <- grep(names(ll), pattern = "studyArea", value = TRUE)
  rtmsNames <- grep(names(ll), pattern = "rasterToMatch", value = TRUE)

  RastClasses <- "SpatRaster"
  VectClasses <- c("SpatVector", "sf", "sfc")
  if (isTRUE(include)) { # || length(include)) {
    include <- ls(ll)
  }
  if (length(include) && !(isTRUE(include %in% FALSE))) {
    isRas <- mapply(obj = include, function(obj) any(sapply(RastClasses, function(cla) is(ll[[obj]], cla))))
    isVec <- mapply(obj = include, function(obj) any(sapply(VectClasses, function(cla) is(ll[[obj]], cla))))
    sasNames <- unique(c(sasNames, include[isVec]))
    nlyrs <- Map(rast = include[isRas], function(rast) terra::nlyr(ll[[rast]]))
    rtmsNames <- unique(c(rtmsNames, include[isRas][nlyrs == 1]))
  }

  if (!missing(exclude))
    if (!isTRUE(exclude %in% FALSE) || any(nzchar(exclude))) {
      sasNames <- setdiff(sasNames, exclude)
      rtmsNames <- setdiff(rtmsNames, exclude)
    }

  sizes <- sapply(sasNames, function(sa) areas(ll[[sa]]))
  empty <- sizes == 0
  if (any(empty)) {
    sasNames <- sasNames[!empty]
    sizes <- sizes[!empty]
  }

  if (is(ll, "simList")) {
    ll <- mget(c(sasNames, rtmsNames), envir = SpaDES.core::envir(ll))
  }
  sizesRtms <- sapply(ll[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  ord <- order(sizes)
  ordSas <- rev(sizes[ord])

  list(ll = ll, sizes = sizes, sasNames = sasNames, rtmsNames = rtmsNames,
       ordRtms = ordRtms, ordSas = ordSas)
}

WhiteboxCols <- c("atlas", "high_relief", "arid", "soft", "muted", "purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep")

extInLatLong <- function(x) {
  if (is(x, "SpatVector"))
    terra::ext(x)
  else if (is(x, "sf") || is(x, "sfc"))
    sf::st_bbox(x)
  else
    NULL
}

#' @importFrom utils installed.packages
requireNamespaces <- function(pkgs) {
  if (!all(sapply(pkgs, requireNamespace))) {
    ip <- installed.packages() |> as.data.table()
    missingPkgs <- pkgs[!pkgs %in% ip$Package]
    stop("Please install c('", paste(missingPkgs, collapse = "', '"), "')")
  }
}

rasterToMatchPaletteUpdate <- function(rasterToMatchPalette, rtmsNames) {
  hasName <- hasNames(rasterToMatchPalette)
  # namsRTMP <- names(rasterToMatchPalette)
  # hasName <- nzchar(namsRTMP)
  if (any(hasName)) {
    rasterToMatchPalette <- rasterToMatchPalette[!hasName]
  }

  if (length(rasterToMatchPalette) < length(rtmsNames)) {
    rasterToMatchPalette <- rep(rasterToMatchPalette, length.out = length(rtmsNames))
  }
  rasterToMatchPalette
}

rasterToMatchPaletteNamed <- function(rasterToMatchPalette) {
  hasName <- hasNames(rasterToMatchPalette)
  # namsRTMP <- names(rasterToMatchPalette)
  # hasName <- nzchar(namsRTMP)
  if (any(hasName)) {
    rasterToMatchPaletteNamed <- rasterToMatchPalette[hasName]
  }
  rasterToMatchPaletteNamed
}

hasNames <- function(rasterToMatchPalette) {
  namsRTMP <- names(rasterToMatchPalette)
  hasName <- nzchar(namsRTMP)
}
