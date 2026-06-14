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

      ## A categorical (factor) rasterToMatch (e.g., ELF classes) produces a
      ## discrete fill aesthetic; a continuous scale (gradientn / whitebox_c)
      ## errors with "Discrete value supplied to a continuous scale". Pick the
      ## matching discrete scale in that case.
      isCategorical <- isTRUE(any(terra::is.factor(ll[[rtmNam]])))

      if (isRColBrew) {
        theColFun <- RColorBrewer::brewer.pal(9, paletteThisRas) |>
          colorRampPalette()

        if (isCategorical) {
          nLev <- nrow(terra::cats(ll[[rtmNam]])[[1]])
          g[[rtmNam]] <- g[[rtmNam]] +
            ggplot2::scale_fill_manual(name = rtmNam,
                                       na.value = "transparent",
                                       na.translate = FALSE,
                                       values = theColFun(max(1L, nLev)))
        } else {
          g[[rtmNam]] <- g[[rtmNam]] +
            ggplot2::scale_fill_gradientn(name = rtmNam,
                                          na.value = "transparent",
                                          colours = theColFun(20))
        }

      } else {
        if (isCategorical) {
          g[[rtmNam]] <- g[[rtmNam]] +
            tidyterra::scale_fill_whitebox_d(
              na.value = "transparent",
              palette = paletteThisRas
            )
        } else {
          g[[rtmNam]] <- g[[rtmNam]] +
            tidyterra::scale_fill_whitebox_c(
              na.value = "transparent",
              palette = paletteThisRas
            )
        }
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

  ## Study-area-only case: with no rasterToMatch, study areas are never added to
  ## `g` above (they are only drawn as overlays on rasterToMatch panels), leaving
  ## `g` empty and `patchwork::wrap_plots(list())` failing with
  ## "'x' and 'units' must have length > 0". Build a single panel of the study
  ## area polygons over the `Canada` base instead.
  if (length(rtmsNames) == 0 && length(sasNames) > 0) {
    saPlotName <- "studyAreas"
    g[[saPlotName]] <- p + tidyterra::geom_spatvector(data = Canada, fill = "NA")
    subTitle[[saPlotName]] <- character()
    i <- 0
    for (sa in names(ordSas)) {
      i <- i + 1
      g[[saPlotName]] <- g[[saPlotName]] +
        tidyterra::geom_spatvector(data = ll[[sa]], fill = NA, col = saCols[i], lwd = 0.5)
      subTitle[[saPlotName]] <- c(subTitle[[saPlotName]], paste0(sa, " (", saCols[i], ")"))
    }
    g[[saPlotName]] <- g[[saPlotName]] +
      ggplot2::labs(subtitle = paste(subTitle[[saPlotName]], collapse = ", ")) +
      ggplot2::theme_bw()

    if (missing(title)) {
      title <- singularPlural(c("studyArea", "studyAreas"), l = sasNames)
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

      geoTiffFile <- .leafletGeoTiffPath(rtmNam)
      terra::writeRaster(ll[[rtmNam]], filename = geoTiffFile, overwrite = TRUE)

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
      sum(!is.na(x[])) * prod(terra::res(x))
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

#' Time-series of `SpatRaster` layers as an interactive leaflet map with a slider
#'
#' Takes a multi-layer `SpatRaster` (or a list of single-layer `SpatRaster`s)
#' and produces a self-contained leaflet htmlwidget. Each layer becomes a
#' radio-selected base group; a draggable range slider is added that drives
#' the radio buttons, so the user steps through years by dragging.
#'
#' Designed to ship in a Quarto / `knitr` static render — uses
#' `.leafletGeoTiffPath()` internally so the per-layer GeoTIFFs are written
#' into the qmd's `_files/figure-html/` folder rather than `tempfile()`.
#'
#' @param x   A multi-layer `SpatRaster`, or a list of single-layer
#'   `SpatRaster`s. The order of layers becomes the order along the slider.
#' @param years   Character or numeric vector, length equal to the number of
#'   layers. Defaults to `names(x)`. These labels appear on the slider and as
#'   the radio-button group names.
#' @param palette   Name of an `hcl.colors()` palette (e.g. `"RdBu"`,
#'   `"Spectral"`, `"viridis"`). Default `"RdBu"` -- a diverging palette
#'   suited to difference / signed-magnitude maps.
#' @param rev   Reverse the palette? Default `TRUE` (puts red on positive).
#' @param layerName   Short prefix used for each per-layer GeoTIFF filename
#'   (avoids collisions when multiple time-series sit on the same page).
#' @param sliderPosition   Leaflet control position for the slider:
#'   `"bottomleft"` (default), `"bottomright"`, `"topleft"`, `"topright"`.
#'
#' @return A `leaflet` htmlwidget with the slider injected via
#'   `htmlwidgets::onRender()`. Ships as static HTML -- no Shiny server needed.
#'
#' @seealso [plotSAsLeaflet()] for the single-snapshot case.
#'
#' @export
plotTimeSeriesLeaflet <- function(x,
                                  years = NULL,
                                  palette = "RdBu",
                                  rev = TRUE,
                                  layerName = "raster",
                                  sliderPosition = "bottomleft") {
  pkgs <- c("leaflet", "leafem", "terra", "htmlwidgets", "jsonlite")
  requireNamespaces(pkgs)

  ## --- coerce input to a list-of-single-layer-SpatRasters ---
  if (inherits(x, "SpatRaster")) {
    nlyr <- terra::nlyr(x)
    layerList <- lapply(seq_len(nlyr), function(i) x[[i]])
    if (is.null(years)) years <- names(x)
  } else if (is.list(x) && all(vapply(x, inherits, logical(1), "SpatRaster"))) {
    layerList <- x
    if (is.null(years)) years <- names(x)
  } else {
    stop("`x` must be a multi-layer SpatRaster or a list of SpatRasters",
         call. = FALSE)
  }

  if (length(layerList) < 2L) {
    stop("`plotTimeSeriesLeaflet()` requires at least 2 layers; ",
         "use `plotSAsLeaflet()` for a single raster", call. = FALSE)
  }
  if (is.null(years) || !length(years)) {
    years <- as.character(seq_along(layerList))
  }
  if (length(years) != length(layerList)) {
    stop("`years` length (", length(years),
         ") does not match number of layers (", length(layerList), ")",
         call. = FALSE)
  }
  years <- as.character(years)

  ## --- build the colour ramp once ---
  cols <- grDevices::hcl.colors(100, palette)
  if (isTRUE(rev)) cols <- base::rev(cols)

  ## --- assemble the map: basemap + one group per year ---
  m <- leaflet::leaflet() |> leaflet::addTiles()

  for (i in seq_along(layerList)) {
    yr <- years[[i]]
    tif <- .leafletGeoTiffPath(paste0(layerName, "-", yr))
    terra::writeRaster(layerList[[i]], tif, overwrite = TRUE)
    m <- leafem::addGeotiff(
      m, tif,
      group = yr,
      layerId = yr,
      colorOptions = leafem::colorOptions(
        palette = cols,
        na.color = "transparent"
      )
    )
  }

  m <- m |> leaflet::addLayersControl(
    baseGroups = years,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  ## --- slider JS: finds the radio inputs leaflet just rendered and
  ## .click()s them as the slider moves. No new R deps; pure DOM. ---
  yearsJSON <- jsonlite::toJSON(years, auto_unbox = FALSE)
  js <- sprintf(
    "function(el, x) {
       var map = this;
       var years = %s;
       var pos = '%s';
       setTimeout(function() {
         var radios = el.querySelectorAll('.leaflet-control-layers-base input[type=\"radio\"]');
         if (!radios.length) return;
         var sliderId = 'ts-slider-' + Math.random().toString(36).slice(2, 8);
         var labelId  = 'ts-label-'  + Math.random().toString(36).slice(2, 8);
         var html =
           '<div style=\"background:white;padding:8px;border-radius:5px;box-shadow:0 1px 4px rgba(0,0,0,0.3);\">' +
             '<input type=\"range\" min=\"0\" max=\"' + (radios.length - 1) + '\" value=\"0\" id=\"' + sliderId + '\" style=\"width:240px;display:block;\">' +
             '<div style=\"text-align:center;margin-top:4px;font-family:sans-serif;\"><b id=\"' + labelId + '\">' + years[0] + '</b></div>' +
           '</div>';
         var SliderCtrl = L.Control.extend({
           onAdd: function() {
             var div = L.DomUtil.create('div');
             div.innerHTML = html;
             L.DomEvent.disableClickPropagation(div);
             L.DomEvent.disableScrollPropagation(div);
             return div;
           }
         });
         map.addControl(new SliderCtrl({position: pos}));
         var slider = document.getElementById(sliderId);
         var label  = document.getElementById(labelId);
         slider.addEventListener('input', function() {
           var idx = parseInt(this.value);
           label.textContent = years[idx];
           radios[idx].click();
         });
         radios[0].click();
       }, 150);
     }",
    yearsJSON, sliderPosition
  )

  htmlwidgets::onRender(m, js)
}

#' Difference between two layers of a time-series, as a leaflet map
#'
#' Subtracts the `from` layer from the `to` layer and plots the result on a
#' single-layer leaflet map with a diverging palette centred on zero. The
#' "change from start to finish" view that pairs with [plotTimeSeriesLeaflet()].
#'
#' @param x   A multi-layer `SpatRaster` or a list of single-layer
#'   `SpatRaster`s.
#' @param from,to   Names (or, if missing, first/last) of the layers to subtract.
#'   `result = x[[to]] - x[[from]]`.
#' @param palette,rev   Diverging palette and direction. Defaults to `"RdBu"`,
#'   reversed (red = positive change, blue = negative change).
#' @param layerName   Short prefix for the per-layer GeoTIFF filename.
#'
#' @return A `leaflet` htmlwidget showing the difference layer with a
#'   symmetric (zero-centred) colour scale. Static-safe.
#'
#' @seealso [plotTimeSeriesLeaflet()] for the time-step viewer that
#'   complements this difference view.
#'
#' @export
plotChangeOverTime <- function(x,
                               from = NULL,
                               to = NULL,
                               palette = "RdBu",
                               rev = TRUE,
                               layerName = "change") {
  pkgs <- c("leaflet", "leafem", "terra")
  requireNamespaces(pkgs)

  ## --- coerce input ---
  if (inherits(x, "SpatRaster")) {
    nlyr <- terra::nlyr(x)
    layerList <- lapply(seq_len(nlyr), function(i) x[[i]])
    names(layerList) <- names(x)
  } else if (is.list(x) && all(vapply(x, inherits, logical(1), "SpatRaster"))) {
    layerList <- x
  } else {
    stop("`x` must be a multi-layer SpatRaster or a list of SpatRasters",
         call. = FALSE)
  }

  ns <- names(layerList)
  if (is.null(ns) || any(!nzchar(ns))) {
    stop("layers of `x` must be named (the names label the from/to selection)",
         call. = FALSE)
  }
  if (length(layerList) < 2L) {
    stop("`plotChangeOverTime()` needs at least 2 named layers", call. = FALSE)
  }
  if (is.null(from)) from <- ns[[1L]]
  if (is.null(to))   to   <- ns[[length(ns)]]
  if (!from %in% ns) stop("`from = \"", from, "\"` not in layer names: ",
                          paste(ns, collapse = ", "), call. = FALSE)
  if (!to   %in% ns) stop("`to = \"",   to,   "\"` not in layer names: ",
                          paste(ns, collapse = ", "), call. = FALSE)

  ## --- difference, symmetric breaks around 0 ---
  diffRas <- layerList[[to]] - layerList[[from]]
  vrange  <- terra::minmax(diffRas)
  absmax  <- max(abs(vrange), na.rm = TRUE)
  if (!is.finite(absmax) || absmax == 0) absmax <- 1   # degenerate fallback

  cols <- grDevices::hcl.colors(100, palette)
  if (isTRUE(rev)) cols <- base::rev(cols)

  ## --- one-layer map ---
  m <- leaflet::leaflet() |> leaflet::addTiles()
  tif <- .leafletGeoTiffPath(paste0(layerName, "-", to, "-minus-", from))
  terra::writeRaster(diffRas, tif, overwrite = TRUE)

  groupName <- paste0(to, " − ", from)   # use proper minus sign (displayed in UI)
  layerId   <- paste0(make.names(to), "-minus-", make.names(from))   # no spaces
  m <- leafem::addGeotiff(
    m, tif,
    group = groupName,
    layerId = layerId,
    colorOptions = leafem::colorOptions(
      palette = cols,
      breaks = seq(-absmax, absmax, length.out = length(cols) + 1L),
      na.color = "transparent"
    )
  )

  m |> leaflet::addLayersControl(
    overlayGroups = groupName,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}

## Resolve a GeoTIFF output path for a raster layer used by `plotSAsLeaflet`.
## Outside of a knitr render, returns a `tempfile()` (preserves existing
## interactive behaviour in RStudio's viewer). Inside knitr, returns a path
## under `knitr::fig_path()` so the GeoTIFF is written into the qmd's
## `_files/figure-html/` folder -- which Quarto copies alongside the rendered
## HTML, so the leaflet widget's relative URL resolves correctly when the page
## is served from a static site (e.g., GitHub Pages). Absolute `tempfile()`
## paths break in that context because (a) the path is filesystem-absolute and
## not browser-fetchable, and (b) `tempdir()` is wiped when the render's R
## session exits.
.leafletGeoTiffPath <- function(rtmNam) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    fp <- knitr::fig_path(paste0("-", make.names(rtmNam), ".tif"))
    dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
    fp
  } else {
    tempfile(fileext = ".tif")
  }
}
