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
#' @param x   One of:
#'   * a multi-layer `SpatRaster` -- becomes one "object" named `"raster"`
#'   * a list of single-layer `SpatRaster`s -- same
#'   * a `simList` -- reads ALL `<obj>*.tif` time-series from
#'     `SpaDES.core::outputPath(x)`
#'   * a length-1 character path to a directory of GeoTIFFs -- same scan
#'
#'   For the disk-scan cases, files are grouped by base name (last regex
#'   match = time tag) into separate "objects". Each object becomes a
#'   radio-selectable layer in the UI.
#' @param palette   Palette name. Default `"viridis"`. Tried first with
#'   [terra::map.pal()], falling back to [grDevices::hcl.colors()].
#' @param rev   Reverse the palette? Default `FALSE`.
#' @param layerName   Short prefix used for each per-layer GeoTIFF filename
#'   (avoids collisions when multiple time-series sit on the same page).
#' @param sliderPosition   Leaflet control position for the object-radio +
#'   year-slider UI: `"bottomleft"` (default), `"bottomright"`,
#'   `"topleft"`, `"topright"`.
#' @param legendPosition   Leaflet control position for the per-object
#'   colour legend. Default `"bottomright"`.
#'
#' @return A `leaflet` htmlwidget with the slider injected via
#'   `htmlwidgets::onRender()`. Ships as static HTML -- no Shiny server needed.
#'
#' @seealso [plotSAsLeaflet()] for the single-snapshot case.
#'
#' @export
plotTimeSeriesLeaflet <- function(x,
                                  palette = "viridis",
                                  rev = FALSE,
                                  layerName = "raster",
                                  sliderPosition = "bottomleft",
                                  legendPosition = "bottomright") {
  pkgs <- c("leaflet", "leafem", "terra", "htmlwidgets", "jsonlite")
  requireNamespaces(pkgs)

  ## --- coerce input: get ALL objects (one per discovered time-series) ---
  objects <- .coerceToMultiObjects(x)
  objects <- Filter(function(lst) length(lst) >= 1L, objects)
  if (!length(objects)) stop("No usable objects to plot", call. = FALSE)

  ## --- shared colour ramp; per-object domain so palette stays meaningful ---
  cols <- tryCatch(terra::map.pal(palette, n = 100),
                   error = function(e) grDevices::hcl.colors(100, palette))
  if (isTRUE(rev)) cols <- base::rev(cols)

  m <- leaflet::leaflet(options = leaflet::leafletOptions(
         zoomSnap = 0.25, zoomDelta = 0.25)) |>
    leaflet::addTiles()

  bounds      <- NULL
  objYears    <- list()       # objName -> character vector of year labels (ordered)
  objDomains  <- list()       # objName -> c(min, max) used for both raster + legend
  legendIdx   <- 0L

  for (objName in names(objects)) {
    layerList <- objects[[objName]]
    yrs <- names(layerList)
    if (is.null(yrs) || any(!nzchar(yrs))) next
    objYears[[objName]] <- as.character(yrs)

    ## per-object value range -- one palette domain for all years within an
    ## object so the slider doesn't change the colour meaning as you scrub
    allVals <- unlist(lapply(layerList, function(r) {
      mm <- terra::minmax(r); mm[is.finite(mm)]
    }))
    if (length(allVals)) {
      rng <- range(allVals)
    } else {
      rng <- c(0, 1)
    }
    if (diff(rng) == 0) rng <- rng + c(-1, 1) * (abs(rng[1L]) + 1) * 1e-6
    objDomains[[objName]] <- rng

    for (yr in yrs) {
      ras <- layerList[[yr]]
      tif <- .leafletGeoTiffPath(paste0(layerName, "-", objName, "-", yr))
      terra::writeRaster(ras, tif, overwrite = TRUE)

      if (is.null(bounds)) {
        bounds <- tryCatch({
          e <- terra::ext(terra::project(ras, "EPSG:4326"))
          as.numeric(c(e[1L], e[3L], e[2L], e[4L]))
        }, error = function(...) NULL)
      }

      groupName <- paste0(objName, "_", yr)
      m <- leafem::addGeotiff(
        m, tif,
        group = groupName,
        layerId = make.names(groupName),
        colorOptions = leafem::colorOptions(
          palette  = cols,
          domain   = rng,
          na.color = "transparent"
        )
      )
    }

    ## one legend per object, toggle-tied by integer index in its className.
    ## shine's rev-pal + reversed-labFormat trick: max value at TOP.
    m <- m |> leaflet::addLegend(
      position  = legendPosition,
      pal       = leaflet::colorNumeric(base::rev(cols), domain = rng,
                                        na.color = "transparent"),
      values    = rng,
      title     = objName,
      opacity   = 1,
      className = paste0("info legend ts-objlegend-", legendIdx),
      labFormat = leaflet::labelFormat(
        transform = function(x) sort(x, decreasing = TRUE)
      )
    )
    legendIdx <- legendIdx + 1L
  }

  if (!length(objYears)) {
    stop("No object had usable named years", call. = FALSE)
  }

  ## --- fitBounds with padding for slider + legend ---
  if (!is.null(bounds)) {
    base <- c(20L, 20L)
    legPad   <- c(260L, 100L)
    slidePad <- c(280L, 80L)
    ptl <- base; pbr <- base
    add <- function(side, p) switch(side,
      topleft     = function(cur) pmax(cur, p),
      bottomleft  = function(cur) pmax(cur, c(p[1L], base[2L])),
      topright    = function(cur) pmax(cur, c(p[1L], base[2L])),
      bottomright = function(cur) pmax(cur, p),
      identity)
    apply_to_TL <- function(side, p) if (side %in% c("topleft", "bottomleft")) add(side, p) else identity
    apply_to_BR <- function(side, p) if (side %in% c("topright", "bottomright")) add(side, p) else identity
    ptl <- apply_to_TL(legendPosition, legPad)(ptl)
    pbr <- apply_to_BR(legendPosition, legPad)(pbr)
    ptl <- apply_to_TL(sliderPosition, slidePad)(ptl)
    pbr <- apply_to_BR(sliderPosition, slidePad)(pbr)
    m <- m |> leaflet::fitBounds(
      bounds[1L], bounds[2L], bounds[3L], bounds[4L],
      options = list(paddingTopLeft = ptl, paddingBottomRight = pbr)
    )
  }

  ## --- hidden layers control: gives us baseGroup radios that toggle
  ## layer visibility for free; our custom UI just .click()s them ---
  allGroups <- unlist(lapply(names(objYears), function(o) paste0(o, "_", objYears[[o]])))
  m <- m |> leaflet::addLayersControl(
    baseGroups = allGroups,
    options    = leaflet::layersControlOptions(collapsed = TRUE)
  )

  ## --- inject custom UI: object radios + year slider + legend toggle ---
  objYearsJSON <- jsonlite::toJSON(objYears, auto_unbox = FALSE)
  uiJS <- sprintf(
    "function(el, x) {
      var map = this;
      var objYears = %s;
      var objNames = Object.keys(objYears);
      var sliderPos = '%s';
      var activeObj = objNames[0];
      var activeYearIdx = 0;

      setTimeout(function() {
        // Build the custom control HTML (object radios + year slider)
        var html = '<div style=\"background:white;padding:8px 10px;border-radius:5px;box-shadow:0 1px 4px rgba(0,0,0,0.3);font-family:sans-serif;font-size:11px;line-height:1.3;\">';
        if (objNames.length > 1) {
          html += '<div style=\"font-weight:bold;margin-bottom:4px;\">Object</div>';
          html += '<div style=\"display:flex;flex-direction:column;gap:2px;margin-bottom:8px;\">';
          objNames.forEach(function(o, i) {
            var checked = (i === 0) ? ' checked' : '';
            html += '<label style=\"cursor:pointer;\"><input type=\"radio\" name=\"ts-obj\" value=\"' + o + '\"' + checked + '> <span>' + o + '</span></label>';
          });
          html += '</div>';
        }
        html += '<div style=\"margin-bottom:2px;\"><b>Year:</b> <span id=\"ts-year-label\"></span></div>';
        html += '<input type=\"range\" id=\"ts-year-slider\" min=\"0\" max=\"0\" value=\"0\" style=\"width:240px;display:block;\">';
        html += '</div>';

        var Ctl = L.Control.extend({
          onAdd: function() {
            var d = L.DomUtil.create('div');
            d.innerHTML = html;
            L.DomEvent.disableClickPropagation(d);
            L.DomEvent.disableScrollPropagation(d);
            return d;
          }
        });
        map.addControl(new Ctl({position: sliderPos}));

        // Wire it up
        var slider = el.querySelector('#ts-year-slider');
        var label  = el.querySelector('#ts-year-label');
        var radios = el.querySelectorAll('input[name=\"ts-obj\"]');
        var baseRadios = el.querySelectorAll('.leaflet-control-layers-base input[type=\"radio\"]');

        function clickBaseGroup(groupName) {
          for (var i = 0; i < baseRadios.length; i++) {
            var lbl = baseRadios[i].parentNode.textContent.trim();
            if (lbl === groupName) { baseRadios[i].click(); return; }
          }
        }
        function showLegendByIdx(idx) {
          for (var i = 0; i < objNames.length; i++) {
            var nodes = el.querySelectorAll('.ts-objlegend-' + i);
            nodes.forEach(function(n) { n.style.display = (i === idx ? '' : 'none'); });
          }
        }
        function refreshSlider() {
          var yrs = objYears[activeObj];
          slider.min = 0;
          slider.max = yrs.length - 1;
          if (activeYearIdx > yrs.length - 1) activeYearIdx = yrs.length - 1;
          slider.value = activeYearIdx;
          label.textContent = yrs[activeYearIdx];
        }
        function apply() {
          var yrs = objYears[activeObj];
          var yr = yrs[activeYearIdx];
          label.textContent = yr;
          clickBaseGroup(activeObj + '_' + yr);
          showLegendByIdx(objNames.indexOf(activeObj));
        }

        radios.forEach(function(r) {
          r.addEventListener('change', function() {
            if (r.checked) { activeObj = r.value; refreshSlider(); apply(); }
          });
        });
        slider.addEventListener('input', function() {
          activeYearIdx = parseInt(this.value, 10);
          apply();
        });

        refreshSlider();
        apply();
      }, 150);
    }",
    objYearsJSON, sliderPosition
  )
  htmlwidgets::onRender(m, uiJS)
}

#' Difference between two layers of a time-series, as a leaflet map
#'
#' Subtracts the `from` layer from the `to` layer and plots the result on a
#' single-layer leaflet map with a diverging palette centred on zero. The
#' "change from start to finish" view that pairs with [plotTimeSeriesLeaflet()].
#'
#' @param x   One of: a multi-layer `SpatRaster`, a list of single-layer
#'   `SpatRaster`s, a `simList` (reads `<name>*.tif` from
#'   `SpaDES.core::outputPath(x)`), or a length-1 character directory path.
#' @param from,to   Names (or, if missing, first/last) of the layers to subtract.
#'   `result = x[[to]] - x[[from]]`.
#' @param name   Required when `x` is a `simList` or directory path: the
#'   base name of the output object whose GeoTIFFs should be loaded
#'   (e.g. `"simPred"`).
#' @param palette   Palette name. Default `"differences"` -- the dedicated
#'   blue→white→red diverging palette from [terra::map.pal()], purpose-built
#'   for difference maps. Any other [terra::map.pal()] name (e.g. `"viridis"`)
#'   is also accepted; if `terra::map.pal()` doesn't recognise the name, we
#'   fall back to [grDevices::hcl.colors()] (e.g. `"RdBu"`, `"Spectral"`).
#' @param rev   Reverse the palette? Default `TRUE` -- with the `"differences"`
#'   palette this gives red = negative, blue = positive. Set `FALSE` to flip
#'   (the `terra::map.pal("differences")` native orientation: red = positive,
#'   blue = negative).
#' @param layerName   Short prefix for the per-layer GeoTIFF filename.
#' @param legendPosition   Where to place the legend on the map.
#'   One of `"bottomright"` (default), `"bottomleft"`, `"topright"`, `"topleft"`.
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
                               palette = "differences",
                               rev = TRUE,
                               layerName = "change",
                               legendPosition = "bottomright") {
  pkgs <- c("leaflet", "leafem", "terra")
  requireNamespaces(pkgs)

  ## --- coerce input: get ALL discovered objects, one diff per object ---
  objects <- .coerceToMultiObjects(x)

  ## --- colour ramp (shared across all objects) ---
  cols <- tryCatch(
    terra::map.pal(palette, n = 100),
    error = function(e) grDevices::hcl.colors(100, palette)
  )
  if (isTRUE(rev)) cols <- base::rev(cols)

  ## zoomSnap/zoomDelta = 0.25 (shine's pattern) lets fitBounds pick a
  ## fractional zoom that hugs the data instead of snapping to the next
  ## integer (= one step too far out, the "tiny map" complaint).
  m <- leaflet::leaflet(options = leaflet::leafletOptions(
         zoomSnap = 0.25, zoomDelta = 0.25)) |>
    leaflet::addTiles()
  groupNames <- character()
  legendIdx  <- 0L     # integer suffix on the legend className → JS-friendly
  bounds     <- NULL   # captured from first diff raster for fitBounds() later

  for (objName in names(objects)) {
    layerList <- objects[[objName]]
    ns <- names(layerList)
    if (is.null(ns) || any(!nzchar(ns)) || length(layerList) < 2L) next

    fromYr <- if (is.null(from)) ns[[1L]] else from
    toYr   <- if (is.null(to))   ns[[length(ns)]] else to
    if (!fromYr %in% ns || !toYr %in% ns) next

    diffRas <- layerList[[toYr]] - layerList[[fromYr]]
    vrange  <- terra::minmax(diffRas)
    absmax  <- max(abs(vrange), na.rm = TRUE)
    if (!is.finite(absmax) || absmax == 0) absmax <- 1

    if (is.null(bounds)) {
      bounds <- tryCatch({
        e <- terra::ext(terra::project(diffRas, "EPSG:4326"))
        as.numeric(c(e[1L], e[3L], e[2L], e[4L]))   # lng1, lat1, lng2, lat2
      }, error = function(...) NULL)
    }

    tif <- .leafletGeoTiffPath(paste0(layerName, "-", objName, "-",
                                      toYr, "-minus-", fromYr))
    terra::writeRaster(diffRas, tif, overwrite = TRUE)

    groupName <- paste0(objName, ": ", toYr, " − ", fromYr)
    groupNames <- c(groupNames, groupName)
    layerId <- make.names(paste0(objName, "-", toYr, "-minus-", fromYr))
    breaks <- seq(-absmax, absmax, length.out = length(cols) + 1L)

    m <- leafem::addGeotiff(
      m, tif,
      group = groupName,
      layerId = layerId,
      ## leave autozoom = default TRUE so we still get *some* fit even if
      ## bounds capture fails downstream; our legend-aware fitBounds()
      ## below will override when bounds are valid.
      colorOptions = leafem::colorOptions(
        palette  = cols,
        breaks   = breaks,
        na.color = "transparent"
      )
    )

    ## per-object continuous legend, max-at-top. The trick is shine's
    ## (see SpaDES.shiny:::.shineAddDiff): pass `rev(cols)` to colorNumeric AND
    ## reverse the label order via labFormat -- both flips compose so the
    ## gradient bar shows positive (blue) at top with correctly oriented labels.
    ## Integer suffix in className -> JS lookup is bulletproof regardless of
    ## special chars in groupName.
    dom <- c(-absmax, absmax)
    m <- m |> leaflet::addLegend(
      position  = legendPosition,
      pal       = leaflet::colorNumeric(base::rev(cols), domain = dom,
                                        na.color = "transparent"),
      values    = dom,
      title     = groupName,
      opacity   = 1,
      className = paste0("info legend ts-legend-", legendIdx),
      labFormat = leaflet::labelFormat(
        transform = function(x) sort(x, decreasing = TRUE)
      )
    )
    legendIdx <- legendIdx + 1L
  }

  if (!length(groupNames)) {
    stop("No object had at least 2 named layers to difference", call. = FALSE)
  }

  ## --- fitBounds with padding that respects where the legend lives, so
  ## the raster isn't half-obscured on initial view. We disabled addGeotiff's
  ## autozoom above; the captured `bounds` is the EPSG:4326 extent of the
  ## first diff raster (all diffs share the same area). Padding is in pixels;
  ## ~220 leaves room for a typical legend, ~30 for a comfortable margin.
  if (!is.null(bounds)) {
    ## Padding in pixels. Leaflet's fitBounds inset = corner-keep-out distance:
    ## the data is fit into (width - leftPad - rightPad) x (height - topPad -
    ## bottomPad) and centered within THAT, so generous padding on the legend
    ## side both shrinks AND shifts the visible map away from the legend.
    base <- c(20L, 20L)
    legendPad <- c(260L, 100L)           # ~legend control width × height
    ptl <- base
    pbr <- base
    if (legendPosition == "topleft")     ptl <- pmax(ptl, legendPad)
    if (legendPosition == "bottomleft")  ptl <- pmax(ptl, c(legendPad[1L], base[2L]))
    if (legendPosition == "topright")    pbr <- pmax(pbr, c(legendPad[1L], base[2L]))
    if (legendPosition == "bottomright") pbr <- pmax(pbr, legendPad)
    m <- m |> leaflet::fitBounds(
      bounds[1L], bounds[2L], bounds[3L], bounds[4L],
      options = list(paddingTopLeft = ptl, paddingBottomRight = pbr)
    )
  }

  ## --- baseGroups give RADIO behaviour -- only one object visible at a time ---
  m <- m |> leaflet::addLayersControl(
    baseGroups = groupNames,
    options    = leaflet::layersControlOptions(collapsed = FALSE)
  )

  ## leaflet's `group =` on addLegend ties to overlay groups, NOT baseGroups,
  ## so per-object legends are all simultaneously visible unless we toggle
  ## them ourselves. Listen for `baselayerchange` and show only the matching
  ## legend (by integer index encoded in its className).
  groupsJSON <- jsonlite::toJSON(groupNames, auto_unbox = FALSE)
  legendToggleJS <- sprintf("
    function(el, x) {
      var map = this;
      var groups = %s;
      function showByIdx(idx) {
        for (var i = 0; i < groups.length; i++) {
          var nodes = el.querySelectorAll('.ts-legend-' + i);
          nodes.forEach(function(n) { n.style.display = (i === idx ? '' : 'none'); });
        }
      }
      setTimeout(function() {
        map.on('baselayerchange', function(e) {
          var idx = groups.indexOf(e.name);
          if (idx >= 0) showByIdx(idx);
        });
        showByIdx(0);   // leaflet auto-shows the first baseGroup
      }, 100);
    }",
    groupsJSON
  )
  m <- htmlwidgets::onRender(m, legendToggleJS)

  m
}

## Normalise the many shapes a user can pass into the leaflet-plotting
## functions down to a single representation: a named list of single-layer
## SpatRasters (the name labels the slider / from-to selection).
##
## Accepted inputs:
##   * SpatRaster (multi-layer): split into per-layer list, names() preserved
##   * list of single-layer SpatRaster: returned as-is
##   * simList: reads GeoTIFFs from `SpaDES.core::outputPath(x)` (shine-style)
##   * character of length 1, an existing directory: same as simList path
##
## For the simList / path cases, `name` is required -- it's the base name
## prefix of the GeoTIFF files to pick up (e.g. "simPred" matches
## "simPred_year2025.tif", "simPred_year2030.tif", ...).
## Like `.coerceToLayerList()` but returns ALL discovered objects.
##
## SpatRaster / list-of-SpatRaster inputs wrap into a single-object list
## keyed by "raster". simList / directory inputs return one entry per
## time-series object discovered under outputPath, keyed by the object name
## inferred from filenames (shine-style).
##
## Each value is a named list of single-layer SpatRasters; the names are
## the year/timestamp labels.
.coerceToMultiObjects <- function(x, timePattern = "[0-9]+") {
  if (inherits(x, "SpatRaster")) {
    nlyr <- terra::nlyr(x)
    lst <- lapply(seq_len(nlyr), function(i) x[[i]])
    names(lst) <- names(x)
    return(list(raster = lst))
  }
  if (is.list(x) && length(x) > 0L &&
      all(vapply(x, inherits, logical(1), "SpatRaster"))) {
    return(list(raster = x))
  }

  outputDir <- if (inherits(x, "simList")) {
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      stop("`SpaDES.core` is required to read a simList's outputPath",
           call. = FALSE)
    }
    SpaDES.core::outputPath(x)
  } else if (is.character(x) && length(x) == 1L && dir.exists(x)) {
    x
  } else {
    stop("`x` must be a multi-layer SpatRaster, list of SpatRasters, simList, ",
         "or a path to a directory of GeoTIFFs", call. = FALSE)
  }

  objs <- .scanOutputDirForTimeSeries(outputDir, timePattern = timePattern)
  if (!length(objs)) {
    stop("No time-series objects discovered under: ", outputDir, call. = FALSE)
  }
  ## convert each scanned df → named list of SpatRasters
  lapply(objs, function(df) {
    lst <- lapply(df$file, terra::rast)
    names(lst) <- ifelse(is.na(df$time),
                         tools::file_path_sans_ext(basename(df$file)),
                         paste0("year", df$time))
    lst
  })
}

.coerceToLayerList <- function(x, name = NULL, timePattern = "[0-9]+") {
  ## --- in-memory shapes ---
  if (inherits(x, "SpatRaster")) {
    nlyr <- terra::nlyr(x)
    lst <- lapply(seq_len(nlyr), function(i) x[[i]])
    names(lst) <- names(x)
    return(lst)
  }
  if (is.list(x) && length(x) > 0L &&
      all(vapply(x, inherits, logical(1), "SpatRaster"))) {
    return(x)
  }

  ## --- disk shapes: simList outputPath or a literal directory path ---
  outputDir <- if (inherits(x, "simList")) {
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      stop("`SpaDES.core` is required to read a simList's outputPath",
           call. = FALSE)
    }
    SpaDES.core::outputPath(x)
  } else if (is.character(x) && length(x) == 1L && dir.exists(x)) {
    x
  } else {
    stop("`x` must be a multi-layer SpatRaster, list of SpatRasters, simList, ",
         "or a path to a directory of GeoTIFFs", call. = FALSE)
  }

  if (is.null(name) || !nzchar(name)) {
    stop("`name` is required when reading from a simList/directory; ",
         "pass the base name of the output object (e.g., name = \"simPred\")",
         call. = FALSE)
  }

  objs <- .scanOutputDirForTimeSeries(outputDir, timePattern = timePattern)
  if (!name %in% names(objs)) {
    stop("name = '", name, "' not found among discovered time-series ",
         "(", paste(shQuote(names(objs)), collapse = ", "), ") under: ",
         outputDir, call. = FALSE)
  }

  df <- objs[[name]]                              # data.frame(time, file), sorted
  lst <- lapply(df$file, terra::rast)
  names(lst) <- ifelse(is.na(df$time),
                       tools::file_path_sans_ext(basename(df$file)),
                       paste0("year", df$time))
  lst
}

## Walk an output directory and group .tif files into time-series objects.
##
## Discovery/parsing logic ported from `SpaDES.shiny:::.shineScan()` so the two
## packages stay consistent: list .tif files recursively, parse the LAST regex
## match in each filename as the numeric time, treat the remainder (with
## trailing "year" word stripped) as the object key, group by key, sort by time.
##
## Returns a named list, one entry per discovered object: data.frame with
## columns `time` (numeric, sorted) and `file` (character, full path). Files
## with no time match are kept with `time = NA` (rendered as static layers).
.scanOutputDirForTimeSeries <- function(path, timePattern = "[0-9]+") {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.tif$", ignore.case = TRUE)
  files <- files[!grepl("\\.aux\\.xml$", files, ignore.case = TRUE)]
  if (!length(files)) return(list())

  parsed <- lapply(files, function(f) {
    stem <- tools::file_path_sans_ext(basename(f))
    m <- gregexpr(timePattern, stem)[[1L]]
    if (m[1L] == -1L) {
      list(key = stem, time = NA_real_, file = f)
    } else {
      i <- length(m)                                      # last match = time tag
      start <- m[i]; len <- attr(m, "match.length")[i]
      time <- suppressWarnings(as.numeric(
        substr(stem, start, start + len - 1L)))
      key <- paste0(substr(stem, 1L, start - 1L),
                    substr(stem, start + len, nchar(stem)))
      key <- sub("(?i)[ _-]*year[ _-]*$", "", key, perl = TRUE)
      key <- gsub("(^[ _-]+)|([ _-]+$)", "", key)
      list(key = key, time = time, file = f)
    }
  })

  keys <- vapply(parsed, `[[`, character(1), "key")
  out <- list()
  for (k in unique(keys)) {
    members <- parsed[keys == k]
    df <- data.frame(
      time = vapply(members, `[[`, numeric(1), "time"),
      file = vapply(members, `[[`, character(1), "file"),
      stringsAsFactors = FALSE
    )
    out[[k]] <- df[order(df$time, na.last = TRUE), , drop = FALSE]
  }
  out
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
