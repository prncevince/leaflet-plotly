# A leaflet package to analyze geospatial data

# globals
min_lin <- function(...) {
  .x <- min(...)
  if (is.finite(.x)) .x else 0
}
min_log <- function(...) {
  .x <- log(x = min(...))
  if (is.finite(.x)) .x else 0
}

pal_reg_lin <- function(.n, .p, .r) {
  breaks <- seq(min_lin(.n), max(.n), length.out = 11)
  colorBin(.p, bins = breaks, reverse = .r, na.color = "#ffffff")
}
pal_wac_lin <- function(.n, .p, .r) {
  breaks <- seq(min_lin(.n), max(.n), length.out = 11)
  colorBin(.p, bins = breaks, reverse = .r, na.color = "#ffffff")
}
pal_reg_log <- function(.n, .p, .r) {
  breaks <- exp(seq(
    from = min_log(.n), to = log(1+max(.n)), length.out = 11
  ))
  colorBin(.p, bins = breaks, reverse = .r, na.color = "#ffffff")
}
pal_wac_log <- function(.n, .p, .r) {
  breaks <- exp(seq(
    from = min_log(.n), to = log(max(.n)), length.out = 11
  ))
  colorBin(.p, bins = breaks, reverse = .r, na.color = "#ffffff")
}

#' Create a Leaflet Map plotting lat/lons and counts of sf polygons. (e.g. WACs, Countries, etc.)
#' 
#' @param d_sf_wac sf data.frame of WACs & metadata
#' \describe{
#'   \item{ID}{The WAC index number}
#'   \item{n}{A count e.g. image footprints counts for the WACs}
#'   \item{*}{sf geometry class. Typically `geometry`}
#' }
#' @param d_sf_reg sf data.frame of WACs & metadata
#' \describe{
#'   \item{geounit}{The Region Name}
#'   \item{n}{A count e.g. image footprints counts for the region}
#'   \item{*}{sf geometry class. Typically `geometry`}
#' }
#' @param basemap single item named list - item name is map provider - value is tile source
#' @param pal - the palette - see `?leaflet::colorBin` 
#' @param pal_scale - character - value within `c("linear", "log")` - sets color palette scale
#' @param pal_reverse - logical - to reverse the color palette or not
leaflet_map <- function(
  d_sf_wac, d_sf_reg, basemap = list(osm = c("raster", "wms")),
  pal = "viridis", pal_scale = c("linear", "log"), pal_reverse = F
){
  # Setters ----
  ## Choropleth Color Palette ----
  l_scale <- list(
    linear = list(
      legend = " - Linear Scale",
      pal_reg = pal_reg_lin,
      pal_wac = pal_wac_lin
    ),
    log = list(
      legend = " - Log Scale",
      pal_reg = pal_reg_log,
      pal_wac = pal_wac_log
    )
  )
  ## Basemap
  l_basemap <- list(
    osm = list(
      raster = list(
        url = "https://osm-{s}.gs.mil/tiles/{crs}/{z}/{x}/{y}.png",
        # subdomains in .mil Tile Layer CRS - https://osm.gs.mil/features/base-map
        layers = c(
          'default', 'bright', 'humanitarian',
          'defualt_pc', 'bright_pc', 'humanitarian_pc'
        )
      ),
      wms = list(),
      bg = c('default', 'bright', 'humanitarian', 'defualt pc', 'bright pc', 'humanitarian pc')
    ))
  # Getters ----
  ## Choropleth Color Palette ----
  # these functions need at least 1 input variable specified - otherwise do not get called properly
  # wac color palette function
  pal_wac <- l_scale[[pal_scale]]$pal_wac(.n = d_sf_wac$n, .p = pal, .r = pal_reverse)
  # region color pallette function
  pal_reg <- l_scale[[pal_scale]]$pal_reg(.n = d_sf_reg$n, .p = pal, .r = pal_reverse)
  legend <- l_sclae[[pal_scale]]$legend # legend title
  ## Basemap ----
  bn <- names(basemap) # basemap name
  bs <- basemap[[1]] # basemap source
  burl <- l_basemap[[bn]][[bs]]$url # basemap url template
  bcrs <- l_basemap[[bn]][[bs]]$layers # basemap crs tile option
  bgrp <- paste("tiles", l_basemap[[bn]]$bg) # basemap group names
  # Leaflet Map ----
  map <-
    leaflet(
      options = leafletOptions(worldCopyJump = T)
    ) %>%
    addTiles(
      urlTemplate = burl, group = bgrp[1],
      options = tileOptions(subdomains = "1234", crs = bcrs[1])
    ) %>%
    addTiles(
      urlTemplate = burl, group = bgrp[2],
      options = tileOptions(subdomains = "1234", crs = bcrs[2])
    ) %>%
    addTiles(
      urlTemplate = burl, group = bgrp[3],
      options = tileOptions(subdomains = "1234", crs = bcrs[3])
    ) %>%
    addPolygons(
      data = d_sf_wac, group = "WAC", fillOpacity = 0.3,
      fillColor = ~suppressWarnings(pal_wac(n)),
      opacity = 0.1, color = "white", weight = 2,
      popup = ~paste(
        "WAC: ", ID, "<br>",
        "Image Count: ", n, "<br>"
      ),
      highlightOptions = highlightOptions(
        opacity = 1, fillOpacity = 0.5
      )
    ) %>%
    addPolygons(
      data = d_sf_reg, group = "Region", fillOpacity = 0.3,
      fillColor = ~suppressWarnings(pal_reg(n)),
      opacity = 0.1, color = "black", weight = 2,
      popup = ~paste(
        "Region: ", geounit, "<br>",
        "Image Count: ", n, "<br>"
      ),
      highlightOptions = highlightOptions(
        opacity = 1, fillOpacity = 0.5
      )
    ) %>%
    addLegend(
      data = d_sf_wac, group = "WAC",
      pal = pal_wac, title = paste0("Image Count", legend),
      values = ~n, "bottomleft", opacity = 0.7
    ) %>%
    addLegend(
      data = d_sf_reg, group = "Region",
      pal = pal_reg, title = paste0("Image Count", legend),
      values = ~n, "bottomleft", opacity = 0.7
    ) %>%
    addLayersControl(
      baseGroups = c(bgrp[1:3]),
      overlayGroups = c("WAC", "Region")
    ) %>%
    hideGroup("Region") %>%
    setView(0, 0, 1)
  return(map)
}
