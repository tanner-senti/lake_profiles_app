# Tanner Senti
# 2024-10-12

# Customized mapping function built from Utah DEQ's buildMap function within
# their wqTools package.

# Read in dummy data, remove this:
#dummy_data <- read.csv("modified_app/data/sites_test_data.csv")

library(leaflet)

buildMapAR <- function(sites,
                     au_poly,
                     bu_poly,
                     ss_poly,
                     search = c("sites",
                                "aus"),
                     plot_polys = TRUE,
                     dragging = T,
                     ...)
  {
  
  if (missing(au_poly)) {
    au_poly = wqTools::au_poly
  }
  if (missing(bu_poly)) {
    bu_poly = wqTools::bu_poly
  }
  if (missing(ss_poly)) {
    ss_poly = wqTools::ss_poly
  }
  ut_poly = wqTools::ut_poly
  wmu_poly = wqTools::wmu_poly
  
  
  au_centroids = suppressWarnings(sf::st_centroid(au_poly))
  au_centroids = cbind(au_centroids, sf::st_coordinates(au_centroids))
  
  if (missing(sites)) {
    map = leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE,
                                                    dragging = dragging))
    map = leaflet::addProviderTiles(
      map,
      "Esri.WorldImagery",
      group = "Satellite",
      options = providerTileOptions(updateWhenZooming = FALSE,
                                    updateWhenIdle = TRUE)
    )
    map = leaflet::addProviderTiles(
      map,
      "Esri.WorldTopoMap",
      group = "Topo",
      options = providerTileOptions(updateWhenZooming = FALSE,
                                    updateWhenIdle = TRUE)
    )
    map = addMapPane(map, "underlay_polygons", zIndex = 410)
    map = addMapPane(map, "huc12_poly", zIndex = 412)
    map = addMapPane(map, "au_poly", zIndex = 415)
    map = addMapPane(map, "markers", zIndex = 420)
    map = leaflet::addCircles(
      map,
      lat = au_centroids$Y,
      lng = au_centroids$X,
      group = "au_names",
      label = au_centroids$AU_NAME,
      stroke = F,
      fill = F,
      popup = paste0(
        "AU ID: ",
        au_centroids$ASSESS_ID,
        "<br> AU Name: ",
        au_centroids$AU_NAME,
        "<br> AU Type: ",
        au_centroids$AU_Type
      )
    )
    map = leaflet::addCircles(
      map,
      lat = au_centroids$Y,
      lng = au_centroids$X,
      group = "au_ids",
      label = au_centroids$ASSESS_ID,
      stroke = F,
      fill = F,
      popup = paste0(
        "AU name: ",
        au_centroids$AU_NAME,
        "<br> AU ID: ",
        au_centroids$ASSESS_ID,
        "<br> AU type: ",
        au_centroids$AU_Type
      )
    )
    if (plot_polys) {
      map = addPolygons(
        map,
        data = bu_poly,
        group = "Beneficial uses",
        fillOpacity = 0.1,
        weight = 3,
        color = "green",
        options = pathOptions(pane = "underlay_polygons"),
        popup = paste0(
          "Description: ",
          bu_poly$R317Descrp,
          "<br> Uses: ",
          bu_poly$bu_class
        )
      )
      map = addPolygons(
        map,
        data = au_poly,
        group = "Assessment units",
        fillOpacity = 0.1,
        layerId = au_poly$polyID,
        weight = 3,
        color = "orange",
        options = pathOptions(pane = "au_poly"),
        popup = paste0(
          "AU name: ",
          au_poly$AU_NAME,
          "<br> AU ID: ",
          au_poly$ASSESS_ID,
          "<br> AU type: ",
          au_poly$AU_Type
        )
      )
      map = addPolygons(
        map,
        data = ss_poly,
        group = "Site-specific standards",
        fillOpacity = 0.1,
        weight = 3,
        color = "blue",
        options = pathOptions(pane = "underlay_polygons"),
        popup = paste0("SS std: ", ss_poly$SiteSpecif)
      )
      map = addPolygons(
        map,
        data = wmu_poly,
        group = "Watershed management units",
        fillOpacity = 0.1,
        weight = 3,
        color = "red",
        options = pathOptions(pane = "underlay_polygons"),
        popup = wmu_poly$Mgmt_Unit
      )
      map = addPolygons(
        map,
        data = ut_poly,
        group = "UT boundary",
        fillOpacity = 0.1,
        weight = 3,
        color = "purple",
        options = pathOptions(pane = "underlay_polygons")
      )
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        baseGroups = c("Topo", "Satellite"),
        overlayGroups = c(
          "Assessment units",
          "Beneficial uses",
          "Site-specific standards",
          "Watershed management units",
          "UT boundary"
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE,
                                                autoZIndex = FALSE)
      )
      map = hideGroup(map, "Assessment units")
      map = hideGroup(map, "Site-specific standards")
      map = hideGroup(map, "Beneficial uses")
      map = hideGroup(map, "UT boundary")
      map = hideGroup(map, "Watershed management units")
    }
    if ("aus" %in% search) {
      map = leaflet.extras::addSearchFeatures(
        map,
        targetGroups = c("au_ids",
                         "au_names"),
        options = leaflet.extras::searchFeaturesOptions(
          zoom = 12,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      )
    }
    map = leaflet::addMeasure(
      map,
      position = "topright",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers"
    )
  }
  else {
    if (!missing(sites)) {
      site_coords = sites[, c( # Column names from Sites data file
        "AU",
        "MonitoringLocationIdentifier",
        "MonitoringLocationName",
        "MonitoringLocationTypeName",
        "LatitudeMeasure",
        "LongitudeMeasure"
      )]
      names(site_coords)[names(site_coords) == "MonitoringLocationIdentifier"] = "locationID"
      names(site_coords)[names(site_coords) == "MonitoringLocationName"] = "locationName"
      names(site_coords)[names(site_coords) == "MonitoringLocationTypeName"] = "locationType"
    }

    if (exists("site_coords")) {
        locs = site_coords
      }
    else {
      print("Uh oh, error in map function!")
    }
    
    pal <- colorRampPalette(RColorBrewer::brewer.pal(11,
                                                     "Spectral"))
    pal = leaflet::colorFactor(pal(length(unique(locs$locationType))),
                               domain = locs$locationType)
    map = leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE,
                                                    dragging = dragging), ...)
    map = leaflet::addProviderTiles(
      map,
      "Esri.WorldImagery",
      group = "Satellite",
      options = providerTileOptions(updateWhenZooming = FALSE,
                                    updateWhenIdle = TRUE)
    )
    map = leaflet::addProviderTiles(
      map,
      "Esri.WorldTopoMap",
      group = "Topo",
      options = providerTileOptions(updateWhenZooming = FALSE,
                                    updateWhenIdle = TRUE)
    )
    
    map = addMapPane(map, "underlay_polygons", zIndex = 410)
    map = addMapPane(map, "huc12_poly", zIndex = 412)
    map = addMapPane(map, "au_poly", zIndex = 415)
    map = addMapPane(map, "markers", zIndex = 420)
    map = leaflet::addCircleMarkers( # Site location circles on map:
      map,
      lat = locs$LatitudeMeasure,
      lng = locs$LongitudeMeasure,
      group = "Sites",
      color = pal(locs$locationType),
      opacity = 0.8,
      layerId = locs$locationID,
      options = pathOptions(pane = "markers"),
      popup = paste0(
        "Assessment Unit: ",
        locs$AU,
        "<br> Station ID: ",
        locs$locationID,
        "<br> Name: ",
        locs$locationName,
        "<br> Type: ",
        locs$locationType,
        "<br> Lat: ",
        locs$LatitudeMeasure,
        "<br> Long: ",
        locs$LongitudeMeasure
      )
    )
    # Possibly not needed?? ------------------------
    # map = leaflet::addCircles(
    #   map,
    #   lat = au_centroids$Y,
    #   lng = au_centroids$X,
    #   group = "au_names",
    #   label = au_centroids$AU_NAME,
    #   stroke = F,
    #   fill = F,
    #   popup = paste0(
    #     "AU ID: ",
    #     au_centroids$ASSESS_ID,
    #     "<br> AU Name: ",
    #     au_centroids$AU_NAME,
    #     "<br> AU Type: ",
    #     au_centroids$AU_Type
    #   )
    # )
    # map = leaflet::addCircles(
    #   map,
    #   lat = au_centroids$Y,
    #   lng = au_centroids$X,
    #   group = "au_ids",
    #   label = au_centroids$ASSESS_ID,
    #   stroke = F,
    #   fill = F,
    #   popup = paste0(
    #     "AU name: ",
    #     au_centroids$AU_NAME,
    #     "<br> AU ID: ",
    #     au_centroids$ASSESS_ID,
    #     "<br> AU type: ",
    #     au_centroids$AU_Type
    #   )
    # )
    # map = leaflet::addCircles(
    #   map,
    #   lat = locs$LatitudeMeasure,
    #   lng = locs$LongitudeMeasure,
    #   group = "locationID",
    #   label = locs$locationID,
    #   stroke = F,
    #   fill = F,
    #   popup = paste0(
    #     "Location ID: ",
    #     locs$locationID,
    #     "<br> Name: ",
    #     locs$locationName,
    #     "<br> Type: ",
    #     locs$locationType,
    #     "<br> Lat: ",
    #     locs$LatitudeMeasure,
    #     "<br> Long: ",
    #     locs$LongitudeMeasure
    #   )
    # )
    # map = leaflet::addCircles(
    #   map,
    #   lat = locs$LatitudeMeasure,
    #   lng = locs$LongitudeMeasure,
    #   group = "locationName",
    #   label = locs$locationName,
    #   stroke = F,
    #   fill = F,
    #   popup = paste0(
    #     "Location ID: ",
    #     locs$locationID,
    #     "<br> Name: ",
    #     locs$locationName,
    #     "<br> Type: ",
    #     locs$locationType,
    #     "<br> Lat: ",
    #     locs$LatitudeMeasure,
    #     "<br> Long: ",
    #     locs$LongitudeMeasure
    #   )
    # )
    map = leaflet::addLabelOnlyMarkers(
      map,
      group = "Labels",
      lat = locs$LatitudeMeasure,
      lng = locs$LongitudeMeasure,
      label = locs$locationID,
      labelOptions = leaflet::labelOptions(noHide = T,
                                           textsize = "15px"),
      clusterOptions = leaflet::markerClusterOptions(spiderfyOnMaxZoom = T)
    )
    # For optional map layers - lake outlines, etc.
    if (plot_polys) {
      # map = addPolygons(
      #   map,
      #   data = bu_poly,
      #   group = "Beneficial uses",
      #   fillOpacity = 0.1,
      #   weight = 3,
      #   color = "green",
      #   options = pathOptions(pane = "underlay_polygons"),
      #   popup = paste0(
      #     "Description: ",
      #     bu_poly$R317Descrp,
      #     "<br> Uses: ",
      #     bu_poly$bu_class
      #   )
      # )
      # map = addPolygons(
      #   map,
      #   data = au_poly,
      #   group = "Assessment units",
      #   fillOpacity = 0.1,
      #   layerId = au_poly$polyID,
      #   weight = 3,
      #   color = "orange",
      #   options = pathOptions(pane = "au_poly"),
      #   popup = paste0(
      #     "AU name: ",
      #     au_poly$AU_NAME,
      #     "<br> AU ID: ",
      #     au_poly$ASSESS_ID,
      #     "<br> AU type: ",
      #     au_poly$AU_Type
      #   )
      # )
      # map = addPolygons(
      #   map,
      #   data = ss_poly,
      #   group = "Site-specific standards",
      #   fillOpacity = 0.1,
      #   weight = 3,
      #   color = "blue",
      #   options = pathOptions(pane = "underlay_polygons"),
      #   popup = paste0("SS std: ", ss_poly$SiteSpecif)
      # )
      # map = addPolygons(
      #   map,
      #   data = ut_poly,
      #   group = "UT boundary",
      #   fillOpacity = 0.1,
      #   weight = 3,
      #   color = "purple",
      #   options = pathOptions(pane = "underlay_polygons")
      # )
      # map = addPolygons(
      #   map,
      #   data = wmu_poly,
      #   group = "Watershed management units",
      #   fillOpacity = 0.1,
      #   weight = 3,
      #   color = "red",
      #   options = pathOptions(pane = "underlay_polygons"),
      #   popup = wmu_poly$Mgmt_Unit
      # )
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        baseGroups = c("Topo", "Satellite"),
        overlayGroups = c( # Include these if using the layers above:
          #"Assessment units", 
         # "Beneficial uses",
        #  "Site-specific standards",
         # "Watershed management units",
        #  "UT boundary",
          "Sites",
          "Labels"
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE,
                                                autoZIndex = FALSE)
      )
     # map = hideGroup(map, "Assessment units")
    #  map = hideGroup(map, "Site-specific standards")
    #  map = hideGroup(map, "Beneficial uses")
      map = hideGroup(map, "Labels")
    #  map = hideGroup(map, "UT boundary")
    #  map = hideGroup(map, "Watershed management units")
    }
    else {
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        baseGroups = c("Topo", "Satellite"),
        overlayGroups = c("Sites",
                          "Labels"),
        options = leaflet::layersControlOptions(collapsed = TRUE,
                                                autoZIndex = FALSE)
      )
      map = hideGroup(map, "Labels")
    }
    map = leaflet::addLegend(
      map,
      position = "topright",
      colors = unique(pal(locs$locationType)),
      labels = unique(locs$locationType)
    )
    if ("sites" %in% search & "aus" %in% search) {
      map = leaflet.extras::addSearchFeatures(
        map,
        targetGroups = c("au_ids",
                         "au_names", "locationID", "locationName"),
        options = leaflet.extras::searchFeaturesOptions(
          zoom = 12,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      )
    }
    if ("sites" %in% search & !"aus" %in% search) {
      map = leaflet.extras::addSearchFeatures(
        map,
        targetGroups = c("locationID",
                         "locationName"),
        options = leaflet.extras::searchFeaturesOptions(
          zoom = 12,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      )
    }
    if (!"sites" %in% search & "aus" %in% search) {
      map = leaflet.extras::addSearchFeatures(
        map,
        targetGroups = c("au_ids",
                         "au_names"),
        options = leaflet.extras::searchFeaturesOptions(
          zoom = 12,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      )
    }
    map = leaflet::addMeasure(map, position = "topright")
    map = leaflet::fitBounds(
      map,
      min(locs$LongitudeMeasure) *
        0.999,
      min(locs$LatitudeMeasure) * 0.999,
      max(locs$LongitudeMeasure) *
        1.001,
      max(locs$LatitudeMeasure) * 1.001
    )
  }
  return(map)
}

# Test run function, delete this:
#buildMapAR(sites = dummy_data)

