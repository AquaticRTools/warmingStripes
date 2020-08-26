getwd()




# load packages -----------------------------------------------------------

library(tools) 
library(USAboundaries)
library(sf)
library(janitor)
library(dataRetrieval)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(shiny)
library(lubridate)
library(tidyverse)
library(tidylog)



# import dataset --------------------------------------------------------

Wtempstats_df <- readRDS('Wtempstats.rds')

Wtempstats_df
dim(Wtempstats_df) # 5007   23
summary(Wtempstats_df)
length(unique(Wtempstats_df$site_no)) # 224



# manipulate for mapping df 

Wtempstats_map <- 
  Wtempstats_df %>% 
  ungroup() %>% 
  group_by(site_no, station_nm, dec_long_va, dec_lat_va, drain_area_va, STATE_NAME, n_year, state_site) %>% 
  # distinct(site_no, station_nm, dec_long_va, dec_lat_va, drain_area_va, STATE_NAME, n_year, state_site)
  summarize(minYear = min(year),
            maxYear = max(year)) %>% 
  mutate(
    dateRange = paste0(minYear, ' - ', maxYear),
    website = paste0('https://waterdata.usgs.gov/nwis/inventory/?site_no=', site_no)) %>% # prep for adding hyperlink to leaflet popup
  

  select(-c(minYear, maxYear)) %>% 
  
  arrange(state_site)

  
  
Wtempstats_map
summary(Wtempstats_map)


# state boundaries
us_sta_bnd <- USAboundaries::us_states(resolution = "low", states = NULL) %>% 
  filter(!(jurisdiction_type %in% c('territory', 'district')))

plot(st_geometry(us_sta_bnd))
us_sta_geom <- us_sta_bnd$geometry



# leaflet map with symbology --------------------------------------------------

## define color palette
pal = colorNumeric("YlGnBu", domain = Wtempstats_map$n_year)


# create leaflet map

gageTempMap <-
  
  Wtempstats_map %>% 
  
  leaflet() %>% 
  
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>% # basemap
  addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>% # aerial map
  
  addResetMapButton() %>%
  
  addSearchOSM() %>%
  
  # ## add state boundaries
  addPolygons(data = us_sta_bnd$geometry,
              color = "#737373",
              fill = F,
              weight = 2) %>%
  
  fitBounds(-170, 20, -46, 72) %>% 
  
  addCircleMarkers(
    lng = ~dec_long_va, lat = ~dec_lat_va,
    radius = 8, stroke = TRUE, col = '#252525', weight = 1, 
    fill = TRUE, fillColor = ~ pal(n_year), fillOpacity = 0.9,
    label = ~ station_nm,
    popup = ~ 
      paste("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:250px'", 
            '<br>',
            '<b>', 'State:  ', '</b>', STATE_NAME, "<br>",
            '<b>', 'Station Name:  ', '</b>', station_nm, "<br>",
            '<b>', 'USGS Site Number:  ', '</b>', site_no, 
            ' ; <a href=', website, 'target=\"_blank\">website</a>', "<br>", # the target it is necessary to open as new tab in html browser
            '<br>',
            '<b>', 'Period of Record (years):  ', '</b>', n_year, "<br>",
            '<b>', 'Date Range:  ', '</b>', dateRange, "<br>",
            '<br>',
            '<b>', 'Drainage Area (sq.mi.):  ', '</b>', drain_area_va, "<br>",
            '<b>', 'Latitude (dd):  ', '</b>', dec_lat_va, "<br>",
            '<b>', 'Longitude (dd):  ', '</b>', dec_long_va, "<br>")) %>%
  
  addLegend(pal = pal, values = ~n_year, opacity = 0.8,
            title = 'Period of Record (years)') %>% 
  
  # add scale bar
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE)) %>% 
  
  # Layers control
  addLayersControl(
    baseGroups = c("Topographic", "Aerial"),
    options = layersControlOptions(collapsed = FALSE))


gageTempMap














