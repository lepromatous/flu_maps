library(tidyverse)
library(vroom)
library(janitor)
library(countrycode)
library(stringr)
library(zoo)
library(rgeos)
library(tmap)
library(tmaptools)
#tmap_mode("view")
library(leaflet)
library(lubridate)
library(data.table)

### for testing... dont need this in production as run in app.R
#setwd("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/")
#source("read base data.R")
#setwd("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/")
### compute new values
df%>%
  group_by(country_area_or_territory, moyr) %>%
  mutate(
    a_total = sum(a_total, na.rm=T),
    b_total = sum(b_total, na.rm=T),
    a_h1 = sum(a_h1, na.rm=T),
    a_h1n1_pdm09 = sum(a_h1n1_pdm09, na.rm=T),
    a_h3 = sum(a_h3, na.rm=T),
    b_victoria_lineage = sum(b_victoria_lineage, na.rm=T),
    b_yamagata_lineage = sum(b_yamagata_lineage, na.rm=T),
    total_number_of_influenza_positive_viruses = sum(total_number_of_influenza_positive_viruses, na.rm=T),
    total_number_of_influenza_negative_viruses = sum(total_number_of_influenza_negative_viruses, na.rm=T),
  ) %>%
  slice(n()) %>%
  ungroup() -> df_global


df_global %>%
  mutate(
    pct_a = a_total/(total_number_of_influenza_positive_viruses) *100,
    pct_b = b_total/(total_number_of_influenza_positive_viruses) *100,
    pct_h1 = (a_h1 + a_h1n1_pdm09)/(total_number_of_influenza_positive_viruses) *100,
    pct_h3 = a_h3/(total_number_of_influenza_positive_viruses) *100,
    pct_bv = b_victoria_lineage/(total_number_of_influenza_positive_viruses) *100,
    pct_by = b_yamagata_lineage/(total_number_of_influenza_positive_viruses) *100,
    pct_positivity = total_number_of_influenza_positive_viruses/(total_number_of_influenza_positive_viruses + total_number_of_influenza_negative_viruses) *100,
    total_h1 = (a_h1 + a_h1n1_pdm09)
  ) -> df_global


source("map_bins.R")



##############################################################################################################
##############################################################################################################
#### map
##############################################################################################################
##############################################################################################################

mapme_strain<- function(datez=max(df_global$moyr, na.rm=T), selection="pct_b"){
  df_global %>%
    filter(moyr == datez) -> df_global
  
  
  
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = as.data.frame(df_global[,selection])[,1]
  )
  
  # labels for popup in HTML
  label <- paste0( '<strong>', "Country: ", '</strong>'
                   , df_global$country_area_or_territory
                   , "<br>"
                   , '<strong>', "Total Positive Samples (%): ", '</strong>'
                   , paste0(
                     df_global$total_number_of_influenza_positive_viruses, 
                     " (", 
                     round(df_global$total_number_of_influenza_positive_viruses/
                             (df_global$total_number_of_influenza_negative_viruses +
                                df_global$total_number_of_influenza_positive_viruses)*100,1),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A (%): ", '</strong>'
                   , paste0(df_global$a_total," (", round(df_global$pct_a,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A, H1 (%): ", '</strong>'
                   , paste0(df_global$a_h1," (", round(df_global$pct_h1,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A H3 (%): ", '</strong>'
                   , paste0(df_global$a_h3," (", round(df_global$pct_h3,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B (%): ", '</strong>'
                   , paste0(df_global$b_total," (", round(df_global$pct_b,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Yamagata (%): ", '</strong>'
                   , paste0(df_global$b_yamagata_lineage," (", round(df_global$pct_by,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Victoria (%): ", '</strong>'
                   , paste0(df_global$b_victoria_lineage," (", round(df_global$pct_bv,2),")")
                   , "<br>")%>%
    lapply(htmltools::HTML)
  
  
  leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"),
                                   minZoom = -50)) %>%
    setView(lng = 0, lat = 0, zoom = 1.75) %>%
    addPolygons(data = df_global, 
                fillColor = ~pal(as.data.frame(df_global[,selection])[,1]),
                weight = 0.2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = label) %>%
    addPolylines(data = World,
                 fillColor = NA,
                 weight = 0.7,
                 color = "black") %>%
    addLegend("bottomright", 
              pal = pal, 
              values = as.data.frame(df_global[,selection])[,1],
              title = paste0("Date: ", datez, "</br>", "Percent"),
              opacity = 1) ->map
    # addGraticule(interval=23.5, 
    #              style = list(color = "black", weight = .6, opacity=0.7),
    #              options = pathOptions(pointerEvents = "none", clickable = T))-> map
  return(map) 
}

