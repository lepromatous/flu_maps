###############################################################################
########### HEMISPHERE STUFF
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


### BASE IS 'DF' READ FROM read base data.R



##########################################################################################
##########################################################################################
#### Data management
##########################################################################################
##########################################################################################

df %>%
  group_by(country_area_or_territory, season) %>%
  mutate(
    a_total = sum(a_total, na.rm=T),
    b_total = sum(b_total, na.rm=T),
    a_h1 = sum(a_h1, na.rm=T),
    a_h1n1_pdm09 = sum(a_h1n1_pdm09, na.rm=T),
    a_h3 = sum(a_h3, na.rm=T),
    total_h1 = (a_h1 + a_h1n1_pdm09),
    
    b_victoria_lineage = sum(b_victoria_lineage, na.rm=T),
    b_yamagata_lineage = sum(b_yamagata_lineage, na.rm=T),
    total_number_of_influenza_positive_viruses = sum(total_number_of_influenza_positive_viruses, na.rm=T),
    total_number_of_influenza_negative_viruses = sum(total_number_of_influenza_negative_viruses, na.rm=T),
    
    pct_a = a_total/total_number_of_influenza_positive_viruses *100,
    pct_b = b_total/total_number_of_influenza_positive_viruses *100,
    pct_h1 = (a_h1 + a_h1n1_pdm09)/total_number_of_influenza_positive_viruses *100,
    pct_h3 = a_h3/total_number_of_influenza_positive_viruses *100,
    pct_bv = b_victoria_lineage/total_number_of_influenza_positive_viruses *100,
    pct_by = b_yamagata_lineage/total_number_of_influenza_positive_viruses *100,
    pct_positivity = total_number_of_influenza_positive_viruses/(total_number_of_influenza_positive_viruses + total_number_of_influenza_negative_viruses) *100
    
  ) %>%
  ungroup() -> df_season


### create id to merge
df_season$id<-seq(1, nrow(df_season))

source("map_bins.R")









##########################################################################################
##########################################################################################
#### MAP
##########################################################################################
##########################################################################################

mapme_strain_season<- function(datez_season=choicez[1], selection_season="pct_b"){
  df_season %>%
    filter(season == datez_season) -> df_season
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = as.data.frame(df_season[,selection_season])[,1]
  )
  
  # labels for popup in HTML
  label <- paste0( '<strong>', "Country: ", '</strong>'
                   , df_season$country_area_or_territory
                   , "<br>"
                   , '<strong>', "Total Positive Samples (%): ", '</strong>'
                   , paste0(
                     df_season$total_number_of_influenza_positive_viruses, 
                     " (", 
                     round(df_season$total_number_of_influenza_positive_viruses/
                             (df_season$total_number_of_influenza_negative_viruses +
                                df_season$total_number_of_influenza_positive_viruses)*100,1),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A (%): ", '</strong>'
                   , paste0(df_season$a_total," (", round(df_season$pct_a,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A, H1 (%): ", '</strong>'
                   , paste0(df_season$a_h1," (", round(df_season$pct_h1,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A H3 (%): ", '</strong>'
                   , paste0(df_season$a_h3," (", round(df_season$pct_h3,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B (%): ", '</strong>'
                   , paste0(df_season$b_total," (", round(df_season$pct_b,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Yamagata (%): ", '</strong>'
                   , paste0(df_season$b_yamagata_lineage," (", round(df_season$pct_by,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Victoria (%): ", '</strong>'
                   , paste0(df_season$b_victoria_lineage," (", round(df_season$pct_bv,2),")")
                   , "<br>")%>%
    lapply(htmltools::HTML)
  
  
  leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"),
                                   minZoom = -50)) %>%
    setView(lng = 0, lat = 0, zoom = 1.75) %>%
    addPolygons(data = df_season, 
                fillColor = ~pal(as.data.frame(df_season[,selection_season])[,1]),
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
              values = as.data.frame(df_season[,selection_season])[,1],
              title = paste0("Date: ", datez_season, "</br>", "Percent"),
              opacity = 1) -> map_season
  return(map_season) 
}


