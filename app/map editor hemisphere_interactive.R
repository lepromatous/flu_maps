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



### read map editor global interactive.R first to get df

df %>%
  group_by(hemi, moyr) %>%
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
  ungroup() -> df_hem











##############################################################################################################
##############################################################################################################
#### map
##############################################################################################################
##############################################################################################################




mapme_strain_hemi<- function(datez_hem=max(df_hem$moyr, na.rm=T), selection_hem="pct_b"){
  df_hem %>%
    filter(moyr == datez_hem) -> df_hem
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = as.data.frame(df_hem[,selection_hem])[,1]
  )
  
  # labels for popup in HTML
  label <- paste0( '<strong>', "Hemisphere: ", '</strong>'
                   , str_to_title(df_hem$hemi)
                   , "<br>"
                   , '<strong>', "Total Positive Samples (%): ", '</strong>'
                   , paste0(
                     df_hem$total_number_of_influenza_positive_viruses, 
                     " (", 
                     round(df_hem$total_number_of_influenza_positive_viruses/
                             (df_hem$total_number_of_influenza_negative_viruses +
                                df_hem$total_number_of_influenza_positive_viruses)*100,1),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A (%): ", '</strong>'
                   , paste0(df_hem$a_total," (", round(df_hem$pct_a,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A, H1 (%): ", '</strong>'
                   , paste0(df_hem$a_h1," (", round(df_hem$pct_h1,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A H3 (%): ", '</strong>'
                   , paste0(df_hem$a_h3," (", round(df_hem$pct_h3,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B (%): ", '</strong>'
                   , paste0(df_hem$b_total," (", round(df_hem$pct_b,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Yamagata (%): ", '</strong>'
                   , paste0(df_hem$b_yamagata_lineage," (", round(df_hem$pct_by,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Victoria (%): ", '</strong>'
                   , paste0(df_hem$b_victoria_lineage," (", round(df_hem$pct_bv,2),")")
                   , "<br>")%>%
    lapply(htmltools::HTML)
  
  
  leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"),
                                   minZoom = -50)) %>%
    setView(lng = 0, lat = 0, zoom = 1.75) %>%
    addPolygons(data = df_hem, 
                fillColor = ~pal(as.data.frame(df_hem[,selection_hem])[,1]),
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
              values = as.data.frame(df_hem[,selection_hem])[,1],
              title = paste0("Period: ", datez_hem, "</br>", "Percent"),
              opacity = 1) -> map_hem
  return(map_hem) 
}

mapme_strain_hemi()