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
# 
# ##https://worldpopulationreview.com/country-rankings/tropical-countries
# # north <- c("Algeria", "Austria", "Belgium", "Bulgaria", "Canada", "China", "Egypt",
# #            "France", "Germany", "Hungary", "India", "Japan", "Libya", "Mexico", "Morocco",
# #            "Netherlands", "Norway", "Poland", "Romania", "Russia", "Spain", 
# #            "Sweden", "Switzerland", "United Kingdom", "Ukraine", "United States")
# south <- data.frame("V1" = c("Antarctica", "Argentina", "Australia", "Botswana", "Chile","Eswatini","Falkland Islands", "Fiji",
#                        "French Southern Territories", "Lesotho", "Mauritania", "Namibia", "New Caledonia",
#                        "New Zealand", "Papua New Guinea","Solomon Islands", "South Africa","Vanuatu", "Uruguay", 
#                        "Zimbabwe"))
# tropical <- data.frame("V1" = c("Angola", "Bahamas", "Bangladesh", "Belize", "Benin", "Brazil", "Bolivia", "Burundi", "Burkina Faso",
#                          "Cameroon", 
#                           "Cambodia", "Central African Republic",
#                           "Chad", "Colombia", "Costa Rica", "Cuba", "Democratic Republic of Congo",
#                           "Dominican Republic", "Djibouti", "Ecuador", "Equatorial Guinea", "Eritrea", "Ethiopia", 
#                           "French Guyana", "French Guiana", "Gabon", "Ghana", "Guinea", "Guinea-Bissau", "Guyana",
#                           "Guatemala", "Haiti", "Honduras", "India", "Indonesia", "Ivory Coast", "Jamaica", "Kenya",
#                           "Laos", "Liberia", "Madagascar", "Malaysia", "Malawi", "Mali","Mauritania", "Mexico", 
#                           "Mozambique","Myanmar", "Nicaragua", "Niger", "Nigeria", "Panama", "Paraguay", "Peru", 
#                           "Philippines",  "Puerto Rico", "Republic of Congo", "Rwanda", "Senegal", "Sierra Leone", 
#                           "Somalia",  "South Sudan", "Sri Lanka", "Sudan", "Suriname", "Taiwan", "Tanzania", "Thailand", "Togo", "Uganda", 
#                           "Venezuela", "Vietnam", "Zambia"))
# 
# 
# south$code <- countrycode(
#   sourcevar = south$V1,
#   origin = "country.name",
#   destination = "iso3c",
#   warn = TRUE,
#   nomatch = NA,
#   custom_dict = NULL,
#   custom_match = NULL,
#   origin_regex = T
# )
# 
# tropical$code <- countrycode(
#   sourcevar = tropical$V1,
#   origin = "country.name",
#   destination = "iso3c",
#   warn = TRUE,
#   nomatch = NA,
#   custom_dict = NULL,
#   custom_match = NULL,
#   origin_regex = T
# )
# 

#######################################################
### read flu data from Jacob and clean
### data scrape from:  https://apps.who.int/flumart/Default?ReportNo=12
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6086842/
###https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0193263
#######################################################
df <- vroom::vroom("https://quartzpfizer.s3.amazonaws.com/who-influenza.csv")

df$`Country, area or territory`[df$`Country, area or territory`=="Venezuela (Bolivarian Republic of)"]<-"Venezuela"

df %>%
  janitor::clean_names() %>%
  mutate(
    moyr = zoo::as.yearmon(end_date)
  )%>%
  filter(end_date >="2009-10-01"
  ) -> df

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
  ungroup() -> df


df %>%
  mutate(
    #   across(everything(), gsub, pattern = "&nbsp;", replacement = ""),
    #        ) %>%
    # mutate(
    pct_a = a_total/(total_number_of_influenza_positive_viruses) *100,
    pct_b = b_total/(total_number_of_influenza_positive_viruses) *100,
    pct_h1 = (a_h1 + a_h1n1_pdm09)/(total_number_of_influenza_positive_viruses) *100,
    pct_h3 = a_h3/(total_number_of_influenza_positive_viruses) *100,
    pct_bv = b_victoria_lineage/(total_number_of_influenza_positive_viruses) *100,
    pct_by = b_yamagata_lineage/(total_number_of_influenza_positive_viruses) *100,
    pct_positivity = total_number_of_influenza_positive_viruses/(total_number_of_influenza_positive_viruses + total_number_of_influenza_negative_viruses) *100,
    total_h1 = (a_h1 + a_h1n1_pdm09)
  ) -> df



#######################################################
#### merge map and DF
#######################################################

df$country_area_or_territory <- stringr::str_replace(df$country_area_or_territory, " \\s*\\([^\\)]+\\)", "")
df$country_area_or_territory <- gsub(")", "", df$country_area_or_territory)

df$code <- countrycode(
  sourcevar = df$country_area_or_territory,
  origin = "country.name",
  destination = "iso3c",
  warn = TRUE,
  nomatch = NA,
  custom_dict = NULL,
  custom_match = NULL,
  origin_regex = T
)

df$code[df$country_area_or_territory=="Kosovo"] <- "KOS"


#######################################################
####### PULL MAP AND USE WINKEL TRIPEL PROJECTION
#######################################################
library(tmap)
data("World")
# crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
# world_wintri <- st_transform(World, crs = crs_wintri)

df <- merge(World, df, by.x="iso_a3", by.y="code", all.x=T)




# df$hemisphere<- ifelse(df$iso_a3%in%south$code, "Southern", 
#                        ifelse(df$iso_a3%in%tropical$code, "Tropical", "Northern"))
# 
# 
# ggplot()+
#   geom_sf(data=df, aes(fill=hemisphere))
#hemi <- read.csv("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/Pre work/hemi.csv")
##### merge in hemisphere

hemi <- read.csv("hemi.csv")

df <- merge(df, hemi, by="iso_a3", all.x=T)



df$season <- factor(ifelse(lubridate::month(df$end_date)%in%c(11,12,1,2,3,4), 0,1), levels=c(0,1), labels=c("Northern Season", "Southern Season"))

df$season <- factor(paste0(df$season, "-", lubridate::year(df$end_date),"/", lubridate::year(df$end_date)+1))


#######################################################
#### map
#######################################################

# mapme_strain<- function(datez=min(df$moyr, na.rm=T), selection="pct_b"){
#   df %>%
#     filter(moyr == datez) -> df
#   p<- leaflet(df) %>%
#       tm_polygons(selection, title="Percent",
#                   title.col="Country/Territory", id="country_area_or_territory", 
#                   popup.vars=c("n Positive Samples"="total_number_of_influenza_positive_viruses", 
#                                "n Influenza A"="a_total", 
#                                "% Influenza A"="pct_a",
#                                
#                                "n Influenza A H1"="total_h1",
#                                "% Influenza A H1"="pct_h1",
#                                
#                                "n Influenza A H1"="a_h3",
#                                "% Influenza A H3"="pct_h3",
#                                
#                                "n Influenza B"="b_total",
#                                "% Influenza B"="pct_b",
#                                
#                                "n Influenza B Victoria"="b_victoria_lineage",
#                                "% Influenza B Victoria"="pct_bv",
#                                
#                                "n Influenza B Yamagata"="b_yamagata_lineage",
#                                "% Influenza B Yamagata"="pct_by"
#                                ),
#                   popup.format=list(pct_a=list(digits=2),
#                                     pct_h1=list(digits=2),
#                                     pct_h3=list(digits=2),
#                                     pct_b=list(digits=2),
#                                     pct_by=list(digits=2),
#                                     pct_bv=list(digits=2)
#                                     )) +
#     tm_view(set.view = c(2, 51, 2)) # longitude 7, latitude 51, zoom 4
# return(p)
# }






#######################################################
#### map
#######################################################

mapme_strain<- function(datez=max(df$moyr, na.rm=T), selection="pct_b"){
  df %>%
    filter(moyr == datez) -> df
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = as.data.frame(df[,selection])[,1]
  )
  
  # labels for popup in HTML
  label <- paste0( '<strong>', "Country: ", '</strong>'
                   , df$country_area_or_territory
                   , "<br>"
                   , '<strong>', "Total Positive Samples (%): ", '</strong>'
                   , paste0(
                     df$total_number_of_influenza_positive_viruses, 
                     " (", 
                     round(df$total_number_of_influenza_positive_viruses/
                             (df$total_number_of_influenza_negative_viruses +
                                df$total_number_of_influenza_positive_viruses)*100,1),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A (%): ", '</strong>'
                   , paste0(df$a_total," (", round(df$pct_a,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A, H1 (%): ", '</strong>'
                   , paste0(df$a_h1," (", round(df$pct_h1,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza A H3 (%): ", '</strong>'
                   , paste0(df$a_h3," (", round(df$pct_h3,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B (%): ", '</strong>'
                   , paste0(df$b_total," (", round(df$pct_b,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Yamagata (%): ", '</strong>'
                   , paste0(df$b_yamagata_lineage," (", round(df$pct_by,2),")")
                   , "<br>"
                   , '<strong>', "Total Influenza B, Victoria (%): ", '</strong>'
                   , paste0(df$b_victoria_lineage," (", round(df$pct_bv,2),")")
                   , "<br>")%>%
    lapply(htmltools::HTML)
  
  
  leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"),
                                   minZoom = -50)) %>%
    setView(lng = 0, lat = 0, zoom = 1.75) %>%
    addPolygons(data = df, 
                fillColor = ~pal(as.data.frame(df[,selection])[,1]),
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
              values = as.data.frame(df[,selection])[,1],
              title = paste0("Date: ", datez, "</br>", "Percent"),
              opacity = 1) -> map
  return(map) 
}


min(df$pct_a, na.rm=T)
