library(tidyverse)
library(vroom)
library(janitor)
library(countrycode)
library(stringr)
library(zoo)
library(rgeos)

#######################################################
### read flu data from Jacob and clean
### data scrape from:  https://apps.who.int/flumart/Default?ReportNo=12
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6086842/
###https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0193263
#######################################################
df <- vroom::vroom("https://quartzpfizer.s3.amazonaws.com/who-influenza.csv")

df %>%
  janitor::clean_names() %>%
  mutate(
    moyr = zoo::as.yearmon(end_date)
    )%>%
   filter(year >=1998
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
    pct_positivity = total_number_of_influenza_positive_viruses/(total_number_of_influenza_positive_viruses + total_number_of_influenza_negative_viruses) *100
    
    ) -> df

#######################################################
####### PULL MAP AND USE WINKEL TRIPEL PROJECTION
#######################################################
library("rworldmap")
library("sf")
library("lwgeom")
library("cowplot")
## pull map
world_sf <- st_as_sf(getMap(resolution = "low"))
## set crs to winkel tripel
crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
world_wintri <- st_transform_proj(world_sf, crs = crs_wintri)



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

df <- merge(world_wintri, df, by.x="ISO_A3", by.y="code", all=T)


#######################################################
#### map
#######################################################
mapme_strain<- function(datez=min(df$moyr, na.rm=T), selection="pct_b"){
  p<-ggplot(df[df$moyr == datez,]) + 
      geom_sf(data=world_wintri, colour="black", fill="white") +
      geom_sf(size = 0.5/.pt, aes_string(fill=selection), colour="black") +
      coord_sf(datum = NULL) +
      scale_fill_distiller(guide="colorbar",
                           na.value="lightgray",
                           direction = 1,
                           name = "Percent",
                           limits = c(0, 100)) +
      #labs(title = datez) +  ## {closest_state}
    theme(
      panel.background = element_blank(),
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      plot.title = element_text(hjust = 1, size=25)
    ) 
return(p)
}

mapme_strain()
