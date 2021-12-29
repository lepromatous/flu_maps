
#######################################################
### read flu df from Jacob and clean
### df scrape from:  https://apps.who.int/flumart/Default?ReportNo=12
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6086842/
###https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0193263
#######################################################
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

options(warn=-1)

##########################################################################################
##########################################################################################
#### read and manage
##########################################################################################
##########################################################################################
setwd("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/")

df <- vroom::vroom("https://quartzpfizer.s3.amazonaws.com/who-influenza.csv")

df$`Country, area or territory`[df$`Country, area or territory`=="Venezuela (Bolivarian Republic of)"]<-"Venezuela"

df %>%
  janitor::clean_names() %>%
  mutate(
    moyr = zoo::as.yearmon(end_date)
  )%>%
  filter(end_date >="2009-10-01"
  ) -> df

df <- subset(df, !is.na(end_date))

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


df <- subset(df, !is.na(df$end_date))

##### merge in hemisphere
#hemi <- read.csv("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/hemi.csv")
hemi <- read.csv("hemi.csv")

df <- merge(df, hemi, by="iso_a3", all.x=T)


### make flu seasons
df$season <- factor(ifelse(lubridate::month(df$end_date)%in%c(11,12,1,2,3,4), 0,1), levels=c(0,1), labels=c("Northern Season", "Southern Season"))

### 
df$season <- factor(ifelse(df$season=="Northern Season", 
                             paste0(df$season, "-", lubridate::year(df$end_date),"/", lubridate::year(df$end_date)+1),
                             paste0(df$season, "-", lubridate::year(df$end_date)-1,"/", lubridate::year(df$end_date))))

### drop 2008/2009 b/c its partial year
df <- subset(df, df$season !="Southern Season-2008/2009")
df$season<-factor(df$season)


north.season <- levels(df$season)[levels(df$season) %like% "Northern"]
south.season <- levels(df$season)[levels(df$season) %like% "Southern"]

choicez <- c(rbind(south.season, north.season))
