#######################################################
#### merge map and data
#######################################################
map_adds <- function(data){
        data$country_area_or_territory <- stringr::str_replace(data$country_area_or_territory, " \\s*\\([^\\)]+\\)", "")
        data$country_area_or_territory <- gsub(")", "", data$country_area_or_territory)
        
        data$code <- countrycode(
          sourcevar = data$country_area_or_territory,
          origin = "country.name",
          destination = "iso3c",
          warn = TRUE,
          nomatch = NA,
          custom_dict = NULL,
          custom_match = NULL,
          origin_regex = T
        )
        
        data$code[data$country_area_or_territory=="Kosovo"] <- "KOS"
        
        
        #######################################################
        ####### PULL MAP AND USE WINKEL TRIPEL PROJECTION
        #######################################################
        library(tmap)
        data("World")
        # crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
        # world_wintri <- st_transform(World, crs = crs_wintri)
        
        data <- merge(World, data, by.x="iso_a3", by.y="code", all.x=T)
        
        
        data <- subset(data, !is.na(data$end_date))
        
        ##### merge in hemisphere
        
        hemi <- read.csv("hemi.csv")
        
        data <- merge(data, hemi, by="iso_a3", all.x=T)
        
        
        ### make flu seasons
        data$season <- factor(ifelse(lubridate::month(data$end_date)%in%c(11,12,1,2,3,4), 0,1), levels=c(0,1), labels=c("Northern Season", "Southern Season"))
        
        ### 
        data$season <- factor(ifelse(data$season=="Northern Season", 
                                   paste0(data$season, "-", lubridate::year(data$end_date),"/", lubridate::year(data$end_date)+1),
                                   paste0(data$season, "-", lubridate::year(data$end_date)-1,"/", lubridate::year(data$end_date))))
        
        ### drop 2008/2009 b/c its partial year
        data <- subset(data, data$season !="Southern Season-2008/2009")
        
        
        north.season <- levels(data$season)[levels(data$season) %like% "Northern"]
        south.season <- levels(data$season)[levels(data$season) %like% "Southern"]
        
        choicez <- c(rbind(south.season, north.season))

  return(list(data, choicez))

}
