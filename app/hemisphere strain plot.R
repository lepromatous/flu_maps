
library(tidyverse)
library(ranger)
library(viridis)
library(sf)
library(grid)
library(plotly)
library(ggplotify)

# setwd("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/")
# source("read base data.R")
# source("map editor hemisphere_season_interactive.R")

test <- df_season_hem
st_geometry(test)<- NULL

test %>%
  mutate(
    area = gsub("[km^2]", "", area),
    economy = factor(economy),
    income_grp = factor(income_grp),
    who_region = factor(who_region),
    influenza_transmission_zone = factor(influenza_transmission_zone)
  )  -> test

test %>%
  group_by(hemi, season) %>%
  slice(n()) %>%
  ungroup() -> test2


# New facet label names for supp variable
fac.labs <- c("Northern", "Southern", "Tropical")
names(fac.labs) <- c("northern", "southern", "tropics")



strain_trajectory <- function(strain_traje="pct_b"){

         
          ggplot() + 
            
            geom_smooth(data = test2, 
                        aes_string(x="moyr", 
                            y=strain_traje, color="hemi", fill="hemi"), show.legend = F) +
            facet_wrap(~hemi, labeller = labeller(hemi = fac.labs))  + 
            
            scale_color_brewer(palette = "Set1", "Hemisphere") +
            
            scale_fill_brewer(palette = "Set1", "Hemisphere") +
          
            scale_y_continuous(expand=expansion(mult = c(0,0.05)), limits=c(0,NA))  +
            
            ylab("Percent\n") +
            xlab("") +
            
            theme(
              panel.background = element_blank(),
              axis.line = element_line("black"),
              axis.text.x=element_text(angle=60, vjust = 1, hjust = 1),
              axis.ticks.x=element_line("black"),
              plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"),
              strip.text.x = element_text(size = 12, colour = "white"),
              panel.spacing = unit(1.5, "lines")
              # strip.background = element_rect(
              #   color="black", fill=c("#E41A1C", "#377EB8", "#4DAF4A"), size=1.5, linetype="solid"
              # ) ### that was for single box only. below does multiple. 
            ) -> p
           

          ### chhange top box colors
          g <- ggplot_gtable(ggplot_build(p))
          strip_both <- which(grepl('strip-', g$layout$name))
          fills <- c("#E41A1C","#377EB8","#4DAF4A")
          k <- 1

          for (i in strip_both) {
            j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
            g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
            k <- k+1
          }

          b<-plot(ggplotify::as.ggplot(g))
          return(b)

}
