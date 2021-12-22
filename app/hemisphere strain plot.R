
# options(repos = c(
#   techtonique = 'https://techtonique.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))
# install.packages("ahead")
library(ahead)
library(tidyverse)
library(ranger)
library(viridis)


test <- df_season_hem
st_geometry(test)<- NULL

test %>%
  mutate(
    area = gsub("[km^2]", "", area),
    economy = factor(economy),
    income_grp = factor(income_grp),
    who_region = factor(who_region),
    influenza_transmission_zone = factor(influenza_transmission_zone)
  ) %>%
  select(continent, area, pop_est, pop_est_dens, economy, gdp_cap_est,
           life_exp, well_being, footprint, inequality, HPI,season,hemi,
           who_region, influenza_transmission_zone, moyr, pct_b, end_date) -> test



test %>%
  group_by(hemi, season) %>%
  slice(n()) %>%
  ungroup() -> test2



lab1 <- (length(test2$moyr)/3)/2
lab2 <- (length(test2$moyr)/3)/2 *3
lab3 <- (length(test2$moyr)/3)/2 *5
lab.vert <- ceiling(max(test2$pct_b))+2.5
xlab.end <- (length(test2$moyr)/3)-2
xlabs <- c(choicez[1], rep("", times=xlab.end), choicez[length(choicez)],
           choicez[1], rep("", times=xlab.end), choicez[length(choicez)],
           choicez[1], rep("", times=xlab.end), choicez[length(choicez)])



ggplot() + 
  
  geom_smooth(data = test2, 
            aes(x=seq(1,length(moyr)), 
                y=pct_b, fill=hemi, color=hemi), show.legend = F) +
  
  geom_rect(aes(xmin=0, xmax=length(test2$moyr)/3 + 0.4, ymin=ceiling(max(test2$pct_b)), ymax=ceiling(max(test2$pct_b))+5), fill="#E41A1C", alpha=0.5) +
  
  geom_rect(aes(xmin=length(test2$moyr)/3 +.6, xmax=length(test2$moyr)/3*2 +0.5, ymin=ceiling(max(test2$pct_b)), ymax=ceiling(max(test2$pct_b))+5), fill="#377EB8", alpha=0.5) +
  
  geom_rect(aes(xmin=length(test2$moyr)/3*2 +0.6, xmax=length(test2$moyr), ymin=ceiling(max(test2$pct_b)), ymax=ceiling(max(test2$pct_b))+5), fill="#4DAF4A", alpha=0.5) +

  scale_color_brewer(palette = "Set1", "Hemisphere") +
  
  scale_fill_brewer(palette = "Set1", "Hemisphere") +
  # 
  # geom_vline(aes(xintercept=length(test2$moyr)/3 + 0.4), colour="white", size=0.8) +
  # 
  # geom_vline(aes(xintercept=length(test2$moyr)/3*2+0.4), colour="white", size=1) +
  # 
  scale_y_continuous(expand=expansion(mult = c(0,0.05)), limits=c(0,NA))+
  
  geom_segment(aes(x=0, y=0, xend=lab1*2-0.4, yend=0), size=1.1) +
  geom_segment(aes(x=lab1*2+0.6, y=0, xend=lab1*4-0.4, yend=0), size=1.1) +
  geom_segment(aes(x=lab1*4+0.6, y=0, xend=lab1*6-0.4, yend=0), size=1.1) +
  
  ylab("Percent\n") +
  xlab("") +
  
  theme(
    panel.background = element_blank(),
    axis.line.y = element_line("black"),
    axis.line.x = element_blank(),
    axis.text.x=element_text(angle=90),
    axis.ticks.x=element_line("black"),
    plot.margin = margin(0, 0, 2, 0.5, "cm")
  ) + 
  
  annotate(
    "text", label = "Northern",
    x = lab1, y = lab.vert, size = 5, colour = "black"
  ) +
  annotate(
    "text", label = "Southern",
    x = lab2, y = lab.vert, size = 5, colour = "black"
  ) +
  annotate(
    "text", label = "Tropics",
    x = lab3, y = lab.vert, size = 5, colour = "black"
  ) +
  scale_x_discrete(labels=xlabs)



