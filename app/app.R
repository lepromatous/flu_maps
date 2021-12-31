library(shiny)
library(shinydashboard)
# library(Cairo)
# options(shiny.usecairo = TRUE)
library(tidyverse)
library(vroom)
library(janitor)
library(countrycode)
library(stringr)
library(zoo)
library(shinyWidgets)
library(plotly)
library(rgeos)
library(leaflet)
library(htmltools)
library(data.table)
library(ranger)
library(Hmisc)
library(RColorBrewer)
library(plotly)
library(ggplotify)
### get data
source("read base data.R")


### map functions
source("map editor global_interactive.R")
source("map editor global_season_interactive.R")

source("map editor hemisphere_interactive.R")
source("map editor hemisphere_season_interactive.R")

source("hemisphere strain plot.R")



### NA in legend fix from here: https://github.com/rstudio/leaflet/issues/615
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Influenza Over Time", titleWidth = "100%"),
  dashboardSidebar(
    width = 375,
    sidebarMenu(h4("Select a map"),
      style = "white-space: normal;",
      br(),
      menuItem("Percent of Strains by Country and Month", tabName = "pct_strain_country"),
      menuItem("Percent of Strains by Hemisphere and Month", tabName = "pct_strain_hemi"),
      menuItem("Trajectory of Strains by Month", tabName="trajectory"),
      menuItem("Percent of Strains by Country and Flu Season", tabName = "pct_strain_country_season"),
      menuItem("Percent of Strains by Hemisphere and Flu Season", tabName = "pct_strain_hemi_season"),
      br(),
      hr(),
      h4("Data Sources:"),
      tags$ol(
        tags$li(
          tags$a(
            href = "https://apps.who.int/flumart/Default?ReportNo=12",
            "World Health Organization"
          )
        ),
      ), # close li and ol

      hr(),
      h4("Notes:"),
      tags$ul(
        tags$li("Not all countries report data at all or at all times."),
        tags$li("Legend does not remain constant across time periods.")
      ),
      hr(),
      h4("Created by:"),
      tags$ul(
        tags$li("Timothy Wiemken, PhD"),
        tags$li("Verna Welch, PhD"),
        tags$li("Jacob Clarke, MD"),
        tags$li("Christopher Prener, PhD"),
        tags$li("John McLaughlin, PhD")
      )
    )
  ), # close sidebar menu and dashsidebar

  dashboardBody(
    HTML(html_fix),
    includeCSS("www/recalc-override.css"),
    tabItems(

      ##########################################################################################################################
      tabItem(
        tabName = "pct_strain_country",
        selectInput(
          inputId = "selector_strain",
          label = "Select strain to analyze",
          width = "50%",
          choices = c(
            "Influenza A" = "pct_a",
            "Influenza A H1" = "pct_h1",
            "Influenza A H3" = "pct_h3",
            "Influenza B" = "pct_b",
            "Influenza B Victoria" = "pct_bv",
            "Influenza B Yamagata" = "pct_by"
          ),
          selected = "pct_b"
        ),
        chooseSliderSkin("Flat"),
        # setSliderColor(sliderId="date_selector_strain","Indigo"),
        sliderTextInput(
          inputId = "date_selector_strain",
          label = "Select your Month/Year",
          width = "75%",
          choices = names(table(df_global$moyr)),
          from_min = min(unique(df_global$moyr), na.rm = T), to_max = max(unique(df_global$moyr), na.rm = T),
          grid = F, animate = animationOptions(interval = 3000)
        ),

        ############################################
        ### MAIN OUTPUTS
        ############################################
        leafletOutput("worldmap", height = "70vh"),
        br(),
        p("Percentages are computed with the total influenza positive specimens per country as the denominator."),
      ), # close tab item pct strain




      tabItem(
        tabName = "pct_strain_hemi",
        selectInput(
          inputId = "selector_strain_hemi",
          label = "Select strain to analyze",
          width = "50%",
          choices = c(
            "Influenza A" = "pct_a",
            "Influenza A H1" = "pct_h1",
            "Influenza A H3" = "pct_h3",
            "Influenza B" = "pct_b",
            "Influenza B Victoria" = "pct_bv",
            "Influenza B Yamagata" = "pct_by"
          ),
          selected = "pct_b"
        ),
        chooseSliderSkin("Flat"),
        # setSliderColor(sliderId="date_selector_strain_hemi", "Indigo"),
        sliderTextInput(
          inputId = "date_selector_strain_hemi",
          label = "Select your Month/Year",
          width = "75%",
          choices = names(table(df_hem$moyr)),
          from_min = min(unique(df_hem$moyr), na.rm = T), to_max = max(unique(df_hem$moyr), na.rm = T),
          grid = F, animate = animationOptions(interval = 3000)
        ),

        ############################################
        ### MAIN OUTPUTS
        ############################################
        leafletOutput("worldmap_hemi", height = "70vh"),
        br(),
        p("Percentages are computed with the total influenza positive specimens per hemisphere as the denominator."),
        p("Color coding corresponds to the total per hemisphere or tropics."),
        p("Tropics defined as such if the centroid of the country is within +/- 23.5 degrees from the equator. Horizontal lines are drawn on the map to denote hemisphere separation.")
      ), # close tab item pct strain


      
      
      
      
      
      tabItem(
        tabName = "trajectory",
        selectInput(
          inputId = "selector_strain_trajectory",
          label = "Select strain to analyze",
          width = "50%",
          choices = c(
            "Influenza A" = "pct_a",
            "Influenza A H1" = "pct_h1",
            "Influenza A H3" = "pct_h3",
            "Influenza B" = "pct_b",
            "Influenza B Victoria" = "pct_bv",
            "Influenza B Yamagata" = "pct_by"
          ),
          selected = "pct_b"
        ),
          
        ############################################
        ### MAIN OUTPUTS
        ############################################
        plotOutput("strain_traj", height = "70vh"),
        br(),
        p("Percentages are computed with the total influenza positive specimens per hemisphere as the denominator.")
      ), # close tab item trajectory


      
      
      
      
      tabItem(
        tabName = "pct_strain_country_season",
        selectInput(
          inputId = "selector_strain_season",
          label = "Select strain to analyze",
          width = "50%",
          choices = c(
            "Influenza A" = "pct_a",
            "Influenza A H1" = "pct_h1",
            "Influenza A H3" = "pct_h3",
            "Influenza B" = "pct_b",
            "Influenza B Victoria" = "pct_bv",
            "Influenza B Yamagata" = "pct_by"
          ),
          selected = "pct_b"
        ),
        chooseSliderSkin("Flat"),
        # setSliderColor(sliderId="date_selector_strain_season","Indigo"),
        sliderTextInput(
          inputId = "date_selector_strain_season",
          label = "Select your Month/Year",
          width = "75%",
          choices = choicez,
          selected = choicez[1],
          # from_min = min(unique(df_season$season), na.rm=T), to_max = max(unique(df_season$season), na.rm=T),
          grid = F, animate = animationOptions(interval = 3500)
        ),

        ############################################
        ### MAIN OUTPUTS
        ############################################
        leafletOutput("worldmap_season", height = "70vh"),
        br(),
        p("Percentages are computed with the total influenza positive specimens per country as the denominator."),
      ), # close tab item pct strain




      tabItem(
        tabName = "pct_strain_hemi_season",
        selectInput(
          inputId = "selector_strain_season_hemi",
          label = "Select strain to analyze",
          width = "50%",
          choices = c(
            "Influenza A" = "pct_a",
            "Influenza A H1" = "pct_h1",
            "Influenza A H3" = "pct_h3",
            "Influenza B" = "pct_b",
            "Influenza B Victoria" = "pct_bv",
            "Influenza B Yamagata" = "pct_by"
          ),
          selected = "pct_b"
        ),
        chooseSliderSkin("Flat"),
        # setSliderColor(sliderId="date_selector_strain_season_hemi","Indigo"),
        sliderTextInput(
          inputId = "date_selector_strain_season_hemi",
          label = "Select your Month/Year",
          width = "75%",
          choices = choicez,
          selected = choicez[1],
          # from_min = min(unique(df_season_hem$moyr), na.rm=T), to_max = max(unique(df_season_hem$moyr), na.rm=T),
          grid = F, animate = animationOptions(interval = 3500)
        ),

        ############################################
        ### MAIN OUTPUTS
        ############################################
        leafletOutput("worldmap_season_hemi", height = "70vh"),
        br(),
        p("Percentages are computed with the total influenza positive specimens per hemisphere as the denominator."),
        p("Color coding corresponds to the total per hemisphere or tropics."),
        p("Tropics defined as such if the centroid of the country is within +/- 23.5 degrees from the equator. Horizontal lines are drawn on the map to denote hemisphere separation.")
      ) # close tab item pct strain
    ) # close tab items
  ), # close dashboard body
) # close dashboard page/ui

#------------------------------ End UI ------------------------------#











#------------------------------BEGIN SERVER --------------------------#

server <- function(input, output, session) {
  output$worldmap <- renderLeaflet(
    mapme_strain(
      datez = input$date_selector_strain,
      selection = input$selector_strain
    )
  )

  output$worldmap_hemi <- renderLeaflet(
    mapme_strain_hemi(
      datez_hem = input$date_selector_strain_hemi,
      selection_hem = input$selector_strain_hemi
    )
  )

  output$worldmap_season <- renderLeaflet(
    mapme_strain_season(
      datez_season = input$date_selector_strain_season,
      selection_season = input$selector_strain_season
    )
  )

  output$worldmap_season_hemi <- renderLeaflet(
    mapme_strain_season_hem(
      datez_hem_season = input$date_selector_strain_season_hemi,
      selection_hem_season = input$selector_strain_season_hemi
    )
  )
  
  output$strain_traj <- renderPlot(
    strain_trajectory(
      strain_traje = input$selector_strain_trajectory
    )
  )
}
# Run the application
shinyApp(ui = ui, server = server)