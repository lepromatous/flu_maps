library(shiny)
library(shinydashboard)
library(Cairo);options(shiny.usecairo=TRUE)
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

source("map editor global_interactive.R")
source("map editor hemisphere_interactive.R")


###NA in legend fix from here: https://github.com/rstudio/leaflet/issues/615
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

ui<-dashboardPage( skin = "purple",
                   
                   dashboardHeader(title = "Influenza Over Time", titleWidth = '100%'),
                   
                   dashboardSidebar(width = 375,
                                    sidebarMenu(h4("Select a map"),
                                                style = "white-space: normal;",
                                                br(),
                                                menuItem("Percent of Strains by Country", tabName = "pct_strain_country"),
                                                menuItem("Percent of Strains by Hemisphere", tabName = "pct_strain_hemi"),
                                                
                                                
                                                br(),
                                                hr(),
                                                
                                                h4("Data Sources:"),
                                                tags$ol(
                                                  tags$li(
                                                    tags$a(href="https://apps.who.int/flumart/Default?ReportNo=12",
                                                           "World Health Organization")),
                                                 
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
                                                
                                    )), # close sidebar menu and dashsidebar
                   
                   dashboardBody(
                     HTML(html_fix),
                     
                     tags$head(tags$style(HTML('
                                          .main-header .logo {
                                          font-family: "Georgia", Times, "Times New Roman", serif;
                                          font-weight: bold;
                                          font-size: 25px;
                                          }
                                          ".irs-grid-text { font-size: 20pt; }"
                                          .content-wrapper {
                                          background-color: #fff;
                                          }'
                     ))),### close tags
          
                     
                     ##########################################################################################################################
                     
                     tabItems(
                       
                       ##########################################################################################################################
                       tabItem(tabName = "pct_strain_country",
                               
                               
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
                               
                               sliderTextInput(
                                 inputId = "date_selector_strain",
                                 label = "Select your Month/Year",
                                 width = "75%",
                                 choices = names(table(df$moyr)),
                                 from_min = min(unique(df$moyr), na.rm=T), to_max = max(unique(df$moyr), na.rm=T),
                                 grid = FALSE, animate = animationOptions(interval = 1500)
                               ),
                               
                               ############################################
                               ### MAIN OUTPUTS
                               ############################################
                               leafletOutput("worldmap", height="70vh"),
                              br(),
                               p("Percentages are computed with the total influenza positive specimens per country as the denominator."),
                       ), # close tab item pct strain
                       
                       
                       
                       
                       tabItem(tabName = "pct_strain_hemi",
                               
                               
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
                               
                               sliderTextInput(
                                 inputId = "date_selector_strain_hemi",
                                 label = "Select your Month/Year",
                                 width = "75%",
                                 choices = names(table(df_hem$moyr)),
                                 from_min = min(unique(df_hem$moyr), na.rm=T), to_max = max(unique(df_hem$moyr), na.rm=T),
                                 grid = FALSE, animate = animationOptions(interval = 1500)
                               ),
                               
                               ############################################
                               ### MAIN OUTPUTS
                               ############################################
                               leafletOutput("worldmap_hemi", height="70vh"),
                               br(),
                               p("Percentages are computed with the total influenza positive specimens per hemisphere as the denominator."),
                               br(),
                               p("Color coding corresponds to the total per hemisphere or tropics. 
                                 Tropics defined as such if the centroid of the country is within +/- 23.5 degrees from the equator.")
                       ) # close tab item pct strain
                       
                     )# close tab items
                   ), # close dashboard body
)# close dashboard page/ui

#------------------------------ End UI ------------------------------#











#------------------------------BEGIN SERVER --------------------------#

server <- function(input, output, session) {
  
  output$worldmap <-renderLeaflet(
    mapme_strain(datez = input$date_selector_strain,
                 selection = input$selector_strain
    )
  )
  
  output$worldmap_hemi <-renderLeaflet(
    mapme_strain_hemi(datez = input$date_selector_strain_hemi,
                 selection = input$selector_strain_hemi
    )
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
