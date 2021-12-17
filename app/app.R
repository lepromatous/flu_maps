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

source("map editor global.R")



ui<-dashboardPage( skin = "purple",
                   
                   dashboardHeader(title = "Influenza Over Time", titleWidth = '100%'),
                   
                   dashboardSidebar(width = 375,
                                    sidebarMenu(h4("Select a map"),
                                                style = "white-space: normal;",
                                                br(),
                                                menuItem("Percent of Strains", tabName = "pct_strain"),

                                                
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
                                                  tags$li("Not all countries report data at all or at all times.")
                                                ),
                                                hr(),
                                                
                                                h4("Created by:"), 
                                                tags$ul(
                                                  tags$li("Timothy Wiemken, PhD"), 
                                                  tags$li("Verna Welch, PhD"),
                                                  tags$li("Jacob Clarke, MD"),
                                                  tags$li("John McLaughlin, PhD")
                                                )
                                                
                                    )), # close sidebar menu and dashsidebar
                   
                   dashboardBody(
                     tags$head(tags$style(HTML('
                                          .main-header .logo {
                                          font-family: "Georgia", Times, "Times New Roman", serif;
                                          font-weight: bold;
                                          font-size: 20px;
                                          }
                                          .content-wrapper {
                                          background-color: #fff;
                                          }'
                     ))),### close tags
          
                     
                     ##########################################################################################################################
                     
                     tabItems(
                       
                       ##########################################################################################################################
                       tabItem(tabName = "pct_strain",
                               
                               
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
                                 width = "100%",
                                 choices = names(table(df$moyr)),
                                 from_min = min(unique(df$moyr), na.rm=T), to_max = max(unique(df$moyr), na.rm=T),
                                 grid = FALSE, animate = animationOptions(interval = 1500)
                               ),
                               
                               ############################################
                               ### MAIN OUTPUTS
                               ############################################
                               plotOutput("worldmap", width="100%"),
                               br(),
                               p("Note, Light gray shading represents missing data.")
                       ) # close tab item pct strain
                       
                      
                     )# close tab items
                   ), # close dashboard body
)# close dashboard page/ui

#------------------------------ End UI ------------------------------#











#------------------------------BEGIN SERVER --------------------------#

server <- function(input, output, session) {
  
  output$worldmap <-renderPlot(
    mapme_strain(datez = input$date_selector_strain,
                 selection = input$selector_strain
    )
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
