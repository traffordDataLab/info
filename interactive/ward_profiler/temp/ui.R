library(shiny) ; library(tidyverse) ; library(sf) ; library(RColorBrewer) ; library(viridis)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")
wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson", quiet = TRUE)

ui <- shinyUI(fluidPage(
  navbarPage(title = "Trafford ward profiler", collapsible = TRUE, fluid = TRUE,
             header = tags$div(style="padding-left: 15px;", selectInput("ward", 
                                                                        label = NULL,
                                                                        choices = unique(wards$area_name),
                                                                        selected = "Altrincham",
                                                                        multiple = FALSE,
                                                                        width = "180px")),
             
###############################################################################################################################             
tabPanel(title = "Housing",
         fluidPage(
           fluidRow(
             column(6,
                    br(),
                    uiOutput("housing_text")
             ),
             column(6,
                    plotOutput("housing_map")
             )
           ),
           fluidRow(
             column(6,
                    plotOutput("")
             ),
             column(6,
                    plotOutput("")
             )
           )
         ))
###############################################################################################################################             
)))