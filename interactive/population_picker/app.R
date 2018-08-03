## Population picker ##

library(shiny) ; library(tidyverse) ; library(sf) ; library(leaflet) ; library(htmlwidgets) ; library(DT) ; library(plotly) ; library(janitor)

pop <- read_csv("data/population_estimates.csv") %>% 
  select(-All) %>% 
  gather(age, count, -year, -area_code, -area_name, -geography, -gender) %>% 
  mutate(age = as.integer(age))

ui <- navbarPage(title = strong("Population Picker"), windowTitle = "Population Picker", fluid = TRUE, collapsible = TRUE,
                 tabPanel("Parameters", "", icon = icon("cog", lib = "font-awesome"),
                          fluidRow(column(8, offset = 2, align = "center", 
                                          htmlOutput("title", inline = TRUE))),
                          br(),
                          fluidRow(column(10, offset = 1, align = "center", 
                                          radioButtons("geography", 
                                                      label = NULL,
                                                      choices = list("District" = "la",
                                                                     "Ward" = "ward",
                                                                     "MSOA" = "msoa",
                                                                     "LSOA" = "lsoa",
                                                                     "OA" = "oa"),
                                                      selected = "ward",
                                                      inline = TRUE))),
                          fluidRow(column(8, offset = 2, align = "center", 
                                          leafletOutput("map", height = "320px"))),
                          br(),
                          fluidRow(column(10, offset = 1, align = "center", 
                                          sliderInput("age",
                                                      label = NULL,
                                                      min = 0,
                                                      max = 90,
                                                      value = c(0, 90),
                                                      step = 1,
                                                      ticks = TRUE,
                                                      post = " years")))),
                 tabPanel("Chart", icon = icon("bar-chart-o", lib = "font-awesome"),
                          fluidRow(column(8, offset = 2, align = "center",
                                          plotlyOutput("plot"),
                                          br(),
                                          tableOutput('table')))),
                 tabPanel("Data", icon = icon("table", lib = "font-awesome"),
                          fluidRow(column(8, offset = 2, align = "center",
                                          dataTableOutput("data", height = "100%")))),
                 tabPanel("About", icon = icon("info", lib = "font-awesome"),
                          fluidRow(column(8, offset = 2,
                                          includeMarkdown("info.md")))))

server <- function(input, output, session) {
  
  output$title <- renderUI({
    
    validate(need(clickedIds$ids, message = "Click one or more areas on the map"))
    
    HTML(paste0("<span style = 'text-decoration: underline;'>", 
                prettyNum(sum(area_data()[area_data()$gender == "Total",]$count), big.mark = ",", scientific = FALSE),
                "</span> residents (",
                round(sum(area_data()[area_data()$gender == "Female",]$count)/sum(area_data()[area_data()$gender == "Total",]$count)*100, 1),
                "% Female | ",
                round(sum(area_data()[area_data()$gender == "Male",]$count)/sum(area_data()[area_data()$gender == "Total",]$count)*100, 1),
                "% Male)"))
  })
  
  layer <- reactive({
    filename <- paste0("data/", input$geography, ".geojson")
    st_read(filename)
  })
  
  clickedIds <- reactiveValues(ids = vector())
  
  observeEvent(input$geography, {
    clickedIds$ids <- NULL
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-2.35533522781156, 53.419025498197, zoom = 11) %>% 
      addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="http://cartodb.com/attributions">CartoDB</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "Low detail") %>% 
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>  | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "High detail") %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", 
               attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community | <a href="https://www.ons.gov.uk/methodology/geography/licences"> Contains OS data © Crown copyright and database right (2018)</a>', 
               group = "Satellite") %>%
      addPolygons(data = layer(), 
                  fillColor = "white", 
                  fillOpacity = 0.4, 
                  color = "#212121", 
                  stroke = T, 
                  weight = 1, 
                  layerId = layer()$area_code) %>% 
      addLayersControl(
        baseGroups = c("Low detail", "High detail", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      htmlwidgets::onRender(paste0("
                                   function(el, x) {
                                   $('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");
                                   }"))

                                   })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    proxy <- leafletProxy("map")
    clickedIds$ids <- c(clickedIds$ids, click$id)
    clickedPolys <- layer()[layer()$area_code %in% clickedIds$ids, ]
    if(click$id %in% clickedPolys$area_name){
      nameMatch <- clickedPolys$area_code[clickedPolys$area_name == click$id]
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
      proxy %>% removeShape(layerId = click$id)
      
    } else {
      
      proxy %>% addPolygons(data = clickedPolys,
                            fillColor = "#fc6721",
                            fillOpacity = 0.4,
                            weight = 1,
                            color = "#212121",
                            stroke = T,
                            layerId = clickedPolys$area_name)
    }
  })
  
  area_data  <- reactive({
    filter(pop, 
           area_code %in% clickedIds$ids,
           age >= input$age[1], age <= input$age[2])
  })
  
  output$plot <- renderPlotly({
    
    validate(need(clickedIds$ids, message = "Click one or more areas on the map"))
    
    temp <- area_data() %>% 
      filter(area_code %in% clickedIds$ids, gender != "Total") %>% 
      mutate(age = as.integer(age),
             ageband = cut(age,
                           breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                           labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                      "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                      "75-79","80-84","85-89","90+"),
                           right = FALSE)) %>% 
      group_by(gender, ageband) %>% 
      summarise(n = sum(count)) %>%
      mutate(percent = round(n/sum(n)*100, 1),
             percent = 
               case_when(
                 gender == "Male" ~ percent*-1,
                 TRUE ~ as.double(percent)))
    
    plot_ly(temp, x = ~percent, y = ~ageband, color = ~gender) %>% 
      add_bars(orientation = 'h', hoverinfo = "none", colors = c('#d8b365', '#5ab4ac')) %>%
      layout(bargap = 0.1, barmode = 'overlay',
             xaxis = list(tickmode = 'array', tickvals = c(-5, -2.5, 0, 2.5, 5),
                          ticktext = c('5%', '2.5%', '0', '2.5%', '5%'),
                          title = ""),
             yaxis = list(title = "")) %>% 
      config(displayModeBar = F)
    
  })
  
  output$table <- renderTable({
    
    validate(need(clickedIds$ids, message = ""))
    
    area_data() %>% 
      select(area_name, gender, age, count) %>% 
      group_by(area_name, gender) %>% 
      summarise(n = sum(count)) %>% 
      spread(gender, n) %>% 
      rename(Area = area_name) %>% 
      adorn_totals("row") %>% 
      mutate(Female = prettyNum(Female, big.mark = ",", scientific = FALSE),
             Male = prettyNum(Male, big.mark = ",", scientific = FALSE),
             Total = prettyNum(Total, big.mark = ",", scientific = FALSE))
    
  })
  
  output$data <- renderDataTable({
    
    validate(need(clickedIds$ids, message = "Click one or more areas on the map"))
    
    area_data() %>% 
      filter(gender != "Total") %>% 
      select(Year = year,
             `Area code` = area_code,
             `Area name` = area_name,
             Geography = geography,
             Gender = gender,
             Age = age,
             Count = count) %>% 
      group_by(`Area code`) %>% 
      arrange(Age)
  }, 
  extensions= c('Buttons', "Scroller"), 
  rownames = FALSE,  
  options=list(
    dom = 'Blfrtip',
    deferRender = TRUE,
    scroller = TRUE, 
    scrollX = TRUE,
    scrollY = "300px",
    columnDefs = list(list(className = 'dt-left', targets = 0:6)), 
    buttons = list('copy', 'csv', 'pdf')))
  
                                   }

shinyApp(ui, server)