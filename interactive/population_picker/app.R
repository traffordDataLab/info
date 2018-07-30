## Population picker ##

library(shiny) ; library(miniUI) ; library(tidyverse) ; library(sf) ; library(leaflet) ; library(htmlwidgets) ; library(DT)

pop <- read_csv("data/population_estimates.csv") %>% 
  select(-All) %>% 
  gather(age, count, -year, -area_code, -area_name, -geography, -gender) %>% 
  mutate(age = as.integer(age))

ui <- miniPage(
  miniTitleBar(title = "Population picker"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(fluidRow(
                   column(width = 12, align = "center",
                          selectInput("geography", 
                                      label = NULL,
                                      choices = c("Metroplitan District" = "la",
                                                  "Wards" = "ward", 
                                                  "Middle Super Output Area" = "msoa",
                                                  "Lower Super Output Areas" = "lsoa",
                                                  "Output Areas" = "oa"), 
                                      selected = "ward",
                                      multiple = FALSE,
                                      width = '200px'),
                          sliderInput("age", label = NULL,
                                      min = 0, 
                                      max = 90, 
                                      value = c(0, 90), 
                                      step = 1,
                                      ticks = TRUE,
                                      post = " years",
                                      width = '250px'),
                          div(htmlOutput("summary", align = "center", style = "height: 50px;")),
                          br(),
                          leafletOutput("map", width = "90%"),
                          h6("Note that age 90 includes those aged 90 years and above."))))),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   dataTableOutput("table", height = "100%"))),
    miniTabPanel("Info", icon = icon("info"),
                 miniContentPanel(
                   includeMarkdown("info.md")))
    )
  )

server <- function(input, output, session) {
  
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

  output$table <- DT::renderDataTable({
    
    req(clickedIds$ids)

    area_data() %>% 
      select(Year = year,
             `Area code` = area_code,
             `Area name` = area_name,
             Geography = geography,
             Gender = gender,
             Age = age,
             Count = count) %>% 
      group_by(`Area code`, Gender) %>% 
      arrange(Age)
  }, 
  extensions= c('Buttons', "Scroller"), 
  rownames = FALSE,  
  options=list(
    dom = 'Blfrtip',
    deferRender = TRUE, 
    scrollY = 300,
    scroller = TRUE, 
    columnDefs = list(list(className = 'dt-left', targets = 0:6)), 
    buttons = list('copy', 'csv', 'pdf')))
  

  output$summary <- renderUI({
    req(clickedIds$ids)
    
    HTML(paste0("<span>The resident population aged between <span style='text-decoration: underline;'>",
                input$age[1], " and ", input$age[2], " years</span> is <strong>",
                prettyNum(sum(area_data()[area_data()$gender == "Total",]$count), big.mark = ",", scientific = FALSE),
                "</strong>.<br><small>Females: ",
                prettyNum(sum(area_data()[area_data()$gender == "Female",]$count), big.mark = ",", scientific = FALSE),
                " (", round(sum(area_data()[area_data()$gender == "Female",]$count)/sum(area_data()[area_data()$gender == "Total",]$count)*100, 1),
                "%) | Males: ",
                prettyNum(sum(area_data()[area_data()$gender == "Male",]$count), big.mark = ",", scientific = FALSE),
                " (", round(sum(area_data()[area_data()$gender == "Male",]$count)/sum(area_data()[area_data()$gender == "Total",]$count)*100, 1),
                "%)</small></br></span>"))
  })
  
}

  

shinyApp(ui, server)