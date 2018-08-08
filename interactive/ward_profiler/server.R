server <- function(input, output, session) {
  
  output$overview <- renderUI({
    shiny::includeHTML(paste0("www/", wards[wards$area_name == input$ward,]$area_code, ".html"))
    })

  source("themes/demographics_server.R", local = TRUE)$value
  
}