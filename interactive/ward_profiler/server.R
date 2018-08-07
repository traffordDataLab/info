server <- function(input, output, session) {
  
  output$overview <- renderUI({
    shiny::includeHTML(paste0("overview/", wards[wards$area_name == input$ward,]$area_code, ".html"))
  })

}