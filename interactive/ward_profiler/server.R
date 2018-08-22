server <- function(input, output, session) {
  
  output$overview <- renderUI({
    shiny::includeHTML(paste0("www/", wards[wards$area_name == input$ward,]$area_code, ".html"))
    })

  source("tabs/demographics/server_fragment.R", local = TRUE)$value
  source("tabs/housing/server_fragment.R", local = TRUE)$value
  
}