ui <- shinyUI(fluidPage(
  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
  br(),
  selectInput("ward", 
              label = NULL,
              choices = unique(wards$area_name),
              selected = "Altrincham",
              multiple = FALSE,
              width = "180px"),
  navbarPage(theme = "styles.css",
             title = "Trafford ward profiler", collapsible = TRUE, fluid = TRUE,
             tabPanel("Overview",
                      uiOutput("overview")),
             tabPanel("Demographics"),
             tabPanel("Economy"),
             tabPanel("Education"),
             tabPanel("Environment"),
             tabPanel("Governance"),
             tabPanel("Health"),
             tabPanel("Housing"),
             tabPanel("Labour Market"),
             tabPanel("Transport")
             )
  )
  )