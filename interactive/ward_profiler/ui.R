ui <- shinyUI(fluidPage(
  navbarPage(theme = "styles.css",
             title = "Trafford ward profiler", collapsible = TRUE, fluid = TRUE,
             header = tags$div(style="padding-left: 10px;", selectInput("ward", 
                         label = NULL,
                         choices = unique(wards$area_name),
                         selected = "Altrincham",
                         multiple = FALSE,
                         width = "180px")),
             tabPanel("Overview",
                      uiOutput("overview")),
             source("themes/demographics_ui.R", local = TRUE)$value,
             tabPanel("Economy"),
             tabPanel("Education"),
             tabPanel("Environment"),
             tabPanel("Governance"),
             tabPanel("Health"),
             tabPanel("Housing"),
             tabPanel("Labour Market"),
             tabPanel("Transport"),
             tabPanel("Sources",
                      includeMarkdown("themes/sources.md"))
             )
  )
  )