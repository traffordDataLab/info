ui <- shinyUI(fluidPage(
  navbarPage(theme = "styles.css",
             title = "Trafford ward profiler", collapsible = TRUE, fluid = TRUE,
             header = tags$div(style="padding-left: 15px;", selectInput("ward", 
                         label = NULL,
                         choices = unique(wards$area_name),
                         selected = "Altrincham",
                         multiple = FALSE,
                         width = "180px")),
             tabPanel("Overview",
                      uiOutput("overview")),
             source("tabs/demographics/ui.R", local = TRUE)$value,
             source("tabs/economy/ui.R", local = TRUE)$value,
             source("tabs/education/ui.R", local = TRUE)$value,
             source("tabs/environment/ui.R", local = TRUE)$value,
             source("tabs/governance/ui.R", local = TRUE)$value,
             source("tabs/health/ui.R", local = TRUE)$value,
             source("tabs/housing/ui.R", local = TRUE)$value,
             source("tabs/labour_market/ui.R", local = TRUE)$value,
             source("tabs/transport/ui.R", local = TRUE)$value,
             tabPanel("Sources",
                      includeMarkdown("tabs/sources.md"))
             )
  )
  )