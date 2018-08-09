# Demographics #

tabPanel(title = "Demographics",
         fluidPage(
           fluidRow(
             br(),
             column(6,
                    uiOutput("demographics_text")
             ),
             column(6,
                    plotOutput("demographics_pyramid")
             )
           ),
           br(),
           fluidRow(
             column(6,
                    "Box 3"
             ),
             column(6,
                    dataTableOutput("demographics_data")
             )
           )
         ))
        