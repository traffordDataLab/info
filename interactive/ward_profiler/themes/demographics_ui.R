# Demographics #

tabPanel(title = "Demographics",
         fluidPage(
           fluidRow(
             br(),
             column(6,
                    plotOutput("demographics_pyramid")
             ),
             column(6,
                    "Box 2"
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
        