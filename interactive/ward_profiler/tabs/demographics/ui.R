# Demographics #

tabPanel(title = "Demographics",
         fluidPage(
           fluidRow(
             column(6,
                    uiOutput("demographics_text")
             ),
             column(6,
                    plotOutput("demographics_pyramid")
             )
           ),
           fluidRow(
             column(6,
                    plotOutput("demographics_languages")
             ),
             column(6,
                    plotOutput("demographics_ethnicity")
             )
           )
         ))
        