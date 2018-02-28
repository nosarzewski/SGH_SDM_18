shinyUI(fluidPage(
             titlePanel("Analiza skupień"),
             sidebarLayout(
               sidebarPanel(
                 numericInput('clusters', 'Liczba grup', 4, min = 2, max = 10)
                 ),
            mainPanel(
              plotOutput("plotDendrogram"),
              plotOutput("plotRadar")
              )
            )
)
)