library(rhandsontable)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("130/30 Portfolio Returns"),
  fluidRow(
    column(12,
           helpText("Change the position type and weights to recalculate the ",
                    "return stream."))
  ),
  fluidRow(
    column(4,
           rHandsontableOutput("hot_btable"),
           #checkboxInput("reweight", "Reweight?"),
           actionButton("calculate", "Calculate Variables"),
           uiOutput('exportAction')
          
         
    ),
    column(6,
           dygraphOutput("plot")
    )
  )
))