library(rhandsontable)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("Breeding Table for Data Analysis"),
  fluidRow(
    column(12,
           helpText("Breeding Smart Table"))
  ),
  fluidRow(
    column(4,
           rHandsontableOutput(outputId = "hot_btable"),
           #checkboxInput("reweight", "Reweight?"),
           actionButton("calculate", "Calculate Variables"),
           uiOutput('exportAction')
          
    ),
    column(6,
           dygraphOutput("plot")
    )
  )
))