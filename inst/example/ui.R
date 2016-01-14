library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)

shinyUI(
  shinydashboard::dashboardPage(
        dashboardHeader(title = "Data Processing"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Table", tabName = "table", icon = icon("dashboard")),
            #fileInput(inputId="hot_file", label="Choose Fieldbook" , multiple = FALSE, accept = NULL, width = NULL),
            shinyFilesButton('file', 'File select', 'Please select a file', FALSE),
            actionButton("calculate", "Calculate Variables")
            #actionButton("exportButton", "Download")
            #actionButton('exportAction',"Download")
            #uiOutput('exportAction')
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "table",
                    actionButton("exportButton", "Download"),
                    fluidRow(box(rHandsontableOutput("hot_btable",width = "1000"),width=1000),collapsible = TRUE)
            )
          )
        )
      )
    
)