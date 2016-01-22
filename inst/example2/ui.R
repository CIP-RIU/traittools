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
    skin="yellow",
    dashboardHeader(title = "Data Processing"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Phenotyping Dashboard", icon = icon("dashboard"),
                 menuSubItem(text = "Data Processing", tabName = "data_processing")
                 )
       ) 
    ),
    
  dashboardBody(
  tabItems(
     tabItem(tabName = "dashboard",
               h2("Hidal Modules"),
               p(
                 class = "text-muted",
                 paste("Building..."
                 ))
       ),
       
     tabItem(tabName = "data_processing",
          h2("Data Quality and Processing"),   
          
        tabsetPanel(
            tabPanel("Check",
                br(),
               # fluidRow(
#                    column(width = 3, #begin first column
#                         box(
#                            title = "Step 1: Select your fieldbook", status = "warning", 
#                            solidHeader = TRUE, collapsible = TRUE, width = NULL,
#                            p("Select the you Excel-XLSX file"),
#                            shinyFilesButton('file', 'File select', 'Please select a file', FALSE)
#                            ),
#                         box(
#                             title = "Step 2: Run Check and Processing", status = "warning",
#                             solidHeader = TRUE, collapsible = TRUE, width = NULL,
#                             p("Start Data Processing"),
#                             actionButton("calculate", "Calculate Variables")
#                           )
#                       
#                  #   ), #End of first column
#                 ),
                
        #  column(width = 3,
                 
                 fluidRow(
                   column(5, shinyFilesButton('file', 'File select', 'Please select a file', FALSE)),
                   column(3, actionButton("calculate", "Calculate",icon("calculator"))),
                   column(3, actionButton("exportButton", "Download", icon("download")))
                 ),
                 tags$style(type='text/css', "#file { width:30%; margin-top: 25px;}"),
                 tags$style(HTML('#file {background-color:#0099cc; color: #ffffff}')),  
                 tags$style(type='text/css', "#calculate { width:50%; margin-top: 25px;}"),
                 tags$style(HTML('#calculate {background-color:#1652a9; color: #ffffff}')),
                 tags$style(type='text/css', "#exportButton { width:50%; margin-top: 25px;}"),
                 tags$style(HTML('#exportButton {background-color:#30c9ae; color: #ffffff}')),


               box(rHandsontableOutput("hot_btable",width = 2000),width =1500)


        #  )
#             column(width = 5,offset =1,
#                    box(
#                      title = "Fieldbook", width = 1000, collapsible = TRUE, status = "info",
#                      HTML('<p style="text-align:justify">'),
#                      
#                      actionButton("exportButton", "Download"),
#                      rHandsontableOutput("hot_btable", width = "1000") 
#                    )
#                   )
            #)
            
                )#end tabset Panel
      )
    )
  )
)

)
)
  
