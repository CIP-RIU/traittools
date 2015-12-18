library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)

#returns = readRDS("ptfieldbook3.rds")
returns = readRDS("spfieldbook.rds")

ui  <-  dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard")),
      #fileInput(inputId="hot_file", label="Choose Fieldbook" , multiple = FALSE, accept = NULL, width = NULL),
      shinyFilesButton('file', 'File select', 'Please select a file', FALSE),
      actionButton("calculate", "Calculate Variables"),
      uiOutput('exportAction')
      #uiOutput('exportAction')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(box(rHandsontableOutput("hot_btable",width = "1200")),width=100 ,collapsible = TRUE)
      )
    )
  )
)

server <- function(input, output,session) {
#  
   #volumes <- shinyFiles::getVolumes()
#   print(volumes)
   #shinyFileChoose(input, 'file', roots=volumes, session=session, restrictions=system.file(package='base'))
#   
  
#   values = shiny::reactiveValues(
#     hot_btable = returns
#   )
#   
#   calc = shiny::reactive({
#     btable = values[["hot_btable"]]
#   })
#  
  hot_bdata <- reactive({
    
    #               fb_file <- input$hot_file
    #               str(p)
    #               if(!is.null(fb_file)){
    #               file.copy(fb_file$datapath, paste(fb_file$datapath, ".xlsx", sep=""))
    #               fieldbook <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook") 
    #               fieldbook
    #       fb_file <- input$hot_file
    #       if(is.null(fb_file)) return(NULL)
    #       reactive_excel_fb(fb_file,"Fieldbook")
    volumes <- shinyFiles::getVolumes()
    #print(volumes)
    shinyFileChoose(input, 'file', roots=volumes, session=session,restrictions=system.file(package='base'))
    hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    hot_bdata <- readxl::read_excel(hot_file, "Fieldbook")
    saveRDS(object =  hot_bdata,file ="fieldbook.rds")
    hot_bdata <- readRDS(file = "fieldbook.rds")
    
    
    # }    
  })
  

  output$hot_btable = renderRHandsontable({
    
    values = shiny::reactiveValues(
      #if(is.null(_data)){hot_btable <- NULL}
      hot_btable = hot_bdata()
      #hot_btable = returns
    )
    
    calc = shiny::reactive({
      btable = values[["hot_btable"]]
    })
    
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      plot_size <- 0.03
      plant_den <- 0.1
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      
    }
    
    if(!is.null(DF)){
    traits <- get_trait_fb(DF)
    col_render_trait(DF,trait = traits ,sweetpotato_yield)    
    }
})

  output$exportAction<- renderUI({
    actionButton("exportButton", "Download")
  })
  
#   shiny::observeEvent(input$exportButton, function(){
#     
#     isolate({ 
#       
#       #     if (!is.null(input$hot_btable)) {
#       #         DF = hot_to_r(input$hot_btable)
#       #         values[["hot_btable"]] = DF
#       #         rhandsontable(DF)
#       #      } 
#       #         else if (!is.null(values[["hot_btable"]])) {
#       if (!is.null(values[["hot_btable"]])) {
#         DF = values[["hot_btable"]]
#         rhandsontable(DF)
#       }
#       
#       openxlsx::write.xlsx(DF, "test_export.xlsx", overwrite=TRUE)
#       shell.exec("test_export.xlsx")
#     })
#     
#   })  
  
  
 
}


shinyApp(ui, server)
