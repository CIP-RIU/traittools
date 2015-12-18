library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)

shinyServer(function(input, output, session) {
  
  volumes <- shinyFiles::getVolumes()
  shinyFileChoose(input, 'file', roots=volumes, session=session,restrictions=system.file(package='base'))
  #hot_path <- reactive({
  hot_bdata <- reactive({ 
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
    hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    #hot_bdata <- readxl::read_excel(hot_path_excel , "Fieldbook")
    hot_bdata <- readxl::read_excel(hot_file , "Fieldbook")
    }
  })

#   hot_bdata <- reactive({
#     hot_path_excel <- hot_path()
#     if(is.null(hot_path_excel)) return(NULL)
#     hot_bdata <- readxl::read_excel(hot_path_excel , "Fieldbook")
# })

#   values = shiny::reactiveValues(
#     hot_btable = hot_bdata
#   )
# 
#   calc = shiny::reactive({
#     btable = values[["hot_btable"]]
#   })

  
  output$hot_btable = renderRHandsontable({
    
    #DF <- readRDS("spfieldbook.rds")
    #DF <- NULL
    
    values  <-  shiny::reactiveValues(
      hot_btable = hot_bdata()
    )
    
    calc  <-  shiny::reactive({
      btable = values[["hot_btable"]]
    })
     
    if(length(input$file)==0){return(NULL)}
    if(length(input$file)>0){
      DF <- NULL
      
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      plot_size <- 2.7
      plant_den <- 37037
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      
    }

    if(!is.null(DF)){
      print(DF)
      print("ok")
      traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_fieldbook.rds")
      col_render_trait(DF,trait = traits ,sweetpotato_yield)  
    }
  }
 
})
 
   shiny::observeEvent(input$exportButton, function(){
      
     DF <- readRDS("hot_fieldbook.rds")
     
     hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
     
     wb <- openxlsx::loadWorkbook(hot_file)
     sheets <- readxl::excel_sheets(path = hot_file)
   
     if("Fieldbook" %in% sheets){    
       openxlsx::removeWorksheet(wb, "Fieldbook")
       print("ok-1")
     }
     
     openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
     openxlsx::writeDataTable(wb,sheet = "Fieldbook",x = DF,colNames = TRUE, withFilter = FALSE)
     openxlsx::saveWorkbook(wb = wb,file = hot_file, overwrite = TRUE) 
     traits <- get_trait_fb(DF)
     traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = sweetpotato_yield)
     
    shell.exec(hot_file)
    
   })  
  
})