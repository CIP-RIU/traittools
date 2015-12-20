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
  shinyFiles::shinyFileChoose(input, 'file', roots=volumes, session=session,restrictions=system.file(package='base'))
  
  hot_path <- reactive ({
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
    hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
    hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })


  output$hot_btable = renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_btable = hot_bdata()
    )
    
    DF <- NULL
      
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
      hot_file <- hot_path()
      installation_sheet<- traittools::get_sheet_data(file=hot_file,sheet <- "Installation")
      plot_size  <-  as.numeric(installation_sheet[stringr::str_detect(installation_sheet$Factor,"Plot size"),"Value"])
      plant_den  <-  as.numeric(installation_sheet[stringr::str_detect(installation_sheet$Factor,"Planting density"),"Value"])
      #plot_size <- 0.05
      #plant_den <- 50
      
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbformula::sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      
    }

    if(!is.null(DF)){
      traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_fieldbook.rds")
      traittools::col_render_trait(DF,trait = traits ,sweetpotato_yield)  
    }
    #}
 
    })
 
   shiny::observeEvent(input$exportButton, function(){
      
     DF <- readRDS("hot_fieldbook.rds")
     
     trait <- get_trait_fb(fieldbook)
     validator <- is.element(trait,trait_dict$ABBR)
     trait <- trait[validator]
   
     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",trait = trait, 
                                   design = "Randomized Complete Block Design (RCBD)", trait_dict = sweetpotato_yield)
     
     hot_file <- hot_path() 
     
     wb <- openxlsx::loadWorkbook(hot_file)
     sheets <- readxl::excel_sheets(path = hot_file)
     
     if("Fieldbook" %in% sheets){    
       openxlsx::removeWorksheet(wb, "Fieldbook")
     }
     
     if("Summary" %in% sheets){    
       openxlsx::removeWorksheet(wb, "Summary")
       # openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE) 
     }
     
     openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
     openxlsx::writeDataTable(wb,sheet = "Fieldbook",x = DF,colNames = TRUE, withFilter = FALSE)
     
     openxlsx::addWorksheet(wb = wb,sheetName = "Summary",gridLines = TRUE)
     openxlsx::writeDataTable(wb,sheet = "Summary",x = summary ,colNames = TRUE, withFilter = FALSE)
     
     openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
     traits <- traittools::get_trait_fb(DF)
     traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = sweetpotato_yield)
     
     shell.exec(hot_file)
    
   })  
  
})