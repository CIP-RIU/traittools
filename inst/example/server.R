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
  
  hot_params <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Installation")
      hot_design <- get_fb_param(hot_param,"Experimental design")
      hot_plot_size <- get_fb_param(hot_param,"Plot size (m2)")
      hot_plant_den <- get_fb_param(hot_param,"Planting density (plants/Ha)")
      hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
                              hot_plant_size =  hot_plant_den)
    }
  })  
    hot_crop <- reactive({
      hot_file <- hot_path()
      if(length(hot_file)==0){return (NULL)}
      if(length(hot_file)>0){
        hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
        hot_crop <- get_fb_param(hot_param,"Crop")
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
      #hot_file <- hot_path()
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      
      #installation_sheet<- traittools::get_sheet_data(file=hot_file,sheet <- "Installation")
      #plot_size  <-  as.numeric(installation_sheet[stringr::str_detect(installation_sheet$Factor,"Plot size"),"Value"])
      #plant_den  <-  as.numeric(installation_sheet[stringr::str_detect(installation_sheet$Factor,"Planting density"),"Value"])
      #plot_size <- 0.05
      #plant_den <- 50
      
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbformula::sbcalculate(fb = DF,plot.size =hot_plot_size, plant.den = hot_plant_den)
      
    }

    if(!is.null(DF)){
      traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_fieldbook.rds")
      crop <- hot_crop()
      if(crop == "potato"){ trait_dict <- potato_yield}
      if(crop == "sweetpotato"){ trait_dict <- sweetpotato_yield}
      
      traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      
      
    }
    #}
 
    })
 
   shiny::observeEvent(input$exportButton, function(){
      
     DF <- readRDS("hot_fieldbook.rds")
     
     trait <- get_trait_fb(DF)
     crop <- hot_crop()
     if(crop == "potato"){ trait_dict <- potato_yield }
     if(crop == "sweetpotato"){ trait_dict <- sweetpotato_yield }
     
     validator <- is.element(trait,trait_dict$ABBR)
     trait <- trait[validator]
     
     hot_design <- as.character(hot_params()$hot_design)
     
     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",trait = trait, 
                                   design = hot_design, trait_dict = trait_dict)
     
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
     traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = trait_dict)
     
     shell.exec(hot_file)
    
   })  
  
})