#' Get all the trait variables of fieldbooks, excluding all the fieldbook factors
#' @param fieldbook A data.frame which contain fieldbook data
#' @author omar benites
#' @return All the trait variables
#' @export
#' 
get_trait_fb <- function(fieldbook){
   
   factors <-c("PLOT","INSTN","REP","FACTOR") 
   trait_names <- names(fieldbook)
   trait_names <- names(fieldbook)[!is.element(names(fieldbook),factors)]
   trait_names 
   
 }
 
 #' Read fieldbooks in reactive enviorment (Shiny)
 #' @param file_id The id of the shinyInput for an fieldbook excel (input$shinyInput_id).
 #' @param sheet The name of sheet which contains fieldbook data
 #' @author omar benites
 #' @return A data.frame contains the fieldbook data
 #' @export
 #'  
 
reactive_excel_fb <- function(file_id,sheet){
   #fb_file <- input$hot_file
   file.copy(file_id$datapath, paste(file_id$datapath, ".xlsx", sep=""))
   fieldbook <- readxl::read_excel(paste(file_id$datapath, ".xlsx", sep=""), sheet = sheet)
   fieldbook
 } 

 
#' Get fieldbook parameters into Excel Files
#' @description This function gets parameters or values from fieldbook excel file. Do an excel scrapping.
#' @param file The file name
#' @param sheet The sheet name
#' @param parameter The label of the factor we want to extract. Ex. "Experimental design", "Plot_Size"
#' @export
#' 
get_fb_param <- function(file,sheet,parameter){
   fb_param <- readxl::read_excel(path = file, sheet = sheet)
   fb_param<- as.data.frame(fb_param)
   fb_param[fb_param$Factor==parameter,2]
 }
 

#' Get data from excel files
#' @description This function gets all the data from excel files. 
#' @param file The file name
#' @param sheet The sheet name
#' @export
#' 
get_sheet_data <- function(file,sheet){
  table_param <- readxl::read_excel(path = file, sheet = sheet)
  table_param <- as.data.frame(table_param)
  table_param
  #value_param <- as.numeric(table_param[stringr::str_detect(table_param[,row_param],value_param),col_param])
  #value_param
  #lapply(x <- 1:ncol(parameter), function(x) parameter[,x]<-as.character(parameter[,x]))
  #for(i in 1:ncol(params)) params[,i]<-as.character(params[,i])
  #parameter[parameter$Factor==parameter,2]
  #plot_size  <-  as.numeric(inst[stringr::str_detect(inst$Factor,"Plot size"),"Value"])
}

#' Post the fieldbook data into excel files
#' @description This function gets all the data from excel files. 
#' @param file The file name
#' @param sheet The sheet name
#' @param fieldbook The fieldbook data
#' @export
#' 
post_sheet_data <- function(file,sheet,fieldbook){
  
      wb <- openxlsx::loadWorkbook(file = file)
      sheets <- readxl::excel_sheets(path = file)
      if(sheet %in% sheets){    
        openxlsx::removeWorksheet(wb = wb, sheet = sheet)
      }
      openxlsx::addWorksheet(wb = wb,sheetName = sheet, gridLines = TRUE)
      openxlsx::writeDataTable(wb = wb,sheet = sheet, x = fieldbook, colNames = TRUE, withFilter = FALSE)
      openxlsx::saveWorkbook(wb = wb, file = sheet, overwrite = TRUE)
}

#Not run #traittools::post_sheet_data(file = hot_file,sheet = "Fieldbook",fieldbook = DF)



