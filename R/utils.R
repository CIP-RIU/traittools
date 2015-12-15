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
 
