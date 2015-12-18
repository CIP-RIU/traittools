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
#' @param parameter The column header to extract parameters.
#' @export
#' 
get.fb.param <- function(file,sheet,parameter){
   parameter <- readxl::read_excel(path = file, sheet = sheet)
   parameter<- as.data.frame(parameter)
   lapply(x <- 1:ncol(parameter), function(x) parameter[,x]<-as.character(parameter[,x]))
   #for(i in 1:ncol(params)) params[,i]<-as.character(params[,i])
   parameter[parameter$Factor==parameter,2]
 }
 




