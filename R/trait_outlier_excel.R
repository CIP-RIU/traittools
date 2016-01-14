#-----------------------------------------------------------------------------
# To mitigate R CMD check producing NOTES
# reference http://stackoverflow.com/a/12429344
#-----------------------------------------------------------------------------
globalVariables(c("."))

#' An outlier values detector of sumarized fieldbook spreadsheets (Excel)  
#' @description This function use openxlsx package to give conditional format for fieldbook spreadsheet
#' @param file The name of the excel file. It must be an .xlsx file
#' @param sumsheet The name of the excel sheet which contains the summary of your fieldbook .
#' @param trait The abbreviation of the trait used in fieldbooks. 
#' @export 

trait_outlier <- function(file,sumsheet,trait){
  
  wb <- openxlsx::loadWorkbook(file)
  fbsummary <- readxl::read_excel(file,sumsheet) %>% as.data.frame(.)
  trait_summary_names <- names( fbsummary)
  trait_mean_pattern <- paste(trait,"_Mean",sep="")
  trait_sd_pattern <- paste(trait,"_sd",sep="")
  is_trait<- trait_mean_pattern %in% trait_summary_names
  
  if(!is_trait) {print("This Trait is not considered")}
  #stat_pattern <- fieldbook %>% names(.) %>% grepl(pattern = "_Mean",.)
  
  if(is_trait){
  trait_pos <- which(names(fbsummary) == trait_mean_pattern)
  rule <- (fbsummary[,trait_sd_pattern])>(fbsummary[,trait_mean_pattern]/2) #To detect outlier
  nc <- nrow(fbsummary)+1
  
  outlier_values <- fbsummary[,trait_mean_pattern][rule]
  negStyle <- openxlsx::createStyle(fontColour = "#330406", bgFill = "#ffa466")
  
  for(i in outlier_values){
    #print(i)
    if(!is.na(i))
    openxlsx::conditionalFormatting(wb, sheet = "Summary", cols =  trait_pos, 
                                    rows = 2:nc, rule = sprintf("==%s", i),style = negStyle)
  }
  openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
  #shell.exec(file)
  }
}  


#' Detect outlier values into trait columns of summarized fieldbook spreadsheets (Excel)  
#' @description This function use openxlsx package to give conditional format for fieldbook spreadsheet
#' @param file The name of the excel file. It must be an .xlsx file
#' @param sumsheet The name of the excel sheet which contains the summary of your fieldbook .
#' @param trait The abbreviation of the trait used in fieldbooks.  
#' @export
  
col_trait_outlier <- function(file,sumsheet,trait){
  
  lapply(trait,function(x) out <- trait_outlier (file, sumsheet, trait=x ))
}
