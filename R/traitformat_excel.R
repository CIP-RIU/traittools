#' Generate conditional format for every trait column into fieldbook spreadsheet (Excel)  
#' @description This function use openxlsx package to give conditional format for fieldbook spreadsheet
#' @param file The name of the file which contains your data fieldbook data. It must be an .xlsx file
#' @param fbsheet The name of the fieldbook sheet into the excel file.
#' @param trait The abbreviation of the trait used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' (paint with colours) the trait column to identify out of range values.
#' @return An excel file with conditional format according trait conditions 
#' @export
#' 
col_validation <- function(file,fbsheet,trait,trait_dict){ 
  
  #print(trait)
  
  ext <- tools::file_ext(file)
  if(ext!="xlsx"){ stop("traittools can not read .xls or .xlm files. Just xlsx")   }
  
  wb <- openxlsx::loadWorkbook(file)
 
  fieldbook <- readxl::read_excel(file,sheet = fbsheet)
  fieldbook <- as.data.frame(fieldbook)
  
  tp <- get_trait_type(trait=trait,trait_dict = trait_dict)#type trait value
  scale_value <- get_scale_trait(trait = trait,trait_dict = trait_dict)
  
  #print(trait)
  if(!(trait %in% names(fieldbook))) { stop("This trait is not in the fieldbook!")  } 
  
  col_trait <- fieldbook[,trait]
  col_number <- which(names(fieldbook)==trait)
  nc <- nrow(fieldbook)+1
  
  negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  posStyle <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  
  
  if(tp == "Continuous"|| tp == "Discrete"){
    #print(out$ll)
    #print(out$ul)
    openxlsx::conditionalFormatting(wb, sheet = fbsheet, cols=col_number, rows=2:nc, rule=sprintf(">%s", scale_value$ul), style = negStyle)#WRONG
    openxlsx::conditionalFormatting(wb, sheet = fbsheet, cols=col_number, rows=2:nc, rule=sprintf("<%s", scale_value$ll), style = negStyle)#WRONG
    openxlsx::conditionalFormatting(wb, sheet = fbsheet, cols = col_number, rows = 2:nc, rule = c(scale_value$ll,scale_value$ul), style = posStyle,type = "between" )
  } 
   
  if(tp =="Categorical"){
    
    #print(out$cat_scale)
    out_values <- col_trait[!is.element(el = col_trait,set = scale_value$cat_scale)]
    #print("ok")
    #print("out_values")
    for(i in out_values)
      if(!is.na(i))
      openxlsx::conditionalFormatting(wb, sheet = fbsheet, cols = col_number, rows = 2:nc, rule = sprintf("==%s", i),style = negStyle)     
  }
  
  if(tp=="none"){
    print("This trait is not in trait dictionary")
  }
  
  openxlsx::saveWorkbook(wb,file = file,overwrite = TRUE)
}

#' Create conditional trait format into Fieldbook Spreadseet (Excel)
#' @description This function highlight all the values which are out of range in a fieldbook spreadsheet 
#' @param file The name of the file which contains your data fieldbook data. It must be an .xlsx file
#' @param fbsheet The name of the fieldbook sheet into the excel file.
#' @param trait The abbreviation(s) of the trait(s) used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' (paint with colours) the trait column to identify out of range values.
#' @return An excel file with conditional format according trait conditions 
#' @export
#' 

col_validation_trait <- function(file,fbsheet,trait,trait_dict){
  
  lapply(trait,function(x) out <- col_validation(file, fbsheet, trait = x, trait_dict = trait_dict ))
  
}



# #'@description This function paint wrong cip numbers in fieldbook using openxlsx package and regular expressions.
# conditionalFormat_cipnumber <- function(fp,sheetName,cip_colname="INSTN"){
#   wb <- openxlsx::loadWorkbook(fp)
#   book <- readxl::read_excel(path = fp,sheet=sheetName)
#   book <- as.data.frame(book)
#   sheet <- sheetName
#   nc <- nrow(book)+1
#   col_number <- which(names(book)==cip_colname)
#   a <- sbformula::cip_number_check(book[,cip_colname])
#   cipwrong <- a$cipnumber_wrong
#   if(length(cipwrong)>0){
#     regla <-  unique(cipwrong)
#     print(regla)
#     for(i in regla){
#       openxlsx::conditionalFormatting(wb, sheet = sheet, cols = col_number, rows = 2:nc, type = "contains", rule = i )
#     }
#     openxlsx::saveWorkbook(wb,file = fp,overwrite = TRUE)
#   }
# }
