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