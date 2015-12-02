#####################################################################################################
#' Conditional Trait Render for Fieldbook Web Table
#' 
#' @description This function highlight all the values which are out of range in a fieldbook WebTable. 
#' @param fieldbook The fieldbook data
#' @param trait The abbreviation(s) of the trait(s) used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' @param export logical. If TRUE, using the right click, appear a context menu for downloading as \code{csv} file.
#' (paint with colours) the trait column to identify out of range values.
#' @return An web table spreadsheet with conditional format according trait conditions 
#' @export
#' 

col_render_trait <- function(fieldbook,trait,trait_dict,export=FALSE){
  
  n <- length(trait)
  fieldbook <- as.data.frame(fieldbook)
  out_temp <- list()
  renderer_trait <-  list()
  for(i in 1:n){
    out_temp[[1]]<- rhandsontable::rhandsontable(data = fieldbook, readOnly = FALSE, useTypes = TRUE) #%>%  
    renderer_trait[[i]] <- render_trait(trait[i],trait_dict)
    j <- i+1
    out_temp[[j]] <- rhandsontable::hot_col(hot = out_temp[[i]],col = trait[i] ,readOnly = FALSE,
                             allowInvalid = TRUE,copyable = TRUE, renderer = renderer_trait[[i]]) 
  } 
  k <- n+1
  out_temp[[k]]
  
  if(export==TRUE){
  out_temp[[k]] %>%
    rhandsontable::hot_context_menu(
      customOpts = list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     "function (key, options) {
                     var csv = csvString(this);
                     var link = document.createElement('a');
                     link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                     encodeURIComponent(csv));
                     link.setAttribute('download', 'data.csv');
                     document.body.appendChild(link);
                     link.click();
                     document.body.removeChild(link);
       }"))))
    }
} 
