#' Conditional Trait Render for Fieldbook Web Table
#' @description This function highlight all the values which are out of range in a fieldbook WebTable. 
#' @param fieldbook The fieldbook data
#' @param trait The abbreviation(s) of the trait(s) used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' @param dsource source of the data \code{1}: Hidap,  \code{2}: FieldbookApp. 
#' @return An web table spreadsheet with conditional format according trait conditions 
#' @export
#' 

col_render_trait <- function(fieldbook,trait,trait_dict, dsource =1){
  
  #n <- length(trait)
  fieldbook <- as.data.frame(fieldbook)
  #fieldbook$PLOT <- as.integer(fieldbook$PLOT)
  #fieldbook$REP <- as.integer(fieldbook$REP)
  #fieldbook$INSTN <- as.factor(fieldbook$INSTN)
  if("PLOT" %in% names(fieldbook)) fieldbook$PLOT<- as.factor(fieldbook$PLOT)
  if("REP" %in% names(fieldbook)) fieldbook$REP<- as.factor(fieldbook$REP)
  if("INSTN" %in% names(fieldbook)) fieldbook$INSTN<- as.factor(fieldbook$INSTN)
  if("FACTOR" %in% names(fieldbook)) fieldbook$FACTOR<- as.factor(fieldbook$FACTOR)
  if("PHASE" %in% names(fieldbook)) fieldbook$PHASE<- as.factor(fieldbook$PHASE)
  if("IDENTIFIED_CRITERIA" %in% names(fieldbook)) fieldbook$IDENTIFIED_CRITERIA<- as.factor(fieldbook$IDENTIFIED_CRITERIA)
  if("STYPE" %in% names(fieldbook)) fieldbook$STYPE<- as.factor(fieldbook$STYPE)
  if("BLOCK_ROW" %in% names(fieldbook)) fieldbook$BLOCK_ROW<- as.factor(fieldbook$BLOCK_ROW)
  if("BLOCK_COL" %in% names(fieldbook)) fieldbook$BLOCK_COL<- as.factor(fieldbook$BLOCK_COL)
  if("SUBPLOT" %in% names(fieldbook)) fieldbook$SUBPLOT<- as.factor(fieldbook$SUBPLOT)
  if("SET" %in% names(fieldbook)) fieldbook$SET<- as.factor(fieldbook$SET)
  if("FEMALE" %in% names(fieldbook)) fieldbook$FEMALE<- as.factor(fieldbook$FEMALE)
  if("MALE" %in% names(fieldbook)) fieldbook$MALE <- as.factor(fieldbook$MALE)
  if("LINE" %in% names(fieldbook)) fieldbook$LINE <- as.factor(fieldbook$LINE)
  if("TESTER" %in% names(fieldbook)) fieldbook$TESTER <- as.factor(fieldbook$TESTER)
  if("ROW" %in% names(fieldbook)) fieldbook$ROW <- as.factor(fieldbook$ROW)
  if("COLUMN" %in% names(fieldbook)) fieldbook$COLUMN <- as.factor(fieldbook$COLUMN)
  
  #FieldbookApp
  #ToDo: add `abbr_user	plot_number	rep	accesion_name	timestamp	person	location	number` to avoidable variables.
  if("abbr_user" %in% names(fieldbook)) fieldbook$abbr_user <- as.factor(fieldbook$abbr_user)
  if("plot_number" %in% names(fieldbook)) fieldbook$plot_number <- as.factor(fieldbook$plot_number)
  if("plot_id" %in% names(fieldbook)) fieldbook$plot_id <- fieldbook$plot_id
  if("plot_name" %in% names(fieldbook)) fieldbook$plot_name <- fieldbook$plot_name
  if("rep" %in% names(fieldbook)) fieldbook$rep <- as.factor(fieldbook$rep)
  if("rep_number" %in% names(fieldbook)) fieldbook$rep_number <- as.factor(fieldbook$rep_number)
  if("accesion_name" %in% names(fieldbook)) fieldbook$accesion_name <- as.factor(fieldbook$accesion_name)
  if("timestamp" %in% names(fieldbook)) fieldbook$timestamp <- as.factor(fieldbook$timestamp)
  if("person" %in% names(fieldbook)) fieldbook$person <- as.factor(fieldbook$person)
  if("location" %in% names(fieldbook)) fieldbook$location <- as.factor(fieldbook$location)
  if("number" %in% names(fieldbook)) fieldbook$number <- as.factor(fieldbook$number)
  if("block_number" %in% names(fieldbook)) fieldbook$block_number <- as.factor(fieldbook$block_number)
  if("row_number" %in% names(fieldbook)) fieldbook$row_number <- as.factor(fieldbook$row_number)
  if("col_number" %in% names(fieldbook)) fieldbook$col_number <- as.factor(fieldbook$col_number)
  if("accession_name" %in% names(fieldbook)) fieldbook$accession_name <- as.factor(fieldbook$accession_name)
  if("is_a_control" %in% names(fieldbook)) fieldbook$is_a_control <- as.factor(fieldbook$is_a_control)
  if("synosyms" %in% names(fieldbook)) fieldbook$synosyms <- as.factor(fieldbook$synosyms)
  if("trial_name" %in% names(fieldbook)) fieldbook$trial_name <- as.factor(fieldbook$trial_name) 
  if("location_name" %in% names(fieldbook)) fieldbook$location_name <- as.factor(fieldbook$location_name) 
  if("year" %in% names(fieldbook)) fieldbook$year <- as.factor(fieldbook$year)
  if("pedigree" %in% names(fieldbook)) fieldbook$pedigree <- as.factor(fieldbook$pedigree)
  if("tier" %in% names(fieldbook)) fieldbook$tier <- as.factor(fieldbook$tier) 
  if("seedlot_name" %in% names(fieldbook)) fieldbook$seedlot_name <- as.factor(fieldbook$seedlot_name)
  if("seed_transaction_operator" %in% names(fieldbook)) fieldbook$seed_transaction_operator <- as.factor(fieldbook$seed_transaction_operator)
  if("num_seed_per_plot"  %in% names(fieldbook)) fieldbook$num_seed_per_plot <- as.factor(fieldbook$num_seed_per_plot)
  if("range_number"  %in% names(fieldbook)) fieldbook$range_number <- as.factor(fieldbook$range_number) 
  if("plot_geo_json" %in% names(fieldbook)) fieldbook$plot_geo_json <- as.factor(fieldbook$plot_geo_json)
 	
  
  dsource <- dsource
  
  
  #Validator validates all the trait which produce render_values different from ("")
  #validator <- lapply(trait,function(x) v <- render_trait(trait = x,trait_dict = trait_dict))
  validator <- lapply(trait,function(x) v <- render_trait_ext(data=fieldbook, trait = x,trait_dict = trait_dict, dsource= dsource))
  
  trait <- trait[validator!=""]
  n <- length(trait)

  out_temp <- list()
  renderer_trait <-  list()
  for(i in 1:n){
      out_temp[[1]]<- rhandsontable::rhandsontable(data = fieldbook, readOnly = FALSE, useTypes = TRUE) #%>%  
      #renderer_trait[[i]] <- render_trait(trait[i],trait_dict)
      renderer_trait[[i]] <- render_trait_ext(data=fieldbook, trait[i], trait_dict, dsource = dsource)
      #if(renderer_trait[[i]]==""){print("no render trait column")}
      #if(renderer_trait[[i]]!=""){
      j <- i+1
      out_temp[[j]] <- rhandsontable::hot_col(hot = out_temp[[i]], col = trait[i] ,readOnly = FALSE,
                               allowInvalid = TRUE,copyable = TRUE, renderer = renderer_trait[[i]]) 
      #}
  } 
  #k <- n+1
  #out_temp[[k]] 
  #if(export){
  k <- n+1
  out_temp[[k]] %>%
    rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    rhandsontable::hot_cols(fixedColumnsLeft = 3)  %>%
    rhandsontable::hot_rows(fixedRowsTop = 1)
  
  
#     hot_cols(colWidths = 100) %>%
#     hot_rows(rowHeights = 50)
  
#   out_temp[[k]] %>%
#     rhandsontable::hot_context_menu(
#       customOpts = list(
#         csv = list(name = "Download to CSV",
#                    callback = htmlwidgets::JS(
#                      "function (key, options) {
#                      var csv = csvString(this);
#                      var link = document.createElement('a');
#                      link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
#                      encodeURIComponent(csv));
#                      link.setAttribute('download', 'data.csv');
#                      document.body.appendChild(link);
#                      link.click();
#                      document.body.removeChild(link);
#        }"))))
#}
  
} 
