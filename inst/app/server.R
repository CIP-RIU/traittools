library(shiny)
library(rhandsontable)
library(dygraphs)
library(data.table)
library(reshape2)
library(quantmod)
library(traittools)
library(traittools)
library(sbformula)
#tkrs = c("MSFT", "CAT", "AXP", "DIS", "MMM")

# quantmod::getSymbols(tkrs, from = "2012-06-01", auto.assign=TRUE)
# returns = Reduce(function(x, y) merge(x, y), lapply(tkrs, get))
# returns = returns[, names(returns)[grepl("Close", names(returns))]]
# returns = data.table(Date = time(returns), coredata(returns))
# returns = melt(returns, id.vars = "Date", variable.name = "Name",
#                value.name = "Price")[order(Name, Date)]
# returns[, `:=`(Name = gsub(".Close", "", Name))]
# returns[, `:=`(Return = c(NA, Price[-1] / head(Price, -1) - 1)), by = Name]
# saveRDS(returns, "returns.rds")
returns = readRDS("ptfieldbook2.rds")
returns <- data.table::data.table(returns)
setkey(returns, PLOT)

#port <- returns
# port = data.table(Name = tkrs,
#                   Position = ifelse(rnorm(length(tkrs)) > 0, "Long", "Short"),
#                   Weight = runif(length(tkrs)))
# port[, `:=`(Weight = Weight / sum(Weight)), by = Position]
# port[Position == "Long", `:=`(Weight = Weight * 1.3)]
# port[Position == "Short", `:=`(Weight = Weight * 0.3)]
# setkey(port, Name)

shinyServer(function(input, output, session) {
  values = shiny::reactiveValues(
    hot_btable = returns
  )
  
  calc = shiny::reactive({
    btable = values[["hot_btable"]]
  })
  
  output$hot_btable = renderRHandsontable({
  
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
      col_render_trait(DF,trait = "NTP",potato_yield)
    } else if (!is.null(values[["hot_btable"]])) {
      plot_size <- 0.003
      plant_den <- 10
          
      DF = values[["hot_btable"]]
      #DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      #col_render_trait(DF,trait = "NTP",potato_yield)
      col_render_trait(DF,trait = "NPH",potato_yield)
    }
    
     #if (!is.null(DF)){
     #if (is.null(input$calculateAction)) return(DF)
     #if (input$calculateAction ==0) return(DF)
     
#     
     
  #}
})  

  output$calculateAction<- renderUI({
    actionButton("calculateButton", "Process your Fieldbook")
  })
  
  output$exportAction<- renderUI({
    actionButton("exportButton", "Download")
  })
  
  shiny::observeEvent(input$exportButton, function(){
    
    isolate({ 
      
#     if (!is.null(input$hot_btable)) {
#         DF = hot_to_r(input$hot_btable)
#         values[["hot_btable"]] = DF
#         rhandsontable(DF)
#      } 
#         else if (!is.null(values[["hot_btable"]])) {
     if (!is.null(values[["hot_btable"]])) {
         DF = values[["hot_btable"]]
         rhandsontable(DF)
     }
       
     openxlsx::write.xlsx(DF, "test_export.xlsx", overwrite=TRUE)
     #shell.exec("test_export.xlsx")
    })
    
  })  

})
