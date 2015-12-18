library(shiny)
library(rhandsontable)
library(dygraphs)
library(data.table)
library(reshape2)
library(quantmod)
library(traittools)

library(sbformula)

# saveRDS(returns, "returns.rds")
returns = readRDS("ptfieldbook.rds")
#returns = readRDS("ptfieldbook2.rds")
#returns = readRDS("ptfieldbook3.rds")
returns <- data.table::data.table(returns)
setkey(returns, PLOT)


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
      #col_render_trait(DF,trait = "NTP",potato_yield)
    } else if (!is.null(values[["hot_btable"]])) {
      plot_size <- 0.003
      plant_den <- 10
          
      DF = values[["hot_btable"]]
      #DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      #col_render_trait(DF,trait = "NTP",potato_yield)
      #col_render_trait(DF,trait = "NPH",potato_yield)
    }
    
    if(input$calculate>0){
      plot_size <- 0.003
      plant_den <- 10
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)

      col_render_trait(DF,trait = "MTWP",potato_yield)
      
      
    }
    
    col_render_trait(DF,trait = "MTWP",potato_yield)
    
  #}

      #col_render_trait(DF,trait = "TTWP",potato_yield)
   
    traits <- get_trait_fb(DF)
    col_render_trait(DF,trait = traits ,potato_yield)


})  

  
#   shiny::observeEvent(input$calculate, {
#     isolate({
#   if (!is.null(values[["hot_btable"]])) {
#       plot_size <- 2
#       plant_den <- 2
#       DF = values[["hot_btable"]]
#       DF <- as.data.frame(DF)
#       DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
# 
#       col_render_trait(DF,trait = "MTWP",potato_yield)
# 
#       traits <- get_trait_fb(DF)
#       col_render_trait(DF,trait = traits,potato_yield)
# }
#       })
#   })
#   
  
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
     shell.exec("test_export.xlsx")
    })
    
  })  

})
