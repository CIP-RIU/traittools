library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)

returns = readRDS("ptfieldbook3.rds")

ui = dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard")),
      actionButton("calculate", "Calculate Variables")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
#               fluidRow(box(rHandsontableOutput("hot", height = 400)),
#                        box(rHandsontableOutput("hot2", width = 200))),
#               fluidRow(box(rHandsontableOutput("hot3"))),
              fluidRow(box(rHandsontableOutput("hot_btable",width = "1200")),width=100 ,collapsible = TRUE)
      )
    )
  )
)

server = function(input, output,session) {
#   output$hot = renderRHandsontable({
#     rhandsontable(do.call(cbind, lapply(1:20, function(i) data.table(rnorm(10000)))))
#   })
#   
#   output$hot2 = renderRHandsontable({
#     rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))))
#   })
#   
#   output$hot3 = renderRHandsontable({
#     rhandsontable(do.call(cbind, lapply(1:3, function(i) data.table(rnorm(5)))),
#                   stretchH = "all")
#   })
  
  output$hot_btable = renderRHandsontable({
#     DF <- returns
#     traits <- get_trait_fb(DF)
#     col_render_trait(DF,trait = traits ,potato_yield)
    
    values = shiny::reactiveValues(
      hot_btable = returns
    )
    
    calc = shiny::reactive({
      btable = values[["hot_btable"]]
    })
    
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
     } else if (!is.null(values[["hot_btable"]])) {
      #plot_size <- 0.003
      #plant_den <- 10
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      plot_size <- 0.003
      plant_den <- 10
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      
    }
    traits <- get_trait_fb(DF)
    col_render_trait(DF,trait = traits ,potato_yield)    
 
  })

}

shinyApp(ui, server)
