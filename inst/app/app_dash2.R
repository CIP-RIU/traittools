library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)

#returns = readRDS("ptfieldbook3.rds")
returns = readRDS("spfieldbook.rds")

ui  <-  dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard")),
      fileInput(inputId="hot_file", label="Choose Fieldbook" , multiple = FALSE, accept = NULL, width = NULL),
      actionButton("calculate", "Calculate Variables")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(box(rHandsontableOutput("hot_btable",width = "1200")),width=100 ,collapsible = TRUE)
      )
    )
  )
)

server <- function(input, output,session) {
  
  output$hot_btable = renderRHandsontable({
     
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
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      plot_size <- 0.03
      plant_den <- 0.1
      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- sbcalculate(fb = DF,plot.size = plot_size,plant.den = plant_den)
      
    }
    traits <- get_trait_fb(DF)
    col_render_trait(DF,trait = traits ,sweetpotato_yield)    
    
  })
  
}

shinyApp(ui, server)
