library(shiny)
library(rhandsontable)
library(dygraphs)
library(data.table)
library(reshape2)
library(quantmod)

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
returns = readRDS("ptfieldbook.rds")
returns <- data.table::data.table(returns)
setkey(returns, PLOT)

port <- returns
# port = data.table(Name = tkrs,
#                   Position = ifelse(rnorm(length(tkrs)) > 0, "Long", "Short"),
#                   Weight = runif(length(tkrs)))
# port[, `:=`(Weight = Weight / sum(Weight)), by = Position]
# port[Position == "Long", `:=`(Weight = Weight * 1.3)]
# port[Position == "Short", `:=`(Weight = Weight * 0.3)]
# setkey(port, Name)

shinyServer(function(input, output, session) {
  values = shiny::reactiveValues(hot = port)
  
  calc = shiny::reactive({
    port = values[["hot"]]
    port
    #perf = na.omit(returns[port])
    #perf = perf[, list(Return = ifelse(Position == "Long", 1, -1) *
                     #    Weight * Return), by = Date][order(Date)]
    #perf[, `:=`(CumulRet = cumprod(1 + Return) - 1)]
    #fieldbook <- perf
    #list(fb <- fieldbook)
    #list(Perf = perf)
  })
  
  output$hot = renderRHandsontable({
    DT = NULL
    trait <-  c("NTP","Plant_Vigor","SE")
    if (!is.null(input$hot)) {
      DT = setDT(hot_to_r(input$hot))
      #DT <- as.data.frame())
      if (input$reweight) {
      fb <- as.data.frame(calc())
      #print("omar1")
      }
    values[["hot"]] = DT
    } else if (!is.null(values[["hot"]])) {
      DT = values[["hot"]]
     #print("omar2")
    }  
    if (!is.null(DT))
    col_render_trait(fieldbook = DT,trait = trait,trait_dict = potato_yield)
    #print("omar3")
  })
   
    
#     if (!is.null(input$hot)) {
#       DT = setDT(hot_to_r(input$hot))
#       if (input$reweight) {
#         DT[Position == "Long", `:=`(Weight = Weight / sum(Weight) * 1.3)]
#         DT[Position == "Short", `:=`(Weight = Weight / sum(Weight) * 0.3)]
#       }
#       values[["hot"]] = DT
#     } else if (!is.null(values[["hot"]])) {
#       DT = values[["hot"]]
#     }
    #col_render_trait(fieldbook = port,trait = trait,trait_dict = potato_yield)
    
#     if (!is.null(DT))
#       rhandsontable(DT) %>%
#       hot_col(col = "Position", type = "dropdown",
#               source = c("Long", "Short")) %>%
#       hot_col(col = "Name", readOnly = TRUE) %>%
#       hot_cols(columnSorting = list(column = which(names(DT) == "Name"),
#                                     sortOrder = TRUE))
  
  
#   output$plot = renderDygraph({
#     if (!is.null(calc())) {
#       dt_plot = xts(calc()$Perf$CumulRet, calc()$Perf$Date)
#       dygraph(dt_plot, main = "Cumulative Return") %>%
#         dySeries("V1", label = "Portfolio") %>%
#         dyLimit(as.numeric(calc()$Perf$CumulRet[1]), color = "red")
#     }
#   })
})
