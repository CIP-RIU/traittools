#' Run a data proccessing framework using the traittools functionality
#' 
#' When the function is invoked a shiny app is started showing a very simple 
#' setup using traittools. A button summons the dialog box allowing the user to
#' navigate the R installation directory. First, you select your excel file, then 
#' traittools will show a spreadsheet identifying outlier and values out of range. Then press
#' on calculate for checking and applying formulas over the traits. Finally, you can download
#' your proccessing data using "Download" button with conditional formatting.
#' 
#' @family traitttols
#' 
#' @export
#' 
traittoolsApp <- function() {
  shiny::runApp(system.file('example', package='traittools', mustWork=T), display.mode='normal')
}