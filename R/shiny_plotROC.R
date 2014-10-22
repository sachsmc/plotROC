#' Start the plotROC Shiny app
#' 
#' A convenience function to easily start the shiny application. It will open in Rstudio, or in the default web browser.
#' 
#' @export
#' 
shiny_plotROC <- function(){
  
  shiny::runApp(system.file("shinyApp", package = "plotROC"))
  
}
