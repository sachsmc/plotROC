#' Reads included JavaScript functions and returns them as a string for pasting into a webpage
#' 
#' @keywords Internal
#' 
getD3 <- function(){
  
  d3.js <- readLines(system.file("d3.v3.min.js", package = "plotROC"), warn = FALSE)
  ci.js <- readLines(system.file("clickCis.js", package = "plotROC"), warn = FALSE)
  pt.js <- readLines(system.file("hoverPoints.js", package = "plotROC"), warn = FALSE)
  paste(
    paste("<script type=\"text/javascript\" charset=\"utf-8\">", paste(d3.js, collapse = "\n"), "</script>"),
    paste("<script type=\"text/javascript\" charset=\"utf-8\">", paste(ci.js, collapse = "\n"), "</script>"), 
    paste("<script type=\"text/javascript\" charset=\"utf-8\">", paste(pt.js, collapse = "\n"), "</script>"), 
    sep = "\n")
  
  
}
