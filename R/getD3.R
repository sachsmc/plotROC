#' @keywords Internal
#' 
getD3 <- function(){
  
  d3.js <- readLines(system.file("d3.v3.min.js", package = "plotROC"), warn = FALSE)
  paste("<script type=\"text/javascript\" charset=\"utf-8\">", paste(d3.js, collapse = "\n"), "</script>")
  
  
}
