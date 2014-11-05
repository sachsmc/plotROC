
#' Generate svg code for an ROC curve object. Includes necessary styling and
#' Javascript code
#' 
#' If you intend to include more than one of these objects in a single page, use
#' a different \code{prefix} string for each one.
#' 
#' @param ggroc_p An object as returned by \link{ggroc}. It can be modified with
#'   annotations, themes, etc.
#' @param cutoffs Vector of cutoff values
#' @param font.size Character string that determines font size of cutoff labels
#' @param prefix A string to assign to the objects within the svg. Enables
#'   unique idenfication by the javascript code
#'  @param width Width in inches of plot
#'  @param height Height in inches of plot
#'   
#' @export
#' 
#' @return A string containing the svg code necessary to plot the ROC curve in a
#'   browser
#'   
export_interactive_roc <- function(ggroc_p, cutoffs, font.size = "12px", prefix = "a", width = 6, height = 6){
  
  pdf(tempfile(fileext= ".pdf"), width = width, height = height)
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  ptns <- grep("geom_point.points", objnames, value = TRUE)
  rects <- grep("geom_rect.rect", objnames, value = TRUE)
  cis <- length(rects) > 0
  if(!cis) rects <- "qk2d4gb6q7ur"
  
  if(is.list(cutoffs)){
    
    for(i in 1:length(cutoffs)){
      
      gridSVG::grid.garnish(path = ptns[i], cutoff = paste(cutoffs[[i]]), group = FALSE, global = TRUE)
      
    }
    
    pointns <- paste0("[id^=\'", paste0(prefix, ptns), ".1.\']")
    
    jsString <- modJs(selector= paste(pointns, collapse = ","), prefix = prefix)
    
  } else {
  
  gridSVG::grid.garnish(path = ptns, cutoff = paste(cutoffs), group = FALSE, global = TRUE)
  jsString <- modJs(paste0("[id^=\'", prefix, ptns, ".1.\']"), prefix = prefix, rects)
  
  }
  
  cssString <- modCss(font.size)
  tmpFile <- tempfile()
  svgString <- gridSVG::grid.export(name = tmpFile, prefix = prefix)$svg
  
  dev.off()
  svgString <- paste(readLines(tmpFile, warn = FALSE), collapse = "\n")
  
  d3String <- getD3()
  
  paste(c(cssString, d3String, svgString, jsString), collapse = "\n\n")
  
}


#' Generate a standalone html document displaying an interactive ROC curve
#' 
#' @param rocdata An object as returned by \link{ggroc}. It can be modified with annotations, themes, etc. 
#' @param file A path to save the result to. If NULL, will save to a temporary directory
#' @param font.size Character string that determines font size of cutoff labels
#' 
#' @export
#' 
#' @return NULL opens an interactive document in the browswer
#'

plot_interactive_roc <- function(rocdata, file = NULL, font.size = "12px"){
  
  if(!is.data.frame(rocdata)){
    p1 <- multi_ggroc(rocdata)
  } else {
  
    ci <- "TP.L" %in% colnames(rocdata)
    p1 <- ggroc(rocdata, ci = ci) 
  
  }
  if(is.null(file)){
    
    tmpDir <- tempdir()
    tmpPlot <- tempfile(tmpdir = tmpDir, fileext = ".html")
    
  } else {
    tmpPlot <- ifelse(length(grep(".html", file)) > 0, file, paste(file, "html", sep = "."))
    tmpDir <- "."
  }
  
  print(p1)
  
  if(!is.data.frame(rocdata)) {
    body <- export_interactive_roc(p1, cutoffs = lapply(rocdata, function(d) d$c), font.size = font.size)
  } else {
    body <- export_interactive_roc(p1, cutoffs = rocdata$c, font.size = font.size)
  }
  
  cat("<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
\n", file = tmpPlot)
  cat(body, file = tmpPlot, append = TRUE)
  cat("\n</html>", file = tmpPlot, append = TRUE)
  
  ## copy d3 to directory
  
  file.copy(system.file("d3.v3.min.js", package = "plotROC"), tmpDir)
  
  ## open in browswer if possible
  
  viewer <- getOption("viewer")
  if (!is.null(viewer)){
    viewer(tmpPlot)
  } else {
    utils::browseURL(tmpPlot)
  }
  
}

