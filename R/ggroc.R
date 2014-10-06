#' Plot an ROC curve
#' 
#' Given a result computed by \link{calculate_roc}, plot the curve using ggplot and sensible defaults
#' 
#' @param rocdata Data frame as returned by calculate_roc
#' 
#' @export
#' 
#' @return A ggplot object
#' 

ggroc <- function(rocdata){
  
  stopifnot("rocdata" %in% class(rocdata))
  
  ggplot2::ggplot(rocdata, ggplot2::aes_string(x = "FPF", y = "TPF")) + ggplot2::geom_path() + ggplot2::geom_point(color = "red", alpha = 0) +
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 2) + 
    ggplot2::scale_x_continuous("False positive fraction") + ggplot2::scale_y_continuous("True positive fraction") 
    
  
}


#' Generate svg code for an ROC curve object. Includes necessary styling and Javascript code
#' 
#' @param ggroc_p An object as returned by \link{ggroc}. It can be modified with annotations, themes, etc. 
#' @param cutoffs Vector of cutoff values
#' 
#' @export
#' 
#' @return A string containing the svg code necessary to plot the ROC curve in a browser
#' 
svgize_roc <- function(ggroc_p, cutoffs, font.size = "12px", prefix = "a"){
  
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  ptns <- grep("geom_point.points", objnames, value = TRUE)[1]
  
  gridSVG::grid.garnish(path = ptns, cutoff = paste(cutoffs), group = FALSE, global = TRUE)
  
  cssString <- modCss(font.size)
  jsString <- modJs(ptns, prefix = prefix)
  
  tmpFile <- tempfile()
  svgString <- gridSVG::grid.export(name = tmpFile, prefix = prefix)$svg
  
  svgString <- paste(readLines(tmpFile, warn = FALSE), collapse = "\n")
  
  d3String <- getD3()
  
  paste(c(cssString, d3String, svgString, jsString), collapse = "\n\n")
  
}

#' Generate a standalone html document displaying an interactive ROC curve
#' 
#' @param rocdata An object as returned by \link{ggroc}. It can be modified with annotations, themes, etc. 
#' 
#' @export
#' 
#' @return NULL opens an interactive document in the browswer
#'

standalone_svg_roc <- function(rocdata, file = NULL, font.size = "12px"){
  
  p1 <- ggroc(rocdata) 
  if(is.null(file)){
    
    tmpDir <- tempdir()
    tmpPlot <- tempfile(tmpdir = tmpDir, fileext = ".html")
    
  } else {
    tmpPlot <- ifelse(length(grep(".html", file)) > 0, file, paste(file, "html", sep = "."))
    tmpDir <- "."
  }
  
  print(p1)
  body <- svgize_roc(p1, cutoffs = rocdata$c, font.size = font.size)
  
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

