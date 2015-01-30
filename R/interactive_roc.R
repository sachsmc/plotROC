
#' Generate svg code for an ROC curve object
#' 
#' Takes a ggplot object as returned by \link{ggroc} or \link{multi_ggroc} and
#' returns a string that contains html suitable for creating a standalone
#' interactive ROC curve plot. 
#' 
#' @details If you intend to include more than one of these objects in a single
#'   page, use a different \code{prefix} string for each one. To use this
#'   function in knitr, use the chunk options \code{fig.keep='none'} and 
#'   \code{results = 'asis'}, then \code{cat()} the resulting string to the
#'   output. See the vignette for examples. Older browsers (< IE7) are not supported. 
#'   
#' @param ggroc_p An object as returned by \link{ggroc} or \link{multi_ggroc}.
#'   It can be modified with annotations, themes, etc.
#' @param cutoffs Optional vector or list of vectors to over-ride the default cutoff labels. Useful for rescaling or rounding. 
#' @param font.size Character string that determines font size of cutoff labels
#' @param prefix A string to assign to the objects within the svg. Enables 
#'   unique idenfication by the javascript code
#' @param width Width in inches of plot
#' @param height Height in inches of plot
#' @param lty Optional vector of integers defining line types to apply to curves
#' @param color Optional vector of color names to apply to curves
#' @param lwd Line widths for curves
#' @param legend Logical. If true plots a legend in bottom right corner of plot

#'   
#' @export
#' 
#' @return A character object containing the html necessary to plot the ROC curve in a
#'   web browser
#'   
export_interactive_roc <- function(ggroc_p, cutoffs = NULL, font.size = "12px", prefix = "a", width = 6, height = 6, 
                                   lty = NULL, color = NULL, lwd = NULL, legend = FALSE){
  
  if(is.null(cutoffs) & ggroc_p$roctype == "single"){ cutoffs <- ggroc_p$rocdata$c 
  } else if(is.null(cutoffs) & ggroc_p$roctype == "multi") cutoffs <- lapply(ggroc_p$rocdata, function(df) df$c)
  
  if(any(!is.null(lty), !is.null(color), !is.null(lwd)) & ggroc_p$roctype == "single"){
    
    args <- list(linetype = lty, color = color, size = lwd)
    args[sapply(args, is.null)] <- NULL
    
    ggroc_p <- ggroc_p + do.call(ggplot2::geom_path, args)
    
  } else {
    
    if(!is.null(lty)){
      
      ggroc_p <- ggroc_p + ggplot2::scale_linetype_manual(values = lty)
      
    }
    if(!is.null(color)){
      
      ggroc_p <- ggroc_p + ggplot2::scale_color_manual(values = color)
      
    }
    if(!is.null(lwd)){
      
      ggroc_p <- ggroc_p + ggplot2::scale_size_manual(values = lwd)
      
    }
    
  }
  
  if(legend){
   ggroc_p <- ggroc_p + ggplot2::theme(#legend.justification=c(1,0), legend.position=c(1,0),# anchor bottom-right/bottom-right doesn't translate to svg
                                  legend.title = ggplot2::element_blank()) 
  } else {
   ggroc_p <- ggroc_p + ggplot2::theme(legend.position = "none")
  }
  
  tmpPlot <- tempfile(fileext= ".pdf")
  pdf(tmpPlot, width = width, height = height)
  
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  ptns <- grep("geom_point.points", objnames, value = TRUE)
  rects <- grep("geom_rect.rect", objnames, value = TRUE)
  cis <- length(rects) > 0
  if(!cis) rects <- "qk2d4gb6q7ur"
  

  gridSVG::grid.garnish(path = ptns, cutoff = paste(unlist(cutoffs)), group = FALSE, global = TRUE)
  jsString <- modJs(paste0("[id^=\'", prefix, ptns, ".1.\']"), prefix = prefix, rects)
    
  cssString <- modCss(font.size)
  tmpFile <- tempfile()
  svgString <- gridSVG::grid.export(name = tmpFile, prefix = prefix)$svg
  
  dev.off()
  svgString <- paste(readLines(tmpFile, warn = FALSE), collapse = "\n")
  
  d3String <- getD3()
  
  unlink(tmpPlot)
  
  paste(c(cssString, d3String, svgString, jsString), collapse = "\n\n")
  
}


#' Generate a standalone html document displaying an interactive ROC curve
#' 
#' @param ggroc An object as returned by \link{ggroc} or \link{multi_ggroc}. It can be modified with annotations, themes, etc. 
#' @param file A path to save the result to. If NULL, will save to a temporary directory
#' @param ... arguments passed to \link{export_interactive_roc}
#' 
#' @export
#' 
#' @return NULL opens an interactive document in Rstudio or the default web browser
#'

plot_interactive_roc <- function(ggroc, file = NULL, ...){
  
  if(is.null(file)){
    
    tmpDir <- tempdir()
    tmpPlot <- tempfile(tmpdir = tmpDir, fileext = ".html")
    
  } else {
    tmpPlot <- ifelse(length(grep(".html", file)) > 0, file, paste(file, "html", sep = "."))
    tmpDir <- "."
  }
  

  body <- export_interactive_roc(ggroc, ...)
  
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

