
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
#' @param font.size Character string that determines font size of cutoff labels
#' @param prefix A string to assign to the objects within the svg. Enables 
#'   unique idenfication by the javascript code
#' @param width Width in inches of plot
#' @param height Height in inches of plot
#' @param omit.js Logical. If true, omit inclusion of javascript source in output. Useful for documents with multple interactive plots
#' @param omit.d3 Logical, deprecated same as omit.js
#' @param ... Other arguments
#'   
#' @export
#' 
#' @return A character object containing the html necessary to plot the ROC curve in a
#'   web browser
#'   
export_interactive_roc <- function(ggroc_p, font.size = "12px", prefix = "a", 
                                   width = 6, height = 6, omit.js = FALSE, omit.d3 = omit.js, ...){
  
  
  tmpPlot <- tempfile(fileext= ".pdf")
  pdf(tmpPlot, width = width, height = height)
  
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  ptns <- grep("geom_roc.", objnames, value = TRUE, fixed = TRUE)
  rects <- grep("geom_rocci.", objnames, value = TRUE, fixed = TRUE)

  jsString <- paste0("<script type='text/javascript'> clickForCis('", prefix, rects, ".1') </script>")
    
  cssString <- '<style type = "text/css">
  
  .tess {
  fill: blue;
  stroke: blue;
  stroke-width: 0px;
  opacity: 0;
  }
  </style>'
  
  tmpFile <- tempfile()
  svgString <- gridSVG::grid.export(name = tmpFile, prefix = prefix)$svg
  
  dev.off()
  svgString <- paste(readLines(tmpFile, warn = FALSE), collapse = "\n")
  
  unlink(tmpPlot)
  
  if(omit.js){
    finstr <- c(cssString, svgString, jsString)
  } else {
    d3String <- getD3()
    finstr <- c(cssString, d3String, svgString, jsString)
  }
  
  paste(finstr, collapse = "\n\n")
  
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

