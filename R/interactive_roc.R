
#' Generate svg code for an ROC curve object
#' 
#' Takes a ggplot object that contains a GeomRoc layer and returns a string that
#' contains html suitable for creating a standalone interactive ROC curve plot.
#' 
#' @details If you intend to include more than one of these objects in a single 
#'   page, use a different \code{prefix} string for each one. To use this 
#'   function in knitr, use the chunk options \code{fig.keep='none'} and 
#'   \code{results = 'asis'}, then \code{cat()} the resulting string to the 
#'   output. See the vignette for examples. Older browsers (< IE7) are not
#'   supported.
#'   
#' @param ggroc_p A ggplot object with a GeomRoc layer as returned by
#'   \link{ggroc} or \link{multi_ggroc}. It can be modified with annotations,
#'   themes, etc.
#' @param add.cis Logical, if true, removes the current confidence interval
#'   layer (if present) and replaces it with a denser layer of confidence
#'   regions
#'   @param guide Logical, if true, add diagonal guideline matching current theme
#'   @param scales Logical, if true, replaces major and minor gridlines with defaults
#'   @param xlab Label for x-axis
#'   @param ylab Label for y-axis
#' @param font.size Character string that determines font size of cutoff labels
#' @param prefix A string to assign to the objects within the svg. Enables 
#'   unique idenfication by the javascript code
#' @param width Width in inches of plot
#' @param height Height in inches of plot
#' @param omit.js Logical. If true, omit inclusion of javascript source in
#'   output. Useful for documents with multple interactive plots
#' @param ... Other arguments passed to \link{geom_rocci} when \code{add.cis =
#'   TRUE}
#'   
#' @export
#' 
#' @return A character object containing the html necessary to plot the ROC
#'   curve in a web browser
#'   
export_interactive_roc <- function(ggroc_p, add.cis = TRUE, guide = TRUE, scales = TRUE, 
                                   xlab = "False positive fraction", 
                                   ylab = "True positive fraction", 
                                   font.size = "12px", prefix = "a", 
                                   width = 6, height = 6, omit.js = FALSE,  ...){
  

  
  lays <- sapply(ggroc_p$layers, function(l){
    
    class(l$geom)[1]
    
  })  
  
  stopifnot("GeomRoc" %in% lays | "GeomRocci" %in% lays)
  
  if(add.cis){

    
    ggroc_p$layers[[which(lays == "GeomRocci")]] <- NULL
    
    args <- list(...)
    if("ci.at" %in% names(args)){
      
      ggroc_p <- ggroc_p + geom_rocci(...)
      
    } else {
      
      Mran <- ggroc_p$data[, paste(ggroc_p$mapping$m)]
      if(length(Mran) > 100){
        
        thisciat <- sort(unique(sample(Mran, 100)))
        
      } else thisciat <- sort(unique(Mran))
      
      ggroc_p <- ggroc_p + geom_rocci(ci.at = thisciat, ...)
      
    }
    
  }
  
  tmpPlot <- tempfile(fileext= ".pdf")
  pdf(tmpPlot, width = width, height = height)
  
  if(guide){
    pcol <- ifelse(length(ggroc_p$theme) == 0, "white", ggroc_p$theme$panel.grid.major$colour)
    ggroc_p <- ggroc_p + geom_abline(slope = 1, intercept = 0, color = pcol)
    
  }
  if(scales){
    min_br <-  c(seq(0, .1, by = .01), seq(.9, 1, by = .01))
    br <- c(0, .1, .25, .5, .75, .9, 1)
    
    ggroc_p <- ggroc_p + scale_x_continuous(breaks = br, minor_breaks = min_br) + 
      scale_y_continuous(breaks = br, minor_breaks = min_br)
    
  }
  
  ggroc_p <- ggroc_p + xlab(xlab) + ylab(ylab)
  
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  
  ## if confidence intervals are present
  
  ptns <- grep("geom_roc.", objnames, value = TRUE, fixed = TRUE)
  rects <- grep("geom_rocci.", objnames, value = TRUE, fixed = TRUE)

  jsString <- paste0("<script type='text/javascript'> clickForCis('", prefix, rects[length(rects)], ".1') </script>")
    
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
    tmpDir <- dirname(file)
  }
  

  body <- export_interactive_roc(ggroc, ...)
  
  cat("<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
\n", file = tmpPlot)
  cat(body, file = tmpPlot, append = TRUE)
  cat("\n</html>", file = tmpPlot, append = TRUE)
  
  ## open in browswer if possible
  
  viewer <- getOption("viewer")
  if (!is.null(viewer)){
    viewer(tmpPlot)
  } else {
    utils::browseURL(tmpPlot)
  }
  
}

