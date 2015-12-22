
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
#' @param ggroc_p A ggplot object with a GeomRoc layer and optionally a
#'   GeomRocci layer as returned by \link{geom_roc} and/or \link{geom_rocci}. It can
#'   be modified with annotations, themes, etc.
#' @param add.cis Logical, if true, removes the current confidence interval 
#'   layer (if present) and replaces it with a denser layer of confidence 
#'   regions
#' @param hide.points Logical, if true, hides points layer so that points with cutoff values are
#'  only visible when hovering. Recommended for plots containing more than 3 curves.
#' @param prefix A string to assign to the objects within the svg. Enables 
#'   unique idenfication by the javascript code
#' @param width,height Width and height in inches of plot
#' @param omit.js Logical. If true, omit inclusion of javascript source in 
#'   output. Useful for documents with multple interactive plots
#' @param style A call to the function \link{style_roc}
#' @param ... Other arguments passed to \link{geom_rocci} when \code{add.cis = 
#'   TRUE}
#'   
#' @export
#' 
#' @return A character object containing the html necessary to plot the ROC 
#'   curve in a web browser
#'   
export_interactive_roc <- function(ggroc_p, add.cis = TRUE, hide.points = FALSE, 
                                   prefix = "a", 
                                   width = 6, height = 6, omit.js = FALSE, 
                                   style = style_roc(theme = theme_grey()), ...){
  

  
  lays <- sapply(ggroc_p$layers, function(l){
    
    class(l$geom)[1]
    
  })  
  
  stopifnot("GeomRoc" %in% lays | "GeomRocci" %in% lays)
  
  if(add.cis){

    if("GeomRocci" %in% lays){
      ggroc_p$layers[[which(lays == "GeomRocci")]] <- NULL
    }
    
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
  grDevices::pdf(tmpPlot, width = width, height = height)
  
  if(!is.null(style)){
    ggroc_p <- ggroc_p + style
  }
  
  print(ggroc_p)
  grid::grid.force()
  
  objnames <- grid::grid.ls(print = FALSE)$name
  
  ## if confidence intervals are present
  
  ptns <- grep("geom_roc.", objnames, value = TRUE, fixed = TRUE)
  
  jsString <- NULL
  if(add.cis || "GeomRocci" %in% lays){
    rects <- grep("geom_rocci.", objnames, value = TRUE, fixed = TRUE)
    jsString <- c(jsString, paste0("<script type='text/javascript'> clickForCis('", prefix, rects, ".1') </script>"))
  } 
  if(hide.points) {
    jsString <- c(paste0("<script type='text/javascript'> hoverForPoints('", prefix, ptns, ".1') </script>"), jsString)
  }
    
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
  
  grDevices::dev.off()
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

