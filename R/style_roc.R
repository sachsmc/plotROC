#' Add guides and annotations to a ROC plot
#' 
#' Adds a diagonal guideline, minor grid lines, and optionally direct labels to ggplot objects containing a geom_roc layer. 
#' 
#' @param major.breaks vector of breakpoints for the major gridlines and axes
#' @param minor.breaks vector of breakpoints for the minor gridlines and axes
#' @param guide logical, if TRUE draws diagonal guideline
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme Theme function compatible with ggplot2
#' @export
#' @examples 
#' D.ex <- rbinom(50, 1, .5)
#' fakedata <- data.frame(M1 = rnorm(50, mean = D.ex), 
#'    D = D.ex)
#' ggplot(fakedata, aes(m = M1, d = D)) + geom_roc() + style_roc()
#' ggplot(fakedata, aes(m = M1, d = D)) + geom_roc() + style_roc(xlab = "1 - Specificity")
#' ggplot(fakedata, aes(m = M1, d = D)) + geom_roc() + style_roc(theme = theme_grey)


style_roc <- function(major.breaks = c(0, .1, .25, .5, .75, .9, 1), 
                      minor.breaks = c(seq(0, .1, by = .01), seq(.9, 1, by = .01)), 
                      guide = TRUE, xlab = "False positive fraction", 
                      ylab = "True positive fraction", theme = theme_bw){
  
  
    res <- list(scale_x_continuous(xlab, breaks = major.breaks, minor_breaks = minor.breaks),
         scale_y_continuous(ylab, breaks = major.breaks, minor_breaks = minor.breaks), 
         theme())
    
    if(guide){
      
      pcol <- theme()$panel.grid.major$colour
      if(is.null(pcol)) pcol <- "white"
      res <- append(res, geom_abline(slope = 1, intercept = 0, color = pcol))
      
    }
    
    res
    
  }
  
#' Add direct labels to a ROC plot
#' 
#' @param ggroc_p A ggplot object that contains a \link{geom_roc} layer
#' @param labels, vector of labels to add directly to the plot next to
#'   the curves. If multiple curves, must be in the same order as the grouping
#'   factor. If NULL, attempts to determine labels from the ggroc_p object
#' @param label.angle angle of adjustment for the direct labels
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels
#'   by. These can be scalars or vectors the same length as the number of labels
#' @param size Size of labels
#' @param ... Other arguments passed to \link[ggplot2]{annotate}
#' @export

direct_label <- function(ggroc_p, labels = NULL, label.angle = 45, nudge_x = 0, nudge_y = 0, size = 6, ...){
  
  pb <- ggplot_build(ggroc_p)
  pbdat <- pb$data[[1]]
  if(is.null(labels)){
    
    pb2 <- pb$plot
    if(pbdat$group[1] == -1L){
      
      labels <- as.character(pb2$mapping$m)
      
    } else {
    
    labels <- as.character(sort(unique(pb2$data[, as.character(pb2$mapping$colour)])))
    
    }
  }
  
  lframe <- NULL
  for(i in 1:length(unique(pbdat$group))){
    
    s1 <- pbdat[pbdat[, "group"] == unique(pbdat$group)[i], ]
    s2 <- s1[s1$y + s1$x < 1, c("x", "y", "colour")]
    lframe <- rbind(lframe, data.frame(s2[nrow(s2), ], 
                                       label = labels[i], 
                                       stringsAsFactors = FALSE))
    
  }
  
  ggroc_p + annotate("text", x = lframe$x + .025 + nudge_x, y = lframe$y - .025 + nudge_y, 
                     label = lframe$label, colour = lframe$colour,
                     angle = label.angle, size = size, ...)
  
}
