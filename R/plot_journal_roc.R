
#' Plot an ROC curve for use in print
#' 
#' Given a ggroc object, creates a plot suitable for print, with sensible defaults
#' 
#' @param ggroc_p An object as returned by \link{ggroc}. It can be modified with
#'   annotations, themes, etc.
#' @param rocdata An object as returned by \link{ggroc}. It can be modified with annotations, themes, etc. 
#' @param font.size Integer that determines font size of cutoff labels
#' @param n.cuts Number of cutoffs to display
#' 
#' 
#' @export
#' 
#' @return A ggplot object
plot_journal_roc <- function(ggroc_p, rocdata, font.size = 3, n.cuts = 20){
  
  if(is.data.frame(rocdata)){
  
  if(nrow(rocdata) < n.cuts){ 
    dex <- 1:nrow(rocdata)
  } else {
    
    dex <- seq(1, nrow(rocdata), length.out = n.cuts)
    
  }
  
  subdat <- rocdata[dex, ]
  strdat <- subdat
  strdat$FPF <- strdat$FPF - .01
  strdat$TPF <- strdat$TPF + .01
  strdat$c <- paste(round(strdat$c, 1))
  
  ggroc_p + ggplot2::theme_bw() + ggplot2::geom_point(data = subdat, ggplot2::aes_string(x = "FPF", y = "TPF")) + 
    ggplot2::geom_text(data = strdat, ggplot2::aes_string(x = "FPF", y = "TPF", label = "c"), hjust = 1, vjust = 0, size = font.size)
  
  } else if(is.list(rocdata)){
    
    for(i in 1:length(rocdata)){
      
        if(nrow(rocdata[[i]]) < n.cuts){ 
          
          dex <- 1:nrow(rocdata[[i]])
          
        } else {
          
          dex <- seq(1, nrow(rocdata[[i]]), length.out = n.cuts)
          
        }
        
        subdat <- rocdata[[i]][dex, ]
        strdat <- subdat
        strdat$FPF <- strdat$FPF - .01
        strdat$TPF <- strdat$TPF + .01
        
        strdat$FPF[1] <- strdat$FPF[1] + .03 * (i - 1)
        strdat$TPF[nrow(strdat)] <- strdat$TPF[nrow(strdat)] + .03 * (i - 1)
        strdat$c <- paste(round(strdat$c, 1))
        
        ggroc_p <- ggroc_p + ggplot2::theme_bw() + ggplot2::geom_point(data = subdat, ggplot2::aes_string(x = "FPF", y = "TPF")) + 
          ggplot2::geom_text(data = strdat, ggplot2::aes_string(x = "FPF", y = "TPF", label = "c"), hjust = 1, vjust = 0, size = font.size)
        
      }
  
  ggroc_p + ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, lwd = .5, color = "grey90")
  
  }
  
  
}



