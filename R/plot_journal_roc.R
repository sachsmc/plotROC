
#' Plot an ROC curve for use in print
#' 
#' Given a ggroc object, creates a plot suitable for print, with sensible
#' defaults
#' 
#' @param ggroc_p An object as returned by \link{ggroc} or \link{multi_ggroc}.
#'   It can be modified with annotations, themes, etc.
#' @param rocdata An object as returned by \link{calculate_roc}.
#' @param font.size Integer that determines font size of cutoff labels
#' @param n.cuts Number of cutoffs to display
#' @param ci.at Cutoff values at which to plot confidence regions, if non-NULL,
#'   \code{rocdata} must contain limits for the confidence region, as returned
#'   by \link{calculate_roc}
#' @param alpha Alpha level for confidence region boxes. Defaults to 0.3. Must be between 0 and 1
#'   
#'   
#' @export
#' 
#' @return A ggplot object
plot_journal_roc <- function(ggroc_p, rocdata, font.size = 3, n.cuts = 20, ci.at = NULL, alpha = .3){
  
  if(is.data.frame(rocdata)){
    
  if(nrow(rocdata) < n.cuts){ 
    dex <- 1:nrow(rocdata)
  } else {
    
    dex <- as.integer(seq(1, nrow(rocdata), length.out = n.cuts))
    
  }
  
  if(!is.null(ci.at)){
    
    ci_dex <- sapply(ci.at, function(s){  
      
      min(which(rocdata$c > s))
      
      })
    
    buff <- as.integer(nrow(rocdata)/50)
    test <- unique(c(sapply(ci_dex, function(i){ 
      it <- i + -buff:buff
      it[it > 0]
    })))
    
    cleandex <- sapply(dex, function(i){ 
      i %in% test
      }) 
    
    dex <- sort(unique(c(dex[!cleandex], ci_dex)))
    
  } 
  
  subdat <- rocdata[dex, ]
  strdat <- subdat
  strdat$FPF <- strdat$FPF - .01
  strdat$TPF <- strdat$TPF + .01
  strdat$c <- paste(round(strdat$c, 1))

  p1 <- ggroc_p + ggplot2::theme_bw() + ggplot2::geom_point(data = subdat, ggplot2::aes_string(x = "FPF", y = "TPF")) + 
    ggplot2::geom_text(data = strdat, ggplot2::aes_string(x = "FPF", y = "TPF", label = "c"), hjust = 1, vjust = 0, size = font.size) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, lwd = .5, color = "grey90")

  if(!is.null(ci.at)){
    rocdata$op <- 0
    rocdata$op[ci_dex] <- alpha
    
    p1 + ggplot2::geom_rect(data = rocdata, ggplot2::aes_string(xmin = "FP.L", xmax = "FP.U", ymin = "TP.L", ymax = "TP.U"), alpha = rocdata$op)
    
  } else p1
  
  
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



