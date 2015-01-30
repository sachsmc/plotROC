
#' Plot an ROC curve for use in print
#' 
#' Given a ggroc object, creates a plot suitable for print.
#' 
#' @param ggroc_p An object as returned by \link{ggroc} or \link{multi_ggroc}.
#'   It can be modified with annotations, themes, etc.
#' @param font.size Integer that determines font size of cutoff labels
#' @param n.cuts Number of cutoffs to display 
#' @param ci.at Cutoff values at which to plot confidence regions, if non-NULL,
#'   \code{rocdata} must contain limits for the confidence region, as returned
#'   by \link{calculate_roc}
#' @param opacity Opacity level for confidence region boxes. Defaults to 0.3. Must be between 0 and 1
#' @param lty Optional vector of integers defining line types to apply to curves
#' @param color Optional vector of color names to apply to curves
#' @param lwd Line widths for curves
#' @param legend Logical. If true plots a legend in bottom right corner of plot
#' 
#'   
#'   
#' @export
#' 
#' @return A ggplot object
plot_journal_roc <- function(ggroc_p, font.size = 3, n.cuts = 20, ci.at = NULL, opacity = .3, 
                             lty = NULL, color = NULL, lwd = NULL, legend = FALSE){
  
  stopifnot(opacity <= 1 & opacity >= 0)
  stopifnot(n.cuts >= 0)
  
  rocdata <- ggroc_p$rocdata
  
  if(is.data.frame(rocdata)){  ## single curve
       
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

  p1 <- ggroc_p + ggplot2::geom_point(data = subdat, ggplot2::aes_string(x = "FPF", y = "TPF")) + 
    ggplot2::geom_text(data = strdat, ggplot2::aes_string(x = "FPF", y = "TPF", label = "c"), hjust = 1, vjust = 0, size = font.size) 
  
  if(any(!is.null(lty), !is.null(color), !is.null(lwd))){
    
    args <- list(linetype = lty, color = color, size = lwd)
    args[sapply(args, is.null)] <- NULL
    
    p1 + aes(linetype = "A") + scale_linetype_manual(values = 2)
    p1 + do.call(ggplot2::geom_path, args)
    
  } 

  if(!is.null(ci.at)){
    rocdata$op <- 0
    rocdata$op[ci_dex] <- opacity
    
    pout <- p1 + ggplot2::geom_rect(data = rocdata, ggplot2::aes_string(xmin = "FP.L", xmax = "FP.U", ymin = "TP.L", ymax = "TP.U"), alpha = rocdata$op)
    
  } else pout <- p1
  
  
  } else if(is.list(rocdata)){  ## multiple curves
    
    
    pout <- ggroc_p
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
        strdat$c <- paste(round(strdat$c, 1))
        
        if(dim(strdat)[1] > 0) {
          strdat$FPF[1] <- strdat$FPF[1] + .03 * (i - 1)
          strdat$TPF[nrow(strdat)] <- strdat$TPF[nrow(strdat)] + .03 * (i - 1)
        }
        
        pout <- pout + 
          ggplot2::geom_point(data = subdat, ggplot2::aes_string(x = "FPF", y = "TPF", 
                                                                 linetype = NULL, color = NULL, size = NULL), show_guide = FALSE) + 
          ggplot2::geom_text(data = strdat, ggplot2::aes_string(x = "FPF", y = "TPF", label = "c", 
                                                                linetype = NULL, color = NULL, size = NULL), 
                             hjust = 1, vjust = 0, size = font.size, show_guide = FALSE)
        
      }
  
  if(!is.null(lty)){
    
    pout <- pout + ggplot2::scale_linetype_manual(values = lty)
    
  }
  if(!is.null(color)){
    
    pout <- pout + ggplot2::scale_color_manual(values = color)
    
  }
  if(!is.null(lwd)){
    
   pout <- pout + ggplot2::scale_size_manual(values = lwd)
    
  }
  
  }
  
  pout <- pout  + ggplot2::theme_bw() + ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, lwd = .5, color = "grey90")
  
  if(legend){
    pout <- pout + ggplot2::theme(legend.justification=c(1,0), legend.position=c(1,0),# anchor bottom-right/bottom-right
                                  legend.title = ggplot2::element_blank()) 
  } else {
    pout <- pout + ggplot2::theme(legend.position = "none")
  }
  
  pout
  
}



